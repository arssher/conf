;;; -*- lexical-binding: t -*-
;; Markdown extras: table alignment for the read-only view, and the way back
;; out of it.

;; markdown-ts-mode aligns a pipe table only on demand, with C-c C-c, and
;; markdown-ts-view-mode switches that off entirely: it sets
;; markdown-ts-enable-table-mode to nil, so markdown-ts-in-table-mode -- which
;; owns the align binding -- never turns on there.  The upshot is that the one
;; mode meant purely for reading is the one that shows tables ragged.
;;
;; Aligning is a buffer rewrite rather than a display effect, which is why it
;; is not simply free in a read-only buffer.  Doing it anyway is sanctioned
;; though: markdown-ts-view-mode runs markdown-ts-view-mode-pre-init-hook
;; before it sets buffer-read-only, documented as the place for functions that
;; "amend buffer content", and ships markdown-ts-add-final-newline on it by
;; default -- which inserts a newline into your buffer just for viewing.
;; Column widths in a pipe table are whitespace, so no rendered output changes.
;;
;; That hook is the precedent, not the place: it runs before the mode creates
;; its tree-sitter parser, so aligning from it dies on "No available parser
;; for this buffer: markdown" and takes the whole mode setup with it.  Hence
;; markdown-ts-view-mode-hook, which runs once everything is up.
(defun ars-markdown-ts-align-all-tables ()
  "Align every pipe table in the buffer, leaving the modified flag as it was.
Meant for `markdown-ts-view-mode-hook'.  The text really is rewritten;
`ars-markdown-ts-edit' puts it back by re-reading the file on the way
out, so the alignment lasts as long as the view does.

A buffer clean on arrival is left clean, so reading a file does not offer
to save it afterwards.  One already dirty keeps its flag, since clearing
it would hide somebody's unsaved edits -- and that same flag is what
tells `ars-markdown-ts-edit' whether re-reading is safe."
  (let ((inhibit-read-only t)
        (was-modified (buffer-modified-p))
        (starts (mapcar #'treesit-node-start
                        (treesit-query-capture
                         (treesit-buffer-root-node 'markdown)
                         '((pipe_table) @table) nil nil t))))
    (save-excursion
      ;; Back to front, so aligning one table cannot shift the positions of
      ;; the ones still to come.
      (dolist (pos (sort starts #'>))
        (goto-char pos)
        ;; markdown-ts-table-align-table refuses to run unless
        ;; markdown-ts-in-table-mode has put its overlay on the table, so turn
        ;; the minor mode on around each one.
        ;;
        ;; Guarded because ragged and empty-celled tables still parse with
        ;; errors (see the known issues at the top of markdown-ts-mode.el, and
        ;; tree-sitter-markdown issue 241), and markdown-ts-at-table-p signals
        ;; on those.  Unguarded, one such table anywhere in a file would abort
        ;; the hook and take the whole view with it.
        (condition-case err
            (progn
              (markdown-ts-in-table-mode 1)
              (markdown-ts-table-align-table))
          (error
           (message "ars-markdown: leaving table at %d unaligned: %s"
                    pos (error-message-string err))))
        (markdown-ts-in-table-mode -1)))
    (unless was-modified
      (set-buffer-modified-p nil))))

;; markdown-table-wrap-pretty-mode draws each table wrapped to the window, see
;; lang.el.  Turned on from an idle timer rather than from the view's hook
;; directly, because in the hook the width it wraps to comes out wrong:
;; window-max-chars-per-line subtracts the line-number column, and
;; line-number-display-width reads 0 until the window has been redisplayed in
;; the new major mode.  Measured in a 120-column frame: drawn 132 wide from
;; the hook, against 127 available once the numbers showed, and
;; visual-line-mode broke every row of it.  An idle timer runs after that
;; redisplay.  The package redraws on resize, which would correct it, but
;; nothing resizes the window when the view comes up.
;;
;; Deferring also puts it after the alignment above, which has to come first:
;; that rewrites the text, and an edit is the package's signal to take its
;; drawing off again.
(defun ars-markdown-ts-wrap-tables ()
  "Turn on `markdown-table-wrap-pretty-mode' once the window has been drawn.
Meant for `markdown-ts-view-mode-hook'."
  (let ((buffer (current-buffer)))
    (run-with-idle-timer
     0 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           ;; Unless the view was already left again.
           (when (eq major-mode 'markdown-ts-view-mode)
             (markdown-table-wrap-pretty-mode 1))))))))

;; markdown-ts-view-mode is a major mode with no way back: its only exit is the
;; q it inherits from special-mode-map, which buries the buffer rather than
;; returning you to editing.  Calling markdown-ts-mode by hand gets the mode
;; line right and nothing else -- it would leave the buffer holding the view's
;; aligned text, and still read-only, buffer-read-only being a permanent local
;; that starting another major mode does not kill.
(defun ars-markdown-ts-edit ()
  "Leave `markdown-ts-view-mode' for `markdown-ts-mode' and allow editing."
  (interactive)
  (if (and buffer-file-name
           (file-exists-p buffer-file-name)
           ;; Edits that predate the view, which re-reading would throw away.
           (not (buffer-modified-p)))
      ;; Re-reading the file is the whole job, and undoes the whole visit:
      ;; the alignment goes, normal-mode picks markdown-ts-mode back out of
      ;; auto-mode-alist, and after-find-file restores buffer-read-only from
      ;; the file's own permissions, so a write-protected file stays protected
      ;; rather than being quietly unlocked by a trip through the view.
      (revert-buffer t t)
    ;; Nothing safe to re-read.  Switch by hand and keep the text, including
    ;; the alignment; recompute read-only the way after-find-file would.
    (markdown-ts-mode)
    (read-only-mode (if (and buffer-file-name
                             (not (file-writable-p buffer-file-name)))
                        1 -1))))

;; Two imenu irritations, both imenu's own defaults rather than anything
;; markdown-ts does, and both settled here buffer-locally so that C, go and the
;; rest keep the behaviour they have.
;;
;; markdown-ts files its index under two categories, Headings and Code Blocks,
;; so imenu asks which category before it asks which heading. It goes on asking
;; in a file with no code blocks at all: an empty category is dropped, but a
;; lone surviving one is still nested, nesting being structural. Giving
;; headings the nil category hoists them to the top level, where a document's
;; table of contents belongs, and leaves Code Blocks a submenu underneath.
;;
;; The *Rescan* entry is the other half. imenu adds it precisely when
;; imenu-auto-rescan is nil -- the last form of imenu--make-index-alist is that
;; cons -- and picking it rebuilds the index and asks again, which reads as
;; doing nothing. Rebuilding every time is not worth avoiding on documents this
;; size, so take that and lose the entry.
(defun ars-markdown-ts-setup-imenu ()
  "Put headings at the top level of imenu, and drop its *Rescan* entry.
For both `markdown-ts-mode-hook' and `markdown-ts-view-mode-hook': the
view mode is a parent of the other only by `derived-mode-add-parents',
which `derived-mode-p' honours but `run-mode-hooks' does not, so neither
hook fires the other.  Running twice is harmless -- the hoist no longer
matches once it has been done."
  (setq-local imenu-auto-rescan t)
  ;; Matched by name, so an upstream rename costs us the hoist and nothing
  ;; else: the mapcar stops matching and imenu goes back to its nested menu.
  (setq-local treesit-simple-imenu-settings
              (mapcar (lambda (setting)
                        (if (equal (car setting) "Headings")
                            (cons nil (cdr setting))
                          setting))
                      treesit-simple-imenu-settings)))

(message "ars-markdown loaded")
(provide 'ars-markdown)
