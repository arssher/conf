;;; -*- lexical-binding: t -*-
;; Markdown extras: wrapped tables in the read-only view, the way back out of
;; it, and imenu.

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
;; line right and nothing else -- it would leave the buffer holding whatever
;; the view added to the text, the final newline its pre-init hook inserts
;; when the file lacks one, and still read-only, buffer-read-only being a
;; permanent local that starting another major mode does not kill.
(defun ars-markdown-ts-edit ()
  "Leave `markdown-ts-view-mode' for `markdown-ts-mode' and allow editing."
  (interactive)
  (if (and buffer-file-name
           (file-exists-p buffer-file-name)
           ;; Edits that predate the view, which re-reading would throw away.
           (not (buffer-modified-p)))
      ;; Re-reading the file is the whole job, and undoes the whole visit:
      ;; the text is the file's again, normal-mode picks markdown-ts-mode back
      ;; out of auto-mode-alist, and after-find-file restores buffer-read-only
      ;; from the file's own permissions, so a write-protected file stays
      ;; protected rather than being quietly unlocked by a trip through the
      ;; view.
      (revert-buffer t t)
    ;; Nothing safe to re-read.  Switch by hand and keep the text as it is;
    ;; recompute read-only the way after-find-file would.
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
