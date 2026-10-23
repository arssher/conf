;;; -*- lexical-binding: t -*-
;; Markdown extras. Currently just table alignment for the read-only view.

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
Meant for `markdown-ts-view-mode-hook'.  Viewing then leaves no trace:
the text really is rewritten, but a buffer that was clean on arrival
stays clean, so reading a file does not offer to save it afterwards.  A
buffer that was already dirty keeps its flag -- clearing it there would
hide somebody's unsaved edits."
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

(message "ars-markdown loaded")
(provide 'ars-markdown)
