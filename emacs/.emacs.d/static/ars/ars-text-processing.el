(defun comment-idea ()
"Provides a comment command like jetbrains idea's.
WARN: Currently it works only when transient-mark-mode is on."
  (interactive)
  (if (not transient-mark-mode)
    (message "Error: comment-idea is not supported with transient-mark mode off")
    (let ((start (line-beginning-position))
          (end (line-end-position))
        )
      ;; rely on region, if region is active
      (when (region-active-p)
        (setq start (save-excursion
                      (goto-char (region-beginning))
                      (beginning-of-line)
                      (point)
      		  )
              end (save-excursion
                    (goto-char (region-end))
                    (end-of-line)
                    (point)
      		)
        )
      )
      (comment-or-uncomment-region start end)
      (when (not (region-active-p))
	(next-line)
      )
    )
  )
)

(defun duplicate-line()
"Duplicate line, see
http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(defun select-line()
  "Dummy function to copy current line into the kill ring using cua"
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position))
)

(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
)

;; http://emacsredux.com/blog/2013/04/09/kill-whole-line/
(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(message "ars-text-processing loaded")
(provide 'ars-text-processing)
