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

(defun just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; ignore multimaster and pg test expected files
(defun ars-delete-trailing-whitespace ()
  (if (and
       ;; (not (string-match-p "contrib/mmts" (buffer-file-name)))
       (not (string-match-p "\\.out\\'" (buffer-file-name)))
       (not (string-match-p "\\.diffs\\'" (buffer-file-name)))
       )
      (delete-trailing-whitespace)
  )
)

(defun toggle-remove-whitespace-on-save ()
  (interactive)
  (if (member 'ars-delete-trailing-whitespace write-file-functions)
    (progn (setq write-file-functions (remove 'ars-delete-trailing-whitespace write-file-functions)) (message "removing whitespaces on save disabled"))
    (add-to-list 'write-file-functions 'ars-delete-trailing-whitespace)
    (message "removing whitespaces on save enabled"))
)

(message "ars-text-processing loaded")
(provide 'ars-text-processing)
