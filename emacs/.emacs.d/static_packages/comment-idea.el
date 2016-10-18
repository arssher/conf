;; provides a comment command like jetbrains idea's
;; WARN: Currently it works only when transient-mark-mode is on

(defun comment-idea ()
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
(message "comment-idea executed")

(provide 'comment-idea)
