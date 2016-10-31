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

;; duplicate line, see
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; TODO: call to mind why I wasn't satisfied with out-of-the-box move-text package and decided to rewrite
;; these functions manually. Probably because of regions? 
;; move text arg lines down (or |arg| lines up, if arg < 0)
;; TODO: rewrite marked mode and handle start of buffer case
;;(defun move-text-internal (arg)
;;  (message "arg is %d" arg)
;;  (cond
;;    ((and mark-active transient-mark-mode)
;;     (if (> (point) (mark))
;;            (exchange-point-and-mark))
;;     (let ((column (current-column))
;;              (text (delete-and-extract-region (point) (mark))))
;;       (forward-line arg)
;;       (move-to-column column t)
;;       (set-mark (point))
;;       (insert text)
;;       (exchange-point-and-mark)
;;       (setq deactivate-mark nil)))
;;    (t
;;     ; TODO: save cursor
;;     (beginning-of-line)
;;     (forward-line)
;;     (transpose-lines arg)
;;     (forward-line -1)
;;     (when (< arg 0)
;;       (forward-line arg)
;;     )
;;    )
;;  )
;;)
;;(defun move-text-down (arg)
;;  "Move region (transient-mark-mode active) or current line
;;  arg lines down."
;;   ; 'interactive makes function a command;
;;   ; p converts argument to number, * ensures that buffer is writable (signals, if it is read-only)
;;  (interactive "*p")
;;  (move-text-internal arg))
;; 
;;(defun move-text-up (arg)
;;   "Move region (transient-mark-mode active) or current line arg lines up."
;;   (interactive "*p")
;;   (move-text-internal (- arg)))

(message "my-text-processing loaded")
(provide 'my-text-processing)
