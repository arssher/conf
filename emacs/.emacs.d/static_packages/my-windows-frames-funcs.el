;; functions for adding window on right or below
(defun my-split-root-window (size direction)
  (split-window (frame-root-window)
                (and size (prefix-numeric-value size))
                direction))
(defun my-split-root-window-below (&optional size)
  (interactive "P")
  (my-split-root-window size 'below))
(defun my-split-root-window-right (&optional size)
  (interactive "P")
  (my-split-root-window size 'right))

;; http://stackoverflow.com/questions/13316494/how-to-follow-the-end-of-messages-buffer-in-emacs
(defun tail-f-msgs ()
  "Go to the end of Messages buffer in all windows."
  (let ((msg-window (get-buffer-window "*Messages*")))
    (if msg-window
        (with-current-buffer (window-buffer msg-window)
          (set-window-point msg-window (point-max))))))
(defun tail-messages-enable()
  "Always tail *Messages* buffer"
  (interactive)
  (add-hook 'post-command-hook 'tail-f-msgs))
(defun tail-messages-disable()
  "Disable tailing *Messages* buffer"
  (interactive)
  (remove-hook 'post-command-hook 'tail-f-msgs))

(defun my-scroll-up-one ()
  "Scroll up 1 line and move cursor to the last visible line"
  (interactive)
  (scroll-up 1)
  (move-to-window-line -1)
  )

(defun my-scroll-down-one ()
  "Scroll up 1 line and move cursor to the last visible line"
  (interactive)
  (scroll-down 1)
  (move-to-window-line 0)
  )

(message "my-windows-frames-funcs loaded")
(provide 'my-windows-frames-funcs)
