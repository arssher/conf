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

(defun ars-frame-fullscreen ()
  "Make selected frame fullscreen"
  (interactive)
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (modify-frame-parameters
     nil `((fullscreen . fullboth) (fullscreen-restore . ,fullscreen)))))

(defun ars-frame-maximized ()
  "Make selected frame maximized.

If the frame is in fullscreen state, don't change its state, but
set the frame's `fullscreen-restore' parameter to `maximized', so
the frame will be maximized after disabling fullscreen state."
  (interactive)
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (cond
     ((memq fullscreen '(fullscreen fullboth))
      (set-frame-parameter nil 'fullscreen-restore 'maximized))
     (t
      (set-frame-parameter nil 'fullscreen 'maximized)))))

(defvar ars-current-theme "dark" "light or dark")

(defun ars-toggle-theme (&optional theme)
  "Switch between light and dark theme, or set given one"
  (interactive)
  (let
      ((new-theme (if theme theme
		    (cond ((string= ars-current-theme "dark") "light")
			  (t "dark"))
		    )))
    (cond ((string= new-theme "dark")
	   (load-theme 'wombat-modified t)
	   ;; font size, in px*10
	   (set-face-attribute 'default nil :height 140)
	   (setq ars-current-theme "dark")
	   )
	  ((string= new-theme "light")
	   (load-theme 'adwaita t
		       )
	   (set-face-attribute 'default nil :height 200)
	   (setq ars-current-theme "light")))
   )
)

(provide 'ars-visual)
