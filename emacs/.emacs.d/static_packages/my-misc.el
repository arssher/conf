;; misc functions

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation, and then reread local-dir vars."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)
    (reload-dir-locals-for-all-buffer-in-this-directory))

;; taken from
;; http://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (reload-dir-locals-for-current-buffer)))))

(message "my-misc loaded")
(provide 'my-misc)
