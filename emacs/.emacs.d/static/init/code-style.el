;; Coding styles

;; Load Postgres code style
(require 'pg-coding-styles)

;; seems that this is a better way to change C settings, see
;; http://stackoverflow.com/questions/7404547/change-emacs-c-style-not-working
;; don't know why default-style didn't worked.
;; Aha, now I think I know why. Style modes are buffer-local, so it is better
;; to set them when the mode is actually loaded.
;; To temporary switch the style, run c-set-style (C-c .)
(defun my-c-mode-hook ()
  (c-set-style "postgresql"))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "gnu")))
