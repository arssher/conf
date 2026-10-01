;; Stuff for remote editing

;; autoreload remote files
(setq auto-revert-remote-files t)

;; default method for remote editing
(setq tramp-default-method "ssh")

;; when connecting anywhere except locally as root, first ssh there under current user
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Multi_002dhops.html
(require 'tramp)
;; format of rules is 'when connecting to $1 as $2, use proxy $3'
;; Inside into emacs regexp: \ is special in elisp strings, so need to double
;; them to pass them to regexp. Then, \` and \' match beginning and end of string:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Backslash.html#Regexp-Backslash
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))
