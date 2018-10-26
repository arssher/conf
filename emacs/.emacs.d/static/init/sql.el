;; SQL mode basic setup

;; currently not used, just for playing around
(require 'sql-indent)
;; (defvar my-sql-indentation-offsets-alist
;;  `((select-clause 0)
;;    (insert-clause 0)
;;    (delete-clause 0)
;;    (update-clause 0)
;;    ,@sqlind-default-indentation-offsets-alist))

(defun sql-style ()
  (setq indent-tabs-mode nil)

  (sqlind-minor-mode t)
  (setq sqlind-basic-offset 4))
  ;; (setq sqlind-indentation-offsets-alist
             ;; my-sql-indentation-offsets-alist))

;; currently it is just horrible, disable it
;; (add-hook 'sql-mode-hook 'sql-style)
