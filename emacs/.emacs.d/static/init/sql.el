;; SQL mode basic setup

(defun sql-style ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)

  (sqlind-minor-mode t)
  (setq sqlind-basic-offset 4))

(add-hook 'sql-mode-hook 'sql-style)
