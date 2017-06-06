;; dired things
;; load dired-x library. I use it to quickly rename files
;; not sure it is a right way to load it...
(require 'dired-x)

;; setting hidden files in dired
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
