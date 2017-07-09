;; Text processing

(transient-mark-mode 1) ; Highlight the region when the mark is active
;; (next-line) will insert a newline at the end of the buffer
;; (setq next-line-add-newlines t)
;; CUA things
(cua-mode t) ; It will make usual C-c C-v copypasting work.
(setq cua-keep-region-after-copy t) ; well, keep region after copy

;; (setq drag-stuff-modifier '(meta shift))
(drag-stuff-mode t) ; move lines and regions

(require 'ars-text-processing)
;; (load "ars-text-processing.el") ;; for debugging

;; delete tralining whitespace on save
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; default grep opts
(require 'grep)
(grep-apply-setting 'grep-command "grep --color -nH -ir -e")
