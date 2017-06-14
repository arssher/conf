;; Windows and frames

;; project tree
;; (require 'all-the-icons) ; load icons for 'icons regime
;; Every time when the neotree window is opened, let it find current file and
;; jump to node.
(setq neo-smart-open t)
(setq neo-theme (if window-system 'icons 'arrow))
;; neo tree ignore list
(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.so$"))

;; scroll 1 line while moving off the visible region
;; with it, all jumps end up at the last line of file, very inconvenient,
;; so I disable it until I deal with it.
;; (setq scroll-step            1
      ;; scroll-conservatively  10000)

(require 'ars-windows-frames-funcs)
;; (load "ars-windows-frames-funcs.el") ;; for debugging

;; always tail *Messages* buffer
(tail-messages-enable)
