;; Visual things, colours, fonts, etc

;; don't show welcome screen
(setq inhibit-startup-screen t)


;; Windows and frames

(require 'ars-visual)
;; (load "ars-windows-frames-funcs.el") ;; for debugging
;; (ars-frame-fullscreen)
;; (ars-frame-maximized)

;; maximize frame on start-up, but not make it fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; always tail *Messages* buffer
(tail-messages-enable)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-face-attribute 'default t :font "Ubuntu Mono")
;; set theme
(ars-toggle-theme "dark")

;; set sane colors while editing text-based tables
(add-hook 'table-fixed-width-mode-hook
	  (lambda ()
	    (set-face-attribute 'table-cell nil :foreground "#303030" :background "white")
	    ))

;; Show long lines as wrapped
(global-visual-line-mode 1)

(global-display-line-numbers-mode 1) ; show line numbers
(setq column-number-mode t) ; show column number

(if (display-graphic-p)
        (progn
	  (toggle-scroll-bar -1)
	  (tool-bar-mode -1)))
(menu-bar-mode -1)

;; ruler/formatting width.
;; This is what pg mail lists like, and seems also to what thunderbird truncates
;; lines in plain text mode (which he shouldn't do, really). For coding it
;; should be a bit larger though, so makes sense to set it on per mode basis like
;; https://stackoverflow.com/questions/8080495/how-do-i-set-the-emacs-fill-column-for-a-specific-mode
(setq-default fill-column 72)
(require 'fill-column-indicator)
(setq fci-rule-color "#4d4d4d")
(define-globalized-minor-mode my-global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(my-global-fci-mode 1)

;; highlight matching braces
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'parenthesis) ; highlight only parenthesis

;; never pop-up GUI file selector
(setq use-file-dialog nil)

;; I'm quite annoyed by hints popping up on mouse hover, in particular for
;; ggtags and ediff. It would be probably more accurate to disable them on
;; case-by-case basic, but let's try shooting from the hip
(tooltip-mode nil)
(setq show-help-function nil)
