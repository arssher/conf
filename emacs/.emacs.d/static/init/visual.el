;; Visual things, colours, fonts, etc

;; don't show welcome screen
(setq inhibit-startup-screen t)

;; fullscreen and maximize on startup
(require 'ars-windows-frames-funcs)
(ars-frame-fullscreen)
(ars-frame-maximized)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'wombat-modified t) ; current theme
(set-face-attribute 'default t :font "Ubuntu Mono")
;; font size, in px*10
(set-face-attribute 'default nil :height 140)

;; set sane colors while editing text-based tables
(add-hook 'table-fixed-width-mode-hook
	  (lambda ()
	    (set-face-attribute 'table-cell nil :foreground "#303030" :background "white")
	    ))

;; Show long lines as wrapped
(global-visual-line-mode 1)

(global-linum-mode t) ; show line numbers
(setq column-number-mode t) ; show column number

(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; ruler at 80
(setq-default fill-column 80)
(require 'fill-column-indicator)
(setq fci-rule-color "#4d4d4d")
(define-globalized-minor-mode my-global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(my-global-fci-mode 1)

;; highlight matching braces
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'parenthesis) ; highlight only parenthesis

;; show trailing whitespaces
(setq show-trailing-whitespace 1)

;; never pop-up GUI file selector
(setq use-file-dialog nil)
