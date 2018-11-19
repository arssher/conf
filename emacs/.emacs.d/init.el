;; Targeted at GNU Emacs, minimum version 24.5
;; See how to build it in Ubuntu here:
;; http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/


;; We do this ourselves later, but package.el insists we should have it.
;; (package-initialize)

(defvar emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "Turtle on which... I mean, directory with init.el")

;; Third-party code not found in repos
(add-to-list 'load-path (expand-file-name "static/lib" emacs-d))
;; Don't forget to add multifile packages manually
;; TODO: just add to load-path every dir under lib/

;; My code
(add-to-list 'load-path (expand-file-name "static/ars" emacs-d))

;; If anything goes wrong, show line numbers in edebug backtrace
(load "elisp-debug")

;; Store autogenerated conf in this file. In fact, I don't like the idea of
;; (auto) customize at all, so I will not even load this file; I couldn't
;; quickly find a way to disable it completely.
;; Perhaps later I should read how abo-abo sets variables:
;; https://oremacs.com/2015/01/17/setting-up-ediff/
;; For now I will just use old-school setq everywhere, however this might be
;; dangerous, see
;; http://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el
(setq custom-file "~/.custom.el")

;; Load custom functions for which I have not found their own file yet
(require 'ars-misc)

;;________________________________________________
;; Load init files
(defvar init-d (concat emacs-d "static/init/"))

;; general
(load-file (concat init-d "packages.el"))
(load-file (concat init-d "visual.el"))
(load-file (concat init-d "backup-restore.el"))
(load-file (concat init-d "text.el"))
(load-file (concat init-d "windows-frames.el"))
(load-file (concat init-d "dired.el"))
(load-file (concat init-d "remote.el"))
(load-file (concat init-d "diff.el"))
(load-file (concat init-d "gnus.el"))

;; ide-like features
(load-file (concat init-d "navigation-and-completion.el"))
(load-file (concat init-d "non-code-completion.el"))
(load-file (concat init-d "code-style.el"))
(load-file (concat init-d "fly.el"))
(load-file (concat init-d "dbg.el"))
(load-file (concat init-d "magit.el"))

;; major modes
(load-file (concat init-d "latex.el"))
(load-file (concat init-d "asm.el"))
(load-file (concat init-d "sql.el"))

;; mail; load only mu4e is installed
(add-to-list 'load-path (format "%s/opt/share/emacs/site-lisp/mu4e" (getenv "HOME")))
(when (require 'mu4e nil 'noerror) (load-file (concat init-d "mail.el")))

;; machine-specific stuff, e.g. set environment variables there
(when (file-exists-p (concat (getenv "HOME") "/.emacs-local.el"))
  (load-file (concat (getenv "HOME") "/.emacs-local.el")))


;;____________________________________________________________
;; Make the keys work with russian layout
(reverse-input-method "russian-computer")

;; Now, the keys.
;; About prefixes:
;; * Alt is the best
;; * Ctrl is the second priority (right Alt)
;; * Shift Alt is ok, if the key is for right hand, but pinky is getting nervious
;; * Ctrl Alt is ok, if the key is on the home row or nearby. Ergo doesn't use it
;; * Ctrl Shift doesn't work in terminals, avoid it for important keys!
;; * Ctrl Alt Shift -- I reserved for system apps

;; Try to write C-letter keys first, then M-letter, then M-big_letter,
;; then C-M-letter

;; Basic, global keys

;; The F keys row
(global-set-key [f1] help-map) ; help prefix
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(global-set-key (kbd "<f1> j") 'info-apropos) ; search for string in all manuals
(global-set-key [f2] 'dired-omit-mode) ; show and hide hidden files in dired
;; enable/disable ivy mode until I deal with excess completing problem
(global-set-key [f3] 'ivy-mode)
(global-set-key [f4] 'toggle-remove-whitespace-on-save)
(global-set-key [f5] 'revert-buffer-no-confirm)
(global-set-key [f6] 'desktop+-create) ; save desktop
(global-set-key [f7] 'desktop+-load) ; load saved desktop
(global-set-key [f8] 'neotree-toggle) ; show and hide neotree
(global-set-key [f9] 'whitespace-mode) ; toggle show whitespace
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key [f12] 'toggle-frame-maximized)
;; M-insert now reloads init.el. Lambda here just passes closured load-file as a function
(global-set-key [M-insert] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-M-<insert>") '(lambda() (interactive) (load-file "~/.gnus.el")))



;; The digits row
(global-set-key (kbd "\C-x4") 'my-split-root-window-below)
(global-set-key (kbd "\C-x5") 'my-split-root-window-right)
(global-set-key (kbd "\C-x9") 'delete-frame)
;; reset scaling
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;; The qwerty row:
(global-set-key (kbd "M-w") 'select-line) ; ergo
(global-set-key (kbd "C-w") 'kill-this-buffer) ; ergo
(global-set-key (kbd "C-M-w") 'just-one-space) ; ergo
(global-set-key (kbd "M-e") 'backward-kill-word) ; ergo
(global-set-key (kbd "M-r") 'kill-word) ; ergo
(global-set-key (kbd "C-M-r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-y") 'smart-kill-whole-line)
(global-set-key (kbd "M-u") 'backward-word) ; ergo
(global-set-key (kbd "C-x C-u") 'upcase-word)
;; dirty hack to distinguish TAB and C-i, see
;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
;; accepted answer doesn't work
(if (display-graphic-p)
    (progn
      (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
      (global-set-key (kbd "H-i") 'my-scroll-down-one)))
(global-set-key (kbd "M-i") 'previous-line) ; ergo
(global-set-key (kbd "M-I") 'drag-stuff-up)
(global-set-key (kbd "M-o") 'forward-word) ; ergo
(global-set-key (kbd "C-o") 'find-file) ; ergo
(global-set-key (kbd "C-S-o") 'smart-open-line-above)
(global-set-key (kbd "M-p") 'recenter-top-bottom) ; ergo

;; The home row:
(global-set-key (kbd "C-a") 'mark-whole-buffer) ; ergo
(global-set-key (kbd "M-a") 'execute-extended-command) ; ergo
(global-set-key (kbd "C-s") 'save-buffer) ; ergo
(global-set-key (kbd "M-s") 'ace-window) ; ergo, quick window switching
(global-set-key (kbd "M-d") 'backward-delete-char) ; ergo
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "M-f") 'delete-forward-char) ; ergo
(global-set-key (kbd "C-f") 'swiper) ; ergo
(global-set-key (kbd "M-g") 'kill-line) ; kill the rest of the line; ergo
(global-set-key (kbd "M-G") '(lambda () (interactive) (kill-line 0))) ; kill the line up to the cursor, ergo
(global-set-key (kbd "M-h") 'move-beginning-of-line) ; ergo
(global-set-key (kbd "M-H") 'move-end-of-line) ; ergo
(global-set-key (kbd "M-j") 'backward-char) ; ergo
(global-set-key (kbd "C-k") 'my-scroll-up-one)
(global-set-key (kbd "M-k") 'next-line) ; ergo
(global-set-key (kbd "M-K") 'drag-stuff-down) ; ergo
(global-set-key (kbd "M-l") 'forward-char) ; ergo
(global-set-key (kbd "C-l") 'goto-line) ; ergo
(global-set-key (kbd "C-;") 'comment-idea)
(global-set-key (kbd "M-;") 'comment-idea)
;; idea's shift-enter, defined in my-text-processing.el
(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)

;; The zxcv row:
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "M-c") 'forward-paragraph)
;; cua is shadowing keys, http://stackoverflow.com/questions/34057023/how-do-you-rebind-a-key-set-by-cua-mode-in-emacs
(define-key cua--cua-keys-keymap (kbd "M-v") 'backward-paragraph)
(global-set-key (kbd "M-b") 'ivy-switch-buffer)
(global-set-key (kbd "M-n") 'beginning-of-buffer)
(global-set-key (kbd "M-N") 'end-of-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

;; Space row
(global-set-key (kbd "M-<SPC>") 'cua-set-mark) ; ergo
;; M-S-arrows move lines and words, it is set before loading drag-stuff
;; enable switching windows with M-arrows
(windmove-default-keybindings 'meta)
;; enlarge and shring active window with C-arrows
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; C-c prefix
(global-set-key (kbd "C-c k") 'flycheck-next-error)
(global-set-key (kbd "C-c i") 'flycheck-previous-error)


;; Now mode-specific bindings

;; dired
(define-key dired-mode-map (kbd "M-s") nil)

;; ggtags: TODO enable them only in C mode
(require 'ggtags)
(global-set-key (kbd "C-b") 'ggtags-find-tag-dwim)
(global-set-key (kbd "C-S-b") 'ggtags-find-definition)
(global-set-key (kbd "M-B") 'ggtags-prev-mark)
(global-set-key (kbd "M-F") 'ggtags-next-mark)
(global-set-key (kbd "M-<f7>") 'ggtags-find-reference) ; idea
(global-set-key (kbd "C-M-g") 'ggtags-grep) ; grepping the project
(global-set-key (kbd "C-S-N") 'projectile-find-file) ; similar to idea
(global-set-key (kbd "C-n") 'ggtags-find-definition) ; similar to idea
(define-key ggtags-navigation-map (kbd "M-s") nil)
(define-key ggtags-navigation-map (kbd "M-k") 'next-error)
(define-key ggtags-navigation-map (kbd "M-i") 'previous-error)

;; ivy:
(require 'ivy)
;; Since clever ivy just remaps his previous/next to global
;; next-line/previous-line, in overall we don't have to remap it. However,
;; concrete M-i mapping is occupied by ivy itself, to it is necessary
(define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line)

;; projectile:
;; switching between header and .c
(global-set-key (kbd "C-M-n") 'projectile-find-other-file)
(global-set-key (kbd "M-R") 'projectile-replace-regexp)


;; grep mode. TODO: merge it with compilation mode somehow
(define-key grep-mode-map (kbd "i") 'compilation-previous-error)
(define-key grep-mode-map (kbd "k") 'compilation-next-error)
(define-key grep-mode-map (kbd "M-n") nil)
(define-key grep-mode-map (kbd "M-p") nil)


;; shell-like modes:
;; we don't need moving up and down in the shells, let them scroll history
;; commands instead:
(define-key comint-mode-map (kbd "M-i") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-k") 'comint-next-input)
;; we don't need replace either, let it do command search as always:
(define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)

;; c-mode:
(defun my-c-mode-config ()
  ;; Disable C-d default meaning in c-mode, we need it for duplicating lines
  (local-set-key (kbd "C-d") nil)
)
(add-hook 'c-mode-hook 'my-c-mode-config)


;; Diff mode
(define-key diff-mode-map (kbd "M-k") nil)
(define-key diff-mode-map (kbd "M-n") nil)
(define-key diff-mode-map (kbd "M-p") nil)
(define-key diff-mode-map (kbd "M-h") nil)
(define-key diff-mode-map (kbd "C-c k") 'diff-hunk-next)
(define-key diff-mode-map (kbd "C-c i") 'diff-hunk-prev)
(define-key diff-mode-map (kbd "C-c C-e") 'diff-ediff)

;; Ediff:
;; keys in files ediff
(defun my-ediff-mode-config ()
  (ediff-setup-keymap) ;; what it does, creates a keymap? Without it doesn't work
  (define-key ediff-mode-map "i" 'ediff-previous-difference)
  (define-key ediff-mode-map "k" 'ediff-next-difference)
)
(add-hook 'ediff-mode-hook 'my-ediff-mode-config)

;; keys in list of files (session group panel)
(defun my-ediff-meta-buffer-config ()
  (define-key ediff-meta-buffer-map "i" 'ediff-previous-meta-item)
  (define-key ediff-meta-buffer-map "k" 'ediff-next-meta-item)
)
(add-hook 'ediff-meta-buffer-keymap-setup-hook 'my-ediff-meta-buffer-config)

;; gdb
(require 'ars-gdb-stuff)
(global-set-key (kbd "C-c C-g") 'gdb-core)

;; magit:
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(define-key magit-mode-map "k" 'magit-section-forward)
(define-key magit-mode-map "i" 'magit-section-backward)
(define-key magit-mode-map (kbd "M-n") nil)
(define-key magit-mode-map (kbd "M-k") 'magit-discard)

;; bookmarks mode
(with-eval-after-load "bookmark"
  (local-set-key (kbd "C-o") nil) ; remove a key
  (define-key bookmark-bmenu-mode-map (kbd "i") 'previous-line)
  (define-key bookmark-bmenu-mode-map (kbd "k") 'next-line)
)

;; auctex
;; compile until needed
(add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-a") 'TeX-texify)))

;; info mode
(define-key Info-mode-map (kbd "i") 'my-scroll-down-one)
(define-key Info-mode-map (kbd "k") 'my-scroll-up-one)
(define-key Info-mode-map (kbd "o") 'Info-index) ; use o for index instead

;; Markdown
(with-eval-after-load "markdown-mode"
  (define-key markdown-mode-map (kbd "M-h") nil)
  (define-key markdown-mode-map (kbd "M-n") nil)
  (define-key markdown-mode-map (kbd "M-p") nil)
)

;;____________________________________________________________

;; Ideas for future mappings:
;; C-S-Z -- redo, tree plugin needed. Should have another shortcut for the console...
;; C-space -- autocomplete
;; C-b -- goto definition
;; M-B -- go back?
;; C-T -- reopen last closed

;; TODO list:
;; cheatsheet in pdf
;; save init.el automatically before reloading
;; fill-unfill from ergo
;; Winner mode?
;; icicles?
;; check out https://github.com/jwiegley/use-package
;; fix highlight matching ()
;; save tree in desktop+?
;; save remote files in desktop+?
;; tail messages toggle instead of enable/disable; or better disable when active,
;;   enable when leaving
;; check kbd escaping
;; autocomplete in code
;; autocomplete helm/ivy/ido -- ivy, to be more precise
;; goto definition, goto file in project, etc
;; go back?
;; cycle windows?
;; wrap long lines without touching the file
;; undo doesn't undo if it requires movement (like in idea)?
;; spelling correction
;; ivy can't open file with name being a substing of the existing
;; smartparens
;; read http://pages.sachachua.com/.emacs.d/Sacha.html
;; check out ivy links
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
