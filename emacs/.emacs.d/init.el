;; Targeted at GNU Emacs, minimum version 24.5
;; See how to build it in Ubuntu here:
;; http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/

;; Store autogenerated conf in this file. In fact, I don't like the idea of
;; (auto) customize at all, so I will not even load this file; I couldn't
;; quickly find a way to disable it completely.
;; Perhaps later I should read how abo-abo sets variables:
;; https://oremacs.com/2015/01/17/setting-up-ediff/
;; For now I will just use old-school setq everywhere, however this might be
;; dangerous, see
;; http://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el
(setq custom-file "~/.custom.el")

;;________________________________________________
;; Packaging stuff
;; add package repos
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; load elpa packages right now because we need package-installed-p,
;; see https://www.emacswiki.org/emacs/ELPA
(setq package-enable-at-startup nil)
(package-initialize)
(message "Packages are loaded")

(defvar required-packages
  '(auctex
    desktop+ ; to save sessionsq
    neotree ; project tree
    dash ; required by all-icons (used for neat icons in neotree)
    drag-stuff ; moving lines and regions
    xcscope ; frontend for cscope
    ggtags ; frontend for gnu global
    swiper ; contains ivy, a completion frontend, and best search ever
    projectile ; for managing projects
    flycheck ; live syntax checking
    ace-window ; quick window switching from abo-abo
    undo-tree ; replace default undo/redo system
    magit ; git on steroids
   )
  "A list of packages to ensure are installed at launch.")
(require 'cl-lib)

(defun required-packages-installed-p ()
  (cl-every 'package-installed-p required-packages))

(unless (required-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; add to load path all packages under static_packages.
;; NOTE: subdirs will not be added!
;; for now I will include every multifile package manually
;; We will put packages not found in repos here.
;; If you want to quickly reload the library you are editing, use load-library
;; function.
(add-to-list 'load-path "~/.emacs.d/static_packages/")
;; Yes, this is an example of multifile package, so we are forced
;; to add it manually
(add-to-list 'load-path "~/.emacs.d/static_packages/all-the-icons.el")

;; Load custom functions for which I have not found their own file yet
(require 'my-misc)

;;____________________________________________________________
;; Visual things, colours, fonts, etc
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'wombat t) ; current theme
;; font size, in px*10
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default t :font "Ubuntu Mono")
(toggle-scroll-bar -1)

;;____________________________________________________________
;; Backuping and loading things
;; a place where to put backup files
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
;; always backup by copying. TODO: read what that means
(setq backup-by-copying t)
;; put autosave files there too
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs_saves")))

;; Desktop saving stuff
;; Finally I decided to use desktop+ extension:
;; https://github.com/ffevotte/desktop-plus
;; It just works.
;; TODO: add bash command
(setq desktop-load-locked-desktop "ask"
      desktop-restore-frames      t) ; save windows layout; works only since 24.4

;; autoreload all files from disk, by default every 5 seconds
(global-auto-revert-mode 1)
;; and remote files too
(setq auto-revert-remote-files t)

;; replace standard undo/redo system with undo-tree package
(require 'undo-tree)
(global-undo-tree-mode)

;; save command history
;; TODO: does it work with all needed commands?
(savehist-mode 1)
(setq savehist-file "~/.emacs_history")

;; show in C-x b recently opened files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; show bookmarks after emacs starts
(setq inhibit-splash-screen t)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;;_________________________________________________
;; Text processing
(transient-mark-mode 1) ; Highlight the region when the mark is active
;; (next-line) will insert a newline at the end of the buffer
;; (setq next-line-add-newlines t)
;; CUA things
(cua-mode t) ; It will make usual C-c C-v copypasting work.
(setq cua-keep-region-after-copy t) ; well, keep region after copy

;; (setq drag-stuff-modifier '(meta shift))
(drag-stuff-mode t) ; move lines and regions

(require 'my-text-processing)
;; (load "my-text-processing.el") ;; for debugging

;;______________________________________________________________
;; Windows and frames

;; project tree
(require 'all-the-icons) ; load icons for 'icons regime
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

(require 'my-windows-frames-funcs)
;; (load "my-windows-frames-funcs.el") ;; for debugging

;; always tail *Messages* buffer
(tail-messages-enable)


;;____________________________________________________________
;; dired things
;; load dired-x library. I use it to quickly rename files
;; not sure it is a right way to load it...
(require 'dired-x)

;; setting hidden files in dired
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))


;;____________________________________________________________
;; Managing minor modes
(global-linum-mode t) ; show line numbers
(setq column-number-mode t) ; show column number

;; hide toolbar and menubar
(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
    )
)

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

;; default method for remote editing
 (setq tramp-default-method "ssh")


;;______________________________________
; Latex stuff
;;______________________________________
(setq TeX-parse-self t) ; enable parse on load.
(setq TeX-PDF-mode t) ; use pdflatex instead of latex
(setq TeX-auto-save t) ; enable parse on saVe.

(add-hook 'Latex-mode-hook 'LaTeX-math-mobde)

;; ???
(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "okular %o")
        ("^html?$" "." "iceweasel %o"))))


;;____________________________________________________________
;; Well, about going to definition, autocompletion, etc
;;
;; Short review:
;; Etags & Ctags programs for generating tags -- definitions only.
;; It seems that there is buit-in support
;; for etags format in emacs, ctags can also  generate these files
;;
;; GNU Global. Another tagger, more powerful, knows about references. Have it's
;; own file format. There a bunch of emacs frontends, most notable are ggtags.el
;; and helm-gtags.el. Global also provides and interface like cscope, so it
;; can be used instead of it. I don't know cons and pros of this approach yet.

;; Cscope. Another tagger, powerful beast too, knows about references.
;; There are again a number of frontends, but the most popular seems to be
;; xcscope.el

;; CEDET, senator, and a lot of more great words: some monster which seems to
;; be capable of anything, but it looks like no one can set it up rightly.

;; Let's try Cscope with xcscope.el for now.
;; (require 'xcscope)
;; (cscope-setup)

;; Now let's try gnu global
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;;____________________________________________________________
;; Autocompletion (non-code)
;;
;; Short review:
;; Helm is a big buggy monster. ido seems to be good old stuff. ivy -- modern,
;; small and efficient package. Let's try it.

;; toggle it on and off to ivy-mode command
(ivy-mode 1) ; enable ivy completion everywhere
;; add recent visited files (if recentf is on) and bookmarks (which bookmarks?)
;; to list of buffers
(setq ivy-use-virtual-buffers t)
;; display index and total files number in the ivy buffer
(setq ivy-count-format "(%d/%d) ")
(setq ivy-wrap t)


;;____________________________________________________________
;; Project management

;; turn on projectile
;; Right now I plan to use it only for two things: switching between header and
;; src, and, perhaps, quick project switching (basically, it just lets me avoid
;; typing full path to the project)
;; Now there is another feature which I use: regexp replacing in the project,
;; I couldn't get it to work in ggtags.
(projectile-mode)


;;____________________________________________________________
;; Coding styles

;; Load Postgres code style
(require 'coding-styles)

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

;; show trailing whitespaces
(setq show-trailing-whitespace 1)
;; and, well, delete them on save
(add-to-list 'write-file-functions 'delete-trailing-whitespace)


;;____________________________________________________________
;; Syntax checking and other checks on the fly

;; enable flychecker
(global-flycheck-mode)
;; Here we can disable some checkers.
;; It is a buffer-local variable, but using setq-default I set it's value
;; globally.
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck))
;; set clang executable path
(setq flycheck-c/c++-clang-executable "clang-3.8")
;; Probably you will also want to put in the .dir-locals.el file of the root of
;; you project something like
;; ((c-mode
;;   (flycheck-clang-include-path . (
;; 				  "/home/ars/postgres/postgresql/src/include"
;; 				  "/home/ars/tmp/tmp/postgresql_build/src/include"
;; 				  "/home/ars/postgres/reverse_executor_test_extension/src/include"
;; 				  ))
;;   (flycheck-clang-args . (
;; 			  "-Wignored-attributes"
;; 			  ))
;;   (flycheck-gcc-include-path . (
;; 				  "/home/ars/postgres/postgresql/src/include"
;; 				  "/home/ars/tmp/tmp/postgresql_build/src/include"
;; 				  "/home/ars/postgres/reverse_executor_test_extension/src/include"
;; 				  ))
;;  ;; for some reason headers don't include postgres.h which leads to a lot of
;;  ;; errors in them
;;  (flycheck-gcc-args . ("-include" "postgres.h"))
;; ))

;; Force emacs to take local vars needed for flycheck as safe with any
;; value; probably it is not very secure, but much more handy
(put 'flycheck-clang-include-path 'safe-local-variable (lambda (xx) t))
(put 'flycheck-clang-args 'safe-local-variable (lambda (xx) t))
(put 'flycheck-gcc-include-path 'safe-local-variable (lambda (xx) t))
(put 'flycheck-gcc-args 'safe-local-variable (lambda (xx) t))

;; highlight changed lines
(require 'diff-hl-flydiff)
(global-diff-hl-mode t)
(diff-hl-flydiff-mode t) ; even when they are not saved yet


;;____________________________________________________________
;; GDB stuff

(require 'my-gdb-stuff)
;; (load "my-gdb-stuff.el") ;; for debugging, no pun intended

;; I hate speedbar with gud-watch'ed vars popping up in separate frame.
;; sometimes this helps opening it in the same frame, but this question needs
;; to be addressed more seriously
(require 'sr-speedbar)

;; enable showing variable value on mouseover
(gud-tooltip-mode t)

;;____________________________________________________________
;; Ediff stuff

;; Keep control buffer in the same frame as diff buffers
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; side-by-side representation, yes, that's what they call "horizontal split"
(setq ediff-split-window-function 'split-window-horizontally)
;; Don't ask confirmation on exit, see
;;http://emacs.stackexchange.com/questions/9322/how-can-i-quit-ediff-immediately-without-having-to-type-y
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))
(advice-add 'ediff-quit :around #'disable-y-or-n-p)

;; ediff colours with my wombat theme are too bright
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-background ediff-fine-diff-face-B "#277227") ; dark green
            (set-face-background ediff-fine-diff-face-A "#681218") ; dark red
	    (set-face-background ediff-even-diff-face-A "#293035")
	    (set-face-background ediff-even-diff-face-B "#293035")
	    (set-face-background ediff-odd-diff-face-A "#161c30")
	    (set-face-background ediff-odd-diff-face-B "#161c30")
	    ))

;; A hacky way to restore window configuration after exiting from ediff
;; It works badly with magit diff though...
;; (winner-mode 1)
;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;____________________________________________________________
;; Make the keys work with russian layout
(require 'my-misc) ;; 'requires' are idempotent, you know
(reverse-input-method "russian-computer")

;;____________________________________________________________
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
(global-set-key (kbd "<f1> j") 'info-apropos) ; search for string in all manuals
(global-set-key [f2] 'dired-omit-mode) ; show and hide hidden files in dired
;; enable/disable ivy mode until I deal with excess completing problem
(global-set-key [f3] 'ivy-mode)
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
;; TODO: save init.el automatically before reloading


;; The digits row
(global-set-key (kbd "\C-x4") 'my-split-root-window-below)
(global-set-key (kbd "\C-x5") 'my-split-root-window-right)
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
;; dirty hack to distinguish TAB and C-i, see
;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
;; accepted answer doesn't work
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(global-set-key (kbd "H-i") 'my-scroll-down-one)
(global-set-key (kbd "M-i") 'previous-line) ; ergo
(global-set-key (kbd "M-I") 'drag-stuff-up)
(global-set-key (kbd "M-o") 'forward-word) ; ergo
(global-set-key (kbd "C-o") 'find-file) ; ergo
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


;; Now mode-specific bindings
;; ggtags: TODO enable them only in C mode
(global-set-key (kbd "C-b") 'ggtags-find-tag-dwim)
(global-set-key (kbd "M-B") 'ggtags-prev-mark)
(global-set-key (kbd "C-M-b") 'ggtags-next-mark)
(global-set-key (kbd "M-<f7>") 'ggtags-find-reference) ; idea
(global-set-key (kbd "M-F") 'ggtags-grep) ; grepping the project
(global-set-key (kbd "C-S-N") 'ggtags-find-file) ; similar to idea
(global-set-key (kbd "C-n") 'ggtags-find-definition) ; similar to idea
(require 'ggtags)
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
(global-set-key (kbd "M-O") 'projectile-switch-project)
(global-set-key (kbd "M-R") 'projectile-replace-regexp)

;; shell-like modes:
;; we don't need moving up and down in the shells, let them scroll history
;; commands instead:
(define-key comint-mode-map (kbd "M-i") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-k") 'comint-next-input)
;; we don't need replace either, let it do command search as always:
(define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)


;; c-mode:
;; Disable C-d default meaning in c-mode, we need it for duplicating lines
(defun my-c-mode-config ()
  (local-set-key (kbd "C-d") nil) ; remove a key
  (local-set-key (kbd "C-c k") 'flycheck-next-error)
  (local-set-key (kbd "C-c i") 'flycheck-previous-error)
)
(add-hook 'c-mode-hook 'my-c-mode-config)


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


;; magit:
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(define-key magit-mode-map "k" 'magit-section-forward)
(define-key magit-mode-map "i" 'magit-section-backward)
(define-key magit-mode-map (kbd "M-n") nil)

;; bookmarks mode
(with-eval-after-load "bookmark"
  (local-set-key (kbd "C-o") nil) ; remove a key
  (define-key bookmark-bmenu-mode-map (kbd "i") 'previous-line)
  (define-key bookmark-bmenu-mode-map (kbd "k") 'next-line)
)

;;____________________________________________________________

;; Ideas for future mappings:
;; C-S-Z -- redo, tree plugin needed. Should have another shortcut for the console...
;; C-space -- autocomplete
;; C-b -- goto definition
;; M-B -- go back?
;; C-T -- reopen last closed

;; design the shorcuts for the following things:
;; move screen one line up and down, keeping cursor near the border
;; switching  windows, four or at least two keys
;; switching buffers: next buffer, previous, general

;; TODO list:
;; cheatsheet in txt
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
