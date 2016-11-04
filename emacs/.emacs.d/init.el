;; Targeted at GNU Emacs, minimum version 24.5
;; See how to build it in Ubuntu here:
;; http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/

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
;; TODO: subdirs will not be added! deal with it.
;; for now I will include every multifile package manually
;; We will put packages not found in repos here.
(add-to-list 'load-path "~/.emacs.d/static_packages/")
(add-to-list 'load-path "~/.emacs.d/static_packages/all-the-icons.el")

;;____________________________________________________________
;; Visual things, colours, fonts, etc
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'wombat t) ; current theme
;; font size, in px*10
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default t :font "Ubuntu Mono")

;;____________________________________________________________
;; Backup things
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

;; autoreload all files from disk
(global-auto-revert-mode 1)

;;_________________________________________________
;; Text processing
(transient-mark-mode 1) ; Highlight the region when the mark is active
;; (next-line) will insert a newline at the end of the buffer
;; (setq next-line-add-newlines t)
;; CUA things
(cua-mode t) ; It will make usual C-c C-v copypasting work, but I will try to avoid them for now.
(setq cua-keep-region-after-copy t) ; well, keep region after copy

(setq drag-stuff-modifier '(meta shift))
(drag-stuff-mode t) ; move lines and regions

(require 'my-text-processing)
;; (load "my-text-processing.el") ;; for debugging

;;______________________________________________________________
;; Windows and frames
;; maximize frame function, see
;; http://stackoverflow.com/questions/9248996/how-to-toggle-fullscreen-with-emacs-as-default 
(defun switch-fullscreen nil
  (interactive)
  (let* ((modes '(nil maximized))
         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
         (next (cadr (member cm modes) ) ) )
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))


;; functions for adding window on right or below
(defun my-split-root-window (size direction)
  (split-window (frame-root-window)
                (and size (prefix-numeric-value size))
                direction))
(defun my-split-root-window-below (&optional size)
  (interactive "P")
  (my-split-root-window size 'below))
(defun my-split-root-window-right (&optional size)
  (interactive "P")
  (my-split-root-window size 'right))

;; project tree
(require 'all-the-icons) ; load icons for 'icons regime
;; Every time when the neotree window is opened, let it find current file and
;; jump to node.
(setq neo-smart-open t)
(setq neo-theme (if window-system 'icons 'arrow))
;; neo tree ignore list
(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.so$"))

;; always tail *Messages* buffer, see
;; http://stackoverflow.com/questions/13316494/how-to-follow-the-end-of-messages-buffer-in-emacs
(defun tail-f-msgs ()
  "Go to the end of Messages buffer."
  (let ((msg-window (get-buffer-window "*Messages*")))
    (if msg-window
        (with-current-buffer (window-buffer msg-window)
          (set-window-point msg-window (point-max))))))
(defun tail-messages-enable()
  (interactive)
  (add-hook 'post-command-hook 'tail-f-msgs))
(defun tail-messages-disable()
  (interactive)
  (remove-hook 'post-command-hook 'tail-f-msgs))
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
       (("^pdf$" "." "okulaar %o")
        ("^html?$" "." "iceweasel %o"))))

(custom-set-variables
  '(TeX-view-program-list (quote (("Okular" "okular --unique %o"))))
 '(TeX-view-program-selection
  (quote
   (((output-dvi style-pstricks) "dvips and gv")
   (output-dvi "xdvi")
   (output-pdf "Okular")
   (output-html "xdg-open")))))


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
(require 'xcscope)
(cscope-setup)

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
(projectile-mode)

;;____________________________________________________________
;; Now, the keys.
;; About prefixes:
;; * Alt is the best
;; * Ctrl is the second priority (right Alt)
;; * Shift Alt is ok, if the key is for right hand, but pinky is getting nervious
;; * Ctrl Alt is ok, if the key is on the home row or nearby. Ergo doesn't use it
;; * Ctrl Shift doesn't work in terminals, avoid it for important keys!
;; * Ctrl Alt Shift -- I reserved for system apps

;; The F keys row
(global-set-key [f1] help-map) ; help prefix
(global-set-key [f6] 'desktop+-create) ; save desktop
(global-set-key [f7] 'desktop+-load) ; load saved desktop
(global-set-key [f8] 'neotree-toggle) ; show and hide neotree
(define-key global-map [f11] 'switch-fullscreen)
(global-set-key [f12] 'dired-omit-mode) ; show and hide hidden files in dired
;; M-insert now reloads init.el. Lambda here just passes closured load-file as a function
(global-set-key [M-insert] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))
;; TODO: save init.el automatically before reloading


;; The digits row
(global-set-key (kbd "\C-x4") 'my-split-root-window-below)
(global-set-key (kbd "\C-x5") 'my-split-root-window-right)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;; The qwerty row:
(global-set-key (kbd "M-w") 'just-one-space) ; ergo
(global-set-key (kbd "C-w") 'kill-this-buffer) ; ergo
(global-set-key (kbd "M-e") 'backward-kill-word) ; ergo
(global-set-key (kbd "M-r") 'kill-word) ; ergo
(global-set-key (kbd "C-M-r") 'revert-buffer)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-y") 'smart-kill-whole-line)
(global-set-key (kbd "M-u") 'backward-word) ; ergo
(global-set-key (kbd "M-i") 'previous-line) ; ergo
(global-set-key (kbd "M-I") 'drag-stuff-up)
(global-set-key (kbd "M-o") 'forward-word) ; ergo
(global-set-key (kbd "C-o") 'find-file) ; ergo
(global-set-key (kbd "M-p") 'recenter-top-bottom) ; ergo

;; The home row:
(global-set-key (kbd "C-a") 'mark-whole-buffer) ; ergo
(global-set-key (kbd "M-a") 'execute-extended-command) ; ergo
(global-set-key (kbd "C-s") 'save-buffer) ; ergo
(global-set-key (kbd "M-d") 'backward-delete-char) ; ergo
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "M-f") 'delete-forward-char) ; ergo
(global-set-key (kbd "C-f") 'swiper) ; ergo
(global-set-key (kbd "M-g") 'kill-line) ; kill the rest of the line; ergo
(global-set-key (kbd "M-G") '(lambda () (interactive) (kill-line 0))) ; kill the line up to the cursor, ergo
(global-set-key (kbd "M-h") 'move-beginning-of-line) ; ergo
(global-set-key (kbd "M-H") 'move-end-of-line) ; ergo
(global-set-key (kbd "M-j") 'backward-char) ; ergo
(global-set-key (kbd "M-k") 'next-line) ; ergo
(global-set-key (kbd "M-K") 'drag-stuff-down) ; ergo
(global-set-key (kbd "M-l") 'forward-char) ; ergo
(global-set-key (kbd "C-l") 'goto-line) ; ergo
(global-set-key (kbd "C-;") 'comment-idea)
;; idea's shift-enter, defined in my-text-processing.el
(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)

;; The zxcv row:
(global-set-key (kbd "M-c") 'forward-paragraph)
;; cua is shadowing keys, http://stackoverflow.com/questions/34057023/how-do-you-rebind-a-key-set-by-cua-mode-in-emacs
(define-key cua--cua-keys-keymap (kbd "M-v") 'backward-paragraph)
(global-set-key (kbd "M-n") 'beginning-of-buffer)
(global-set-key (kbd "M-N") 'end-of-buffer)

;; Space row
(global-set-key (kbd "M-<SPC>") 'cua-set-mark) ; ergo
;; M-S-arrows move lines and words, it is set before loading drug-stuff
;; enable switching windows with M-arrows
(windmove-default-keybindings 'meta)

;; Now mode-specific bindings
;; ggtags:
(global-set-key (kbd "C-b") 'ggtags-find-tag-dwim)
(global-set-key (kbd "M-B") 'ggtags-prev-mark)
(global-set-key (kbd "C-M-b") 'ggtags-next-mark)
(global-set-key (kbd "M-<f7>") 'ggtags-find-reference)
(global-set-key (kbd "M-F") 'ggtags-grep) ; grepping the project
(global-set-key (kbd "C-n") 'ggtags-find-file) ; similar to idea
(global-set-key (kbd "M-R") 'ggtags-query-replace) ; similar to idea
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


;;____________________________________________________________

;; Ideas for future mappings:
;; C-S-Z -- redo, tree plugin needed. Should have another shortcut for the console...

;; very future mappings:
;; C-space -- autocomplete
;; C-b -- goto definition
;; M-B -- go back?
;; C-T -- reopen last closed
;; fill-unfill from ergo

;; TODO list:
;; cheatsheet in txt
;; Winner mode?
;; icicles?
;; check out https://github.com/jwiegley/use-package
;; fix highlight matching ()
;; save tree in desktop+?
;; save remote files in desktop+?
;; autorevert remote files?
;; tail messages toggle instead of enable/disable; or better disable when active,
;;   enable when leaving
;; check kbd escaping
;; autocomplete in code
;; autocomplete helm/ivy/ido -- ivy, to be more precise
;; goto definition, goto file in project, etc
;; go back?
;; cycle windows?
;; cycle buffers?
;; fix moving beyond the window
;; highlighting git differences like in idea
;; tabs?
;; undo doesn't undo if it requires movement?

