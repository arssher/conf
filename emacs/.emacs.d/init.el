; Targeted at GNU Emacs, minimum version 24.5
; See how to build it in Ubuntu here:
; http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/

; (load "myplugin.el")  ; example of loading a plugin

;;________________________________________________
;; Packaging stuff
; add package repos
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
; load elpa packages right now because we need package-installed-p,
; see https://www.emacswiki.org/emacs/ELPA
(setq package-enable-at-startup nil)
(package-initialize)
(message "Packages are loaded")

(defvar required-packages
  '(auctex
    move-text ; to move lines and regions
    desktop+ ; to save sessions
    neotree ; project tree
    dash ; required by all-icons
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

; add to load path all packages under static_packages.
; TODO: subdirs will not be added! deal with it.
; for now I will include every multifile package manually
; We will put packages not found in repos here.
(add-to-list 'load-path "~/.emacs.d/static_packages/")
(add-to-list 'load-path "~/.emacs.d/static_packages/all-the-icons.el")

; M-insert now reloads init.el. Lambda here just passes closured load-file as a function
(global-set-key [M-insert] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))
; TODO: save init.el automatically before reloading
;;_________________________________________________

;; Visual things
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'wombat t) ; current theme
;;

;; Backup files
; a place where to put backup files
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
; always backup by copying. TODO: read what that means
(setq backup-by-copying t)
; put autosave files there too
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs_saves")))

;;

;;__________________________________________________
;; Desktop saving stuff
; Finally I decided to use desktop+ extension:
; https://github.com/ffevotte/desktop-plus
; It just works.
; TODO: add bash command
(setq desktop-load-locked-desktop "ask"
      desktop-restore-frames      t) ; save windows layout; works only since 24.4Xo
;;__________________________________________________


;;_________________________________________________
;; Text processing
; CUA things
(cua-mode t) ; It will make usual C-c C-v copypasting work, but I will try to avoid them for now.
(setq cua-auto-tabify-rectangles nil) ; Don't tabify after rectangle commands WTF
(transient-mark-mode 1) ; No region when it is not highlighted WTF
(setq cua-keep-region-after-copy t) ; Standard Windows behaviour WTF

; duplicate line by C-c C-d, see
; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "\C-c\C-d") 'duplicate-line)

; TODO: call to mind why I wasn't satisfied with out-of-the-box move-text package and decided to rewrite
; these functions manually. Probably because of regions? 
; move text arg lines down (or |arg| lines up, if arg < 0)
; TODO: rewrite marked mode and handle start of buffer case
;(defun move-text-internal (arg)
;  (message "arg is %d" arg)
;  (cond
;    ((and mark-active transient-mark-mode)
;     (if (> (point) (mark))
;            (exchange-point-and-mark))
;     (let ((column (current-column))
;              (text (delete-and-extract-region (point) (mark))))
;       (forward-line arg)
;       (move-to-column column t)
;       (set-mark (point))
;       (insert text)
;       (exchange-point-and-mark)
;       (setq deactivate-mark nil)))
;    (t
;     ; TODO: save cursor
;     (beginning-of-line)
;     (forward-line)
;     (transpose-lines arg)
;     (forward-line -1)
;     (when (< arg 0)
;       (forward-line arg)
;     )
;    )
;  )
;)
;(defun move-text-down (arg)
;  "Move region (transient-mark-mode active) or current line
;  arg lines down."
;   ; 'interactive makes function a command;
;   ; p converts argument to number, * ensures that buffer is writable (signals, if it is read-only)
;  (interactive "*p")
;  (move-text-internal arg))
; 
;(defun move-text-up (arg)
;   "Move region (transient-mark-mode active) or current line arg lines up."
;   (interactive "*p")
;   (move-text-internal (- arg)))

;(global-set-key [\C-\S-down] 'move-text-down)
;(global-set-key [\C-\S-up] 'move-text-up)
(move-text-default-bindings) ; enable move text with default bindings, M-up M-down moves lines

;; enable comments like in idea
(require 'comment-idea)
;; (load "comment-idea.el") ;; for debugging
(global-set-key (kbd "C-;") 'comment-idea)

;;__________________________________________________

; maximize window by pressing F11, see 
; http://stackoverflow.com/questions/9248996/how-to-toggle-fullscreen-with-emacs-as-default 
(defun switch-fullscreen nil
  (interactive)
  (let* ((modes '(nil maximized))
         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
         (next (cadr (member cm modes) ) ) )
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))
(define-key global-map [f11] 'switch-fullscreen)

; open project tree on f8
(require 'all-the-icons) ; load icons for 'icons regime
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if window-system 'icons 'arrow))

;; Managing global minor modes
(global-linum-mode t) ; show line numbers
(setq column-number-mode t) ; show column number

;; hide toolbar and menubar
(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
    )
)

; ruler at 80
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


; load dired-x library. I use it to quickly rename files
; not sure it is a right way to load it...
(require 'dired-x)

; autoreload all files from disk
(global-auto-revert-mode 1)

;; always tail *Messages* buffer, see
;; http://stackoverflow.com/questions/13316494/how-to-follow-the-end-of-messages-buffer-in-emacs
(defun tail-f-msgs ()
  "Go to the end of Messages buffer."
  (let ((msg-window (get-buffer-window "*Messages*")))
    (if msg-window
        (with-current-buffer (window-buffer msg-window)
          (set-window-point msg-window (point-max))))))
(add-hook 'post-command-hook 'tail-f-msgs)

;______________________________________
; Latex stuff
;______________________________________
(setq TeX-parse-self t) ; enable parse on load.
(setq TeX-auto-save t) ; enable parse on saVe.
(setq TeX-PDF-mode t) ; use pdflatex instead of latex

(add-hook 'Latex-mode-hook 'LaTeX-math-mode)

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "iceweasel %o"))))

					; TODO list:
					; cua functionality
					; very strange idents
					; cheatsheet in txt
					; Winner mode?
					; icicles?
					; TODO: check out https://github.com/jwiegley/use-package
					; moving regions?
					; Transient mark
					; fix highlight matching ()

