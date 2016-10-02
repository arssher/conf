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
; load elpa packages right now because we need package-installed-p, see https://www.emacswiki.org/emacs/ELPA
; now we must require every (or almost every? -- move-text, for instance, works without it) used package
(setq package-enable-at-startup nil)
(package-initialize)
(load "auctex.el" nil t t)
(require 'move-text)
(message "Packages are loaded")

(defvar required-packages
  '(auctex move-text)
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
;;

;;__________________________________________________
;; Desktop saving stuff
;; Automatically save and restore sessions
(require 'desktop)
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-restore-frames      t) ; save windows layout; works only since 24.4Xo
(desktop-save-mode 1) ; save session on exit
; taken from https://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
    (desktop-save desktop-dirname)))
;; save desktop automatically on each autosave, i.e. each 30 seconds by default
(add-hook 'auto-save-hook 'my-desktop-save)
;;__________________________________________________


;;_________________________________________________
;; Text processing
(cua-mode t) ; Make usual C-c C-v copypasting work
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

; move text arg lines down (or |arg| lines up, if arg < 0)
; TODO: rewrite marked mode and handle start of buffer case
(defun move-text-internal (arg)
  (message "arg is %d" arg)
  (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     ; TODO: save cursor
     (beginning-of-line)
     (forward-line)
     (transpose-lines arg)
     (forward-line -1)
     (when (< arg 0)
       (forward-line arg)
     )
    )
  )
)

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
   ; 'interactive makes function a command;
   ; p converts argument to number, * ensures that buffer is writable (signals, if it is read-only)
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\C-\S-down] 'move-text-down)
(global-set-key [\C-\S-up] 'move-text-up)


;(move-text-default-bindings) ; enable move text with default bindings, M-up M-down moves lines
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

(global-linum-mode t) ; show line numbers


;______________________________________
; Latex stuff
;______________________________________
(setq TeX-parse-self t) ; enable parse on load.
(setq TeX-auto-save t) ; enable parse on save.
(setq TeX-PDF-mode t) ; use pdflatex instead of latex

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "iceweasel %o"))))

