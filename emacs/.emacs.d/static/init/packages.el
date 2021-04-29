;; Packaging stuff and basic things

;; Here we will keep elpa packages
(setq package-user-dir
      (expand-file-name "elpa" emacs-d))

;; add package repos; use ssl if available
(setq package-archives nil)
(let* ((proto (if (gnutls-available-p) "https" "http")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

;; load elpa packages right now because we need package-installed-p,
;; see https://www.emacswiki.org/emacs/ELPA
(setq package-enable-at-startup nil)
(package-initialize)
(message "Packages are loaded")

(defvar required-packages
  '(use-package ; package for configuring packages, see below
    auctex
    all-the-icons ; for neotree, which I actually don't use
    dash ; required by all-icons (used for neat icons in neotree)
    neotree ; project tree
    drag-stuff ; moving lines and regions
    xcscope ; frontend for cscope
    ggtags ; frontend for gnu global
    swiper ; contains ivy, a completion frontend, and best search ever
    projectile ; for managing projects
    flycheck ; live syntax checking
    ace-window ; quick window switching from abo-abo
    undo-tree ; replace default undo/redo system
    magit ; git on steroids
    bbdb ; email contacts
    counsel-bbdb ; interface to it
    ;; markdown-mode ; markdown editing and viewing
    go-mode ; go support
    sql-indent ; sql indent
    exec-path-from-shell ; source shell vars
    diminish ;; make modeline less noisy
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

;; don't ask long yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; use-package
;;
;; use-package 1) helps lazy loading 2) centralizes package configuration 3)
;; has basic support for automatic package dowloading, which in fact should be
;; enough for me atm.
;; I guess eventually I'd move everything here, but that's what we have currently.
;;
;; Short tutorial:
;; Basic
;;  (use-package foo)
;; loads the package immediately.
;;
;; :init runs some code right now, before the package is loaded. It should be
;; fast and without much dependencies.
;; :config is run once the package is loaded, which often happens lazily.
;;
;; :command, :bind, :mode and similar delay the package loading.
;; :command registers the function autoloaded (package is loaded once on its
;; first invocation)
;; :bind 1) registers function as autoloaded 2) immediately binds it
;; ;mode 1) registers function turning on mode (named as package unless explicitly given)
;;   as autoloaded 2) pushes entry to auto-mode-alist saying when to call this mode
;;
;; ;hook (prog-mode . ace-jump-mode) is a bit weird. It 1) registers ace-jump-mode
;;   as autoloaded, of course 2) add-hook's ace-jump-mode on prog-mode-hook.
;;
;; you can force-disable lazy loading with :demand t
(require 'use-package)

;; ask to download all packages by default automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; automatically update packages on startup
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))
