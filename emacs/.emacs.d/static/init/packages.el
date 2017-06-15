;; Packaging stuff and basic things

;; Here we will keep elpa packages
(setq package-user-dir
      (expand-file-name "elpa" emacs-d))

;; add package repos
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; load elpa packages right now because we need package-installed-p,
;; see https://www.emacswiki.org/emacs/ELPA
(setq package-enable-at-startup nil)
(package-initialize)
(message "Packages are loaded")

(defvar required-packages
  '(auctex
    desktop+ ; to save sessions
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
    markdown-mode ; markdown editing and viewing
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
