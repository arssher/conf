;; Backuping and restoring things; more precisely, anything that remembers
;; something and later uses it


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

;; replace standard undo/redo system with undo-tree package
(require 'undo-tree)
(global-undo-tree-mode)

;; save command history
;; TODO: does it work with all needed commands?
(setq history-length t) ;; infinite history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

;; show in C-x b recently opened files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; always update bookmarks file
(setq bookmark-save-flag 1)
;; load bookmarks
(bookmark-bmenu-list)
