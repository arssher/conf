;;; -*- lexical-binding: t -*-
;; Backuping and restoring things, generally anything that remembers
;; something and later uses it


;; a place where to put backup files
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
;; always backup by copying. TODO: read what that means
(setq backup-by-copying t)
;; put autosave files there too
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs_saves")))

;; autoreload all files from disk, by default every 5 seconds
(global-auto-revert-mode 1)

;; replace standard undo/redo system with undo-tree package
(require 'undo-tree)
(global-undo-tree-mode)
;; put *.~undo-tree~ files to ~/.emacs.d/undo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; save command history
;; TODO: does it work with all needed commands?
(setq history-length t) ;; infinite history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

;; show in C-x b recently opened files
(require 'recentf)
(recentf-mode 1)
;; How many of them survive a restart.  The default is 25, and it applies when
;; the list is written rather than as files are added -- recentf-push only
;; dedupes and pushes, it never truncates -- so a long session remembers
;; everything and only a restart drops back to this.  Not
;; recentf-max-menu-items, which stood here instead and sizes nothing but
;; File > Open Recent.
(setq recentf-max-saved-items 500)
;; recentf's one save hook is kill-emacs-hook, and its one timer runs
;; recentf-cleanup, which prunes the list without writing it.  So without the
;; line below the file lands on a clean exit and at no other time, and a crash
;; or a reboot loses every file opened since startup -- days of them, on a
;; machine where emacs stays up.  savehist above autosaves on its own timer,
;; which is why that file stays current while recentf.eld went stale.
;;
;; Silent: recentf-save-list passes 'quiet to write-region unless it was
;; called interactively.  nil time means start now, then every REPEAT.
(run-at-time nil (* 5 60) #'recentf-save-list)

;; always update bookmarks file
(setq bookmark-save-flag 1)
;; load bookmarks
(bookmark-bmenu-list)
