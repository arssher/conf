;; -*- mode: emacs-lisp -*-

;; This file contains code to set up Emacs to edit PostgreSQL source
;; code.  Copy these snippets into your .emacs file or equivalent, or
;; use load-file to load this file directly.
;;
;; Note also that there is a .dir-locals.el file at the top of the
;; PostgreSQL source tree, which contains many of the settings shown
;; here (but not all, mainly because not all settings are allowed as
;; local variables).  So for light editing, you might not need any
;; additional Emacs configuration.


;;; C files

;; In postgres, tabs are used everywhere, i.e. not only for indentation, but
;; for the alignment too. I will tabify the line everytime it is indented by
;; Emacs.
(defun postgres-tabify-after-indent ()
  (tabify (line-beginning-position) (line-end-position))
)

;; Style that matches the formatting used by
;; src/tools/pgindent/pgindent.  Many extension projects also use this
;; style.
(c-add-style "postgresql"
             '("bsd"
               (c-auto-align-backslashes . nil)
               (c-basic-offset . 4)
               (c-offsets-alist . ((case-label . +)
                                   (label . -)
                                   (statement-case-open . +)))
               (fill-column . 78)
               (indent-tabs-mode . t)
               (tab-width . 4)
	       (c-special-indent-hook . postgres-tabify-after-indent)
	      )
)


(add-hook 'c-mode-hook
          (defun postgresql-c-mode-hook ()
            (when (string-match "/postgres\\(ql\\)?/" (if buffer-file-name buffer-file-name ""))
              (c-set-style "postgresql")
              ;; Don't override the style we just set with the style in
              ;; `dir-locals-file'.  Emacs 23.4.1 needs this; it is obsolete,
              ;; albeit harmless, by Emacs 24.3.1.
              (set (make-local-variable 'ignored-local-variables)
                   (append '(c-file-style) ignored-local-variables)))))


;;; Perl files

;; Style that matches the formatting used by
;; src/tools/pgindent/perltidyrc.
(defun pgsql-perl-style ()
  "Perl style adjusted for PostgreSQL project"
  (interactive)
  (setq perl-brace-imaginary-offset 0)
  (setq perl-brace-offset 0)
  (setq perl-continued-brace-offset -2)
  (setq perl-continued-statement-offset 2)
  (setq perl-indent-level 4)
  (setq perl-label-offset -2)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

;; It used to be wrapped in
;;    (when (string-match "/postgres\\(ql\\)?/" buffer-file-name) ...)
;; but since I code in perl for pg only, set the style unconditionally
(add-hook 'perl-mode-hook
          (defun postgresql-perl-mode-hook ()
               (pgsql-perl-style)))


;;; documentation files

(add-hook 'sgml-mode-hook
          (defun postgresql-sgml-mode-hook ()
             (when (and (buffer-file-name) (string-match "/postgres\\(ql\\)?/"
             buffer-file-name))
               (setq fill-column 78)
               (setq indent-tabs-mode nil)
               (setq sgml-basic-offset 1))))


;;; Makefiles

;; use GNU make mode instead of plain make mode
(add-to-list 'auto-mode-alist '("/postgres\\(ql\\)?/.*Makefile.*" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("/postgres\\(ql\\)?/.*\\.mk\\'" . makefile-gmake-mode))

(message "coding styles loaded")
(provide 'pg-coding-styles)
