;; go
;; load go-guru
(require 'go-guru)

(defun my-go-mode-hook ())

(add-hook 'go-mode-hook 'my-go-mode-hook)


;; TLA+
(defun untabify-tla ()
  "Replace tabs with spaces in .*.tla file"
  (if (string-match-p ".tla\\'" (buffer-name))
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-tla)


;; Asm
(require 'asm-mode)
(add-hook 'asm-mode-hook (lambda ()
                           (setq indent-tabs-mode nil) ; use spaces to indent
                           (electric-indent-mode -1) ; indentation in asm-mode is annoying
                           (setq tab-stop-list (number-sequence 2 60 2)) ; much better indentation
			   )
	  )


;; SQL mode basic setup

;; currently not used, just for playing around
(require 'sql-indent)
;; (defvar my-sql-indentation-offsets-alist
;;  `((select-clause 0)
;;    (insert-clause 0)
;;    (delete-clause 0)
;;    (update-clause 0)
;;    ,@sqlind-default-indentation-offsets-alist))

(defun sql-style ()
  (setq indent-tabs-mode nil)

  (sqlind-minor-mode t)
  (setq sqlind-basic-offset 4))
  ;; (setq sqlind-indentation-offsets-alist
             ;; my-sql-indentation-offsets-alist))

;; currently it is just horrible, disable it
;; (add-hook 'sql-mode-hook 'sql-style)


;; tex
(require 'ars-tex)
(setq TeX-parse-self t) ; enable parse on load.
(setq TeX-PDF-mode t) ; use pdflatex instead of latex
(setq TeX-auto-save t) ; enable parse on save

(setq TeX-view-program-selection '((output-pdf "Okular")) )

(setq TeX-texify-Show nil)

;; check with lacheck
;; Right now it doesn't work well with 'listing' listing, so I turning it off
;; (add-hook 'LaTeX-mode-hook
	  ;; (lambda ()
	    ;; (setq flycheck-checker 'tex-lacheck)))


;; lsp client
(use-package lsp-mode
  :commands lsp
  ;; lsp binds into xref, e.g. xref-find-definitions, so we don't need lsp-find-definition
  ;; or similar
  :bind (:map lsp-mode-map
	      ("C-b" . xref-find-definitions)
	      ("M-<f7>" . xref-find-references)
	      ("M-/" . completion-at-point))
  ;; see https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  :config
  ;; highlighting occurences of symbol the cursor is currently on
  (setq lsp-enable-symbol-highlighting nil)
  ;; probably later
  (setq lsp-enable-snippet nil)
  ;; flycheck report counts in modeline
  (setq lsp-modeline-diagnostics-enable nil)
  ;; not sure about which pop up this one, but anyway
  (setq lsp-signature-auto-activate nil)
  ;; disable showing signature/other stuff in minibuffer when cursor is on symbol
  (setq lsp-eldoc-hook nil)
  ;; apparently this is only about company which I don't use
  (setq lsp-completion-provider nil)
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; (setq lsp-eldoc-render-all t)
  (setq lsp-idle-delay 0.6)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-response-timeout 10)
  ;; prefix for lsp-mode-map
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))


;; rust
;;
;; you must install rust (obviously, e.g. via rustup) and rust-analyzer for this
;; to work
;;
(use-package rustic
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-display-spinner nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'ars/rustic-mode-hook))

(defun ars/rustic-mode-hook ()
  so that run lsp-rename  works without having to confirm
  (setq-local buffer-save-without-query t))

;; C/C++
;;
;; Short review:
;; Etags & Ctags programs for generating tags -- definitions only. It seems
;; that there is buit-in support for etags format in emacs, ctags can also
;; generate these files
;;
;; GNU Global. Another tagger, more powerful, knows about references. Has its
;; own file format. There are a bunch of emacs frontends, most notable are
;; ggtags.el and helm-gtags.el. Global also provides and interface like cscope,
;; so it can be used instead of it. I don't know cons and pros of this approach
;; yet.

;; Cscope. Another tagger, powerful beast too, knows about references.
;; There are again a number of frontends, but the most popular seems to be
;; xcscope.el

;; CEDET, senator, and a lot of more great words: some monster which seems to
;; be capable of anything, but it looks like no one can set it up rightly.

;; Time to use lang server, its 2021, actually.

;; Let's try Cscope with xcscope.el for now.
;; (require 'xcscope)
;; (cscope-setup)

;; Now let's try gnu global
;; Remember that you need GTAGSLIBPATH env var to search external projects,
;; you can do that in ~/.emacs-local.el
;; (add-hook 'c-mode-common-hook
          ;; (lambda ()
            ;; (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              ;; (ggtags-mode 1))))


;; enable lsp-mode
;; ... you have to get compile_commands.json at the top dir and reload lang
;; server to make it work, which is kinda pain
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


;; turn on projectile
;; Right now I plan to use it only for two things: switching between header and
;; src, and, perhaps, quick project switching (basically, it just lets me avoid
;; typing full path to the project)
;; Now there is another feature which I use: regexp replacing in the project,
;; I couldn't get it to work in ggtags.
;; better set the var in init.el, but it must be set before loading...
(setq projectile-keymap-prefix (kbd "C-c p"))
(projectile-mode 1)
(setq projectile-completion-system 'ivy)
;; default fast 'alien method ignores 'projectile-globally-ignored-directories
;; with .cache and stuff, so let's switch to hybrid
(setq projectile-indexing-method 'hybrid)
