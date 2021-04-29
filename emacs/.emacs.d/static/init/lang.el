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
  :bind (:map lsp-mode-map
	      ("C-b" . lsp-find-definition)
	      ("M-<f7>" . lsp-find-references)
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
  ;; prefix for lsp-mode-map
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))


;; rust
;;
;; you must install rust (obviously, e.g. via rustup) and rust-analyzer for this
;; to work
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-display-spinner nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'ars/rustic-mode-hook))

(defun ars/rustic-mode-hook ()
  ;; so that run lsp-rename  works without having to confirm
  (setq-local buffer-save-without-query t))
