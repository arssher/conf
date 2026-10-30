;;; -*- lexical-binding: t -*-
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
;; I switched to vscode for rust, let's disable it for now.
;; (use-package lsp-mode
;;   :commands lsp
;;   ;; lsp binds into xref, e.g. xref-find-definitions, so we don't need lsp-find-definition
;;   ;; or similar
;;   :bind (:map lsp-mode-map
;;  	      ("C-b" . xref-find-definitions)
;;  	      ("M-<f7>" . xref-find-references)
;;  	      ("M-/" . completion-at-point))
;;   ;; see https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;;   :config
;;   ;; highlighting occurences of symbol the cursor is currently on
;;   (setq lsp-enable-symbol-highlighting nil)
;;   ;; probably later
;;   (setq lsp-enable-snippet nil)
;;   ;; flycheck report counts in modeline
;;   (setq lsp-modeline-diagnostics-enable nil)
;;   ;; not sure about which pop up this one, but anyway
;;   (setq lsp-signature-auto-activate nil)
;;   ;; disable showing signature/other stuff in minibuffer when cursor is on symbol
;;   (setq lsp-eldoc-hook nil)
;;   ;; apparently this is only about company which I don't use
;;   (setq lsp-completion-provider nil)
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (setq lsp-rust-analyzer-cargo-watch-command "clippy")
;;   ;; (setq lsp-eldoc-render-all t)
;;   (setq lsp-idle-delay 0.6)
;;   (setq lsp-rust-analyzer-server-display-inlay-hints t)
;;   (setq lsp-response-timeout 10)
;;   ;; prefix for lsp-mode-map
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))


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
  "So that lsp-rename works without having to confirm."
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

;; Take the project root from the VCS directory instead of asking. With the
;; default nil, the first C file opened in each project gets a "Select action:
;; import project root / blacklist" prompt, and answering it is the only thing
;; that lets the rest of c-mode-hook run. Set before lsp-mode
;; loads, which is fine: a defcustom leaves an already-set value alone.
(setq lsp-auto-guess-root t)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


;; Markdown
;;
;; markdown-ts-mode is built into emacs 31, but unlike the other ts modes it
;; is neither autoloaded nor attached to any file extension: emacs ships it
;; switched off because upstream still calls it experimental. Both the
;; autoload and the auto-mode-alist entry below are therefore ours.
;;
;; Guarded on the two grammars actually being present. Debian packages
;; neither, install_emacs.sh builds them, and markdown needs both -- the block
;; grammar and the inline one. Without the guard a machine that had not run
;; that step would open every .md in a mode that errors on arrival. Falling
;; through instead leaves the entry markdown-mode's own autoloads install,
;; which is the old behaviour, and M-x markdown-mode stays reachable either
;; way if the new mode misbehaves on some file.
;;
;; The regexp is markdown-mode's own list of extensions. add-to-list pushes to
;; the front, and packages.el has already run package-initialize by now, so
;; this entry is found first.
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (require 'treesit nil t)
           (locate-library "markdown-ts-mode")
           (treesit-language-available-p 'markdown)
           (treesit-language-available-p 'markdown-inline))
  (autoload 'markdown-ts-mode "markdown-ts-mode"
    "Major mode for Markdown, tree-sitter based." t)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
                 . markdown-ts-mode))
  ;; The view mode (f7) is for reading, so lay the tables out. See
  ;; ars-markdown.el for why this is not something the mode does itself.
  (require 'ars-markdown)
  (add-hook 'markdown-ts-view-mode-hook #'ars-markdown-ts-align-all-tables)
  ;; And draw them wrapped to the window, since a table wider than that is
  ;; unreadable once visual wrapping breaks its rows across lines.
  ;; markdown-table-wrap-pretty-mode, from the markdown-table-wrap package, does
  ;; it for display only: an overlay over each table line shows the wrapped
  ;; rendering, with box-drawing borders, and the buffer keeps the real table.
  ;; It redraws on resize. Every table gets drawn, not only the wide ones, so
  ;; the alignment above shows only for a table toggled back to raw with
  ;; markdown-table-wrap-pretty-toggle. The editing mode keeps raw tables.
  ;; ars-markdown.el has why it is switched on through a function of ours.
  ;; Guarded for a machine where installing the package failed.
  (when (fboundp 'markdown-table-wrap-pretty-mode)
    (setq markdown-table-wrap-pretty-default-on-major-modes
          '(markdown-ts-view-mode))
    (add-hook 'markdown-ts-view-mode-hook #'ars-markdown-ts-wrap-tables))
  ;; Both hooks on purpose, see the function.
  (dolist (hook '(markdown-ts-mode-hook markdown-ts-view-mode-hook))
    (add-hook hook #'ars-markdown-ts-setup-imenu)))

;; Markdown 1pt smaller than everything else, in all three of its modes: the
;; two ts ones above and markdown-mode, which a machine without the grammars
;; still uses. Each needs its own hook -- markdown-ts-view-mode is a child of
;; markdown-ts-mode, and markdown-ts-mode of markdown-mode, only through
;; derived-mode-add-parents, which run-mode-hooks ignores.
;;
;; A function as the :height rather than a number, so the 1pt comes off
;; whatever the default is at the time: an integer would be an absolute size,
;; and a float a ratio, 15/16 of 160 but 1.6pt off the light theme's 250. C-x
;; C-+ and C-x C-- still scale on top. The remapping is buffer-local and a
;; change of major mode drops it, so going between the view and the editing
;; mode never stacks it. 2pt was tried first, and was too much.
(defun ars-markdown-smaller-font ()
  "Make the default face 1pt smaller in this buffer."
  (face-remap-add-relative 'default :height (lambda (height) (- height 10))))

(dolist (hook '(markdown-mode-hook markdown-ts-mode-hook
                markdown-ts-view-mode-hook))
  (add-hook hook #'ars-markdown-smaller-font))


;; Tree-sitter modes for json, shell, toml and python, each once its grammar
;; is built -- install_emacs.sh builds them, and a machine without them keeps
;; the classic modes rather than opening files in a mode with no parser. A
;; grammar is also built on demand by turning its ts mode on by hand once.
;; treesit-enabled-modes is emacs 31's switch: it puts js-json-mode ->
;; json-ts-mode and the rest into major-mode-remap-alist. setopt, not setq:
;; the remapping is done by the option's setter, which setq skips.
;;
;; These replace the classic major modes, so the old modes' hooks and keymaps
;; do not apply in them -- python-mode-hook does not run in python-ts-mode,
;; though derived-mode-p still says python-mode. Hook the -ts-mode or the
;; -base-mode instead. Nothing here hooked the old ones, and flycheck covers
;; the ts modes as it did the old, except that bash-ts-mode has no posix
;; checker for #!/bin/sh scripts, only bash -n and shellcheck.
;;
;; A list rather than t, which would take every ts mode emacs knows, c-ts-mode
;; included: C stays on c-mode for lsp and its hooks, go and rust on their own
;; packages. YAML needs no entry -- emacs 31 already sends .yml to
;; yaml-ts-mode-maybe, which picks the ts mode once the grammar exists.
;;
;; sh-mode covers zsh files too, and they get remapped with the rest, but
;; bash-ts-mode sees the shell is zsh, starts no parser and keeps sh-mode's
;; highlighting, so .zshrc is no worse off.
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (require 'treesit nil t))
  (setopt treesit-enabled-modes
          (mapcar #'car
                  (seq-filter (lambda (mode)
                                (treesit-language-available-p (cdr mode)))
                              '((json-ts-mode . json)
                                (bash-ts-mode . bash)
                                (toml-ts-mode . toml)
                                (python-ts-mode . python))))))


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
