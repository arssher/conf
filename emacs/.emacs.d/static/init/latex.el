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
