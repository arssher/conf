;; Ediff stuff

;; Keep control buffer in the same frame as diff buffers
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; side-by-side representation, yes, that's what they call "horizontal split"
(setq ediff-split-window-function 'split-window-horizontally)
;; Don't ask confirmation on exit, see
;;http://emacs.stackexchange.com/questions/9322/how-can-i-quit-ediff-immediately-without-having-to-type-y
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))
(advice-add 'ediff-quit :around #'disable-y-or-n-p)

;; ediff colours with my wombat theme are too bright
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-background ediff-fine-diff-face-B "#277227") ; dark green
            (set-face-background ediff-fine-diff-face-A "#681218") ; dark red
	    (set-face-background ediff-even-diff-face-A "#293035")
	    (set-face-background ediff-even-diff-face-B "#293035")
	    (set-face-background ediff-odd-diff-face-A "#161c30")
	    (set-face-background ediff-odd-diff-face-B "#161c30")
	    ))

;; A hacky way to restore window configuration after exiting from ediff
;; It works badly with magit diff though, so I will turn it off for now...
;; (winner-mode 1)
;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; overwrite buggy diff-hunk-text
(require 'ars-diff)
(advice-add 'diff-hunk-text :override 'diff-hunk-text-fixed)
