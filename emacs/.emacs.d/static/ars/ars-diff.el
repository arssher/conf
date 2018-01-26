;; Current implementation of diff-hunk-text is broken: it doesn't handle lines
;; containing only newlines in patches: it removes the newline and then goes one
;; line forward more, skipping the next one.
(defun diff-hunk-text-fixed (hunk destp char-offset)
  "Return the literal source text from HUNK as (TEXT . OFFSET).
If DESTP is nil, TEXT is the source, otherwise the destination text.
CHAR-OFFSET is a char-offset in HUNK, and OFFSET is the corresponding
char-offset in TEXT."
  (with-temp-buffer
    (insert hunk)
    (goto-char (point-min))
    (let ((src-pos nil)
	  (dst-pos nil)
	  (divider-pos nil)
	  (num-pfx-chars 2))
      ;; Set the following variables:
      ;;  SRC-POS     buffer pos of the source part of the hunk or nil if none
      ;;  DST-POS     buffer pos of the destination part of the hunk or nil
      ;;  DIVIDER-POS buffer pos of any divider line separating the src & dst
      ;;  NUM-PFX-CHARS  number of line-prefix characters used by this format"
      (cond ((looking-at "^@@")
	     ;; unified diff
	     (setq num-pfx-chars 1)
	     (forward-line 1)
	     (setq src-pos (point) dst-pos (point)))
	    ((looking-at "^\\*\\*")
	     ;; context diff
	     (forward-line 2)
	     (setq src-pos (point))
	     (re-search-forward diff-context-mid-hunk-header-re nil t)
	     (forward-line 0)
	     (setq divider-pos (point))
	     (forward-line 1)
	     (setq dst-pos (point)))
	    ((looking-at "^[0-9]+a[0-9,]+$")
	     ;; normal diff, insert
	     (forward-line 1)
	     (setq dst-pos (point)))
	    ((looking-at "^[0-9,]+d[0-9]+$")
	     ;; normal diff, delete
	     (forward-line 1)
	     (setq src-pos (point)))
	    ((looking-at "^[0-9,]+c[0-9,]+$")
	     ;; normal diff, change
	     (forward-line 1)
	     (setq src-pos (point))
	     (re-search-forward "^---$" nil t)
	     (forward-line 0)
	     (setq divider-pos (point))
	     (forward-line 1)
	     (setq dst-pos (point)))
	    (t
	     (error "Unknown diff hunk type")))

      (if (if destp (null dst-pos) (null src-pos))
	  ;; Implied empty text
	  (if char-offset '("" . 0) "")

	;; For context diffs, either side can be empty, (if there's only
	;; added or only removed text).  We should then use the other side.
	(cond ((equal src-pos divider-pos) (setq src-pos dst-pos))
	      ((equal dst-pos (point-max)) (setq dst-pos src-pos)))

	(when char-offset (goto-char (+ (point-min) char-offset)))

	;; Get rid of anything except the desired text.
	(save-excursion
	  ;; Delete unused text region
	  (let ((keep (if destp dst-pos src-pos)))
	    (when (and divider-pos (> divider-pos keep))
	      (delete-region divider-pos (point-max)))
	    (delete-region (point-min) keep))
	  ;; Remove line-prefix characters, and unneeded lines (unified diffs).
	  (let ((kill-char (if destp ?- ?+)))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (cond ((eq (string-width (thing-at-point 'line 1)) 0) (forward-line 1))
		    ((eq (char-after) kill-char)
		     (delete-region (point) (progn (forward-line 1) (point))))
		    (t (progn (delete-char num-pfx-chars) (forward-line 1)))))))

	(let ((text (buffer-substring-no-properties (point-min) (point-max))))
	  (if char-offset (cons text (- (point) (point-min))) text))))))

(message "ars-diff loaded")
(provide 'ars-diff)
