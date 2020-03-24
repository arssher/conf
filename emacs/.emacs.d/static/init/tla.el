(defun untabify-tla ()
  "Replace tabs with spaces in .*.tla file"
  (if (string-match-p ".tla\\'" (buffer-name))
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-tla)
