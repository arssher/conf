(defun ggtags-recreate ()
  "Delete and create ggtags again. Doesn't work."
  (interactive)
  (ggtags-create-tags ggtags-project-root))

(message "ars-navigation-and-completion loaded")
(provide 'ars-navigation-and-completion)
