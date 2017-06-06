;; Syntax checking and other checks on the fly

;; enable flychecker
(global-flycheck-mode)
;; disable it for latex, see above
(setq flycheck-global-modes '(not LaTeX-mode latex-mode))

;; Here we can disable some checkers.
;; It is a buffer-local variable, but using setq-default I set it's value
;; globally.
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck))
;; set clang executable path
(setq flycheck-c/c++-clang-executable "clang-3.8")
;; Probably you will also want to put in the .dir-locals.el file of the root of
;; you project something like
;; ((c-mode
;;   (flycheck-clang-include-path . (
;; 				  "/home/ars/postgres/postgresql/src/include"
;; 				  "/home/ars/tmp/tmp/postgresql_build/src/include"
;; 				  "/home/ars/postgres/reverse_executor_test_extension/src/include"
;; 				  ))
;;   (flycheck-clang-args . (
;; 			  "-Wignored-attributes"
;; 			  ))
;;   (flycheck-gcc-include-path . (
;; 				  "/home/ars/postgres/postgresql/src/include"
;; 				  "/home/ars/tmp/tmp/postgresql_build/src/include"
;; 				  "/home/ars/postgres/reverse_executor_test_extension/src/include"
;; 				  ))
;;  ;; for some reason headers don't include postgres.h which leads to a lot of
;;  ;; errors in them
;;  (flycheck-gcc-args . ("-include" "postgres.h"))
;; ))

;; Force emacs to take local vars needed for flycheck as safe with any
;; value; probably it is not very secure, but much more handy
(put 'flycheck-clang-include-path 'safe-local-variable (lambda (xx) t))
(put 'flycheck-clang-args 'safe-local-variable (lambda (xx) t))
(put 'flycheck-gcc-include-path 'safe-local-variable (lambda (xx) t))
(put 'flycheck-gcc-args 'safe-local-variable (lambda (xx) t))

;; highlight changed lines
(require 'diff-hl-flydiff)
(global-diff-hl-mode t)
(diff-hl-flydiff-mode t) ; even when they are not saved yet
