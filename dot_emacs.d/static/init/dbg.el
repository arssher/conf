;; GDB stuff

(require 'ars-gdb-stuff)
;; (load "my-gdb-stuff.el") ;; for debugging, no pun intended

;; I hate speedbar with gud-watch'ed vars popping up in separate frame.
;; sometimes this helps opening it in the same frame, but this question needs
;; to be addressed more seriously
(require 'sr-speedbar)

;; enable showing variable value on mouseover
(gud-tooltip-mode t)

;; show the source in neighbour window
(setq gdb-show-main t)
