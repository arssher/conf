;; Navigation/autocompletion

;; Autocompletion (non-code)
;;
;; Short review:
;; Helm is a big buggy monster. ido seems to be good old stuff. ivy -- modern,
;; small and efficient package. Let's try it.

;; toggle it on and off to ivy-mode command
(ivy-mode 1) ; enable ivy completion everywhere
;; add recent visited files (if recentf is on) and bookmarks
;; to list of buffers
(setq ivy-use-virtual-buffers t)
;; display index and total files number in the ivy buffer
(setq ivy-count-format "(%d/%d) ")
(setq ivy-wrap t)
