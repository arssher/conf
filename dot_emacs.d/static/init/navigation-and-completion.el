;;; -*- lexical-binding: t -*-
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


;; Keybinding discovery
;;
;; C-h after a prefix offers what is bound under it through completing-read,
;; so ivy is there and a couple of letters narrow to the command instead of
;; paging a popup or scrolling a *Help* buffer. prefix-help-command is the
;; built-in seam for this; describe-prefix-bindings is what it holds by
;; default. See init.el for the C-h l listings, which are embark's too.
;;
;; which-key, in core since 30, is the other way to fill the same seam: a
;; popup on an idle timer, paged with C-h. Tried it, preferred typing. It
;; comes back with (which-key-mode 1), which would have to go above this line,
;; since turning it on claims prefix-help-command for itself.
;;
;; embark-prefix-help-command is autoloaded, so naming it is all it takes --
;; embark stays unloaded until the first C-h.
(setq prefix-help-command #'embark-prefix-help-command)

;; And offer the same thing unasked once a prefix has been held for
;; embark-auto-prefix-help-delay, two seconds by default. Unlike which-key's
;; popup this is a real completing-read session, so it takes the input rather
;; than hinting from the side -- raise the delay if it starts arriving while
;; you are still deciding.
;;
;; The mode sets prefix-help-command itself, which makes the line above look
;; redundant. It is not: that value becomes the backup this mode restores when
;; it is turned off, so switching the timer off leaves C-h on embark instead of
;; dropping back to describe-prefix-bindings.
(embark-auto-prefix-help-mode 1)
