;; All about going to definition, autocompletion, etc
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

;; Let's try Cscope with xcscope.el for now.
;; (require 'xcscope)
;; (cscope-setup)

;; Now let's try gnu global
(require 'ars-navigation-and-completion)
;; Remember that you need GTAGSLIBPATH env var to search external projects,
;; you can do that in ~/.emacs-local.el
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))



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
