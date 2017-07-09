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
(projectile-mode 1)
(setq projectile-completion-system 'ivy)
