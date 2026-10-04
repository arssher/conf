;;; draw-kbd.el --- Draw my keybindings on a keyboard picture  -*- lexical-binding: t -*-

;; Lives in the chezmoi source tree (dot_emacs.d/kbd-diagram), which is listed
;; in .chezmoiignore, so it is version controlled but never applied to $HOME.
;; Loaded *after* init.el by draw-kbd.sh, in batch mode.  It reuses the SVG
;; template and the label tables that ship with ergoemacs-mode, but it never
;; turns `ergoemacs-mode' on: `ergoemacs-theme--svg' falls back to
;; `(current-global-map)', which at this point holds exactly the bindings my
;; init.el has made.
;;
;; The template only has room for four layers per key:
;;   Alt+key, Alt+Shift+key, Ctrl+key, Ctrl+Shift+key
;; Prefix sequences beyond their first key are not drawn at all.  The layers
;; themselves are only a convention of the template, though -- each slot ends up
;; in `ergoemacs-theme--svg-elt' as (INDEX . MODIFIERS) and is resolved with
;; `event-convert-list', which takes any modifiers.  So a second sheet is made
;; by re-pointing the slots at other modifiers; see `draw-kbd-extra-layers'.

;;; Code:

(require 'cl-lib)

(defvar draw-kbd-ergoemacs-src
  (or (getenv "ERGOEMACS_SRC") (expand-file-name "~/dev/ergoemacs-mode"))
  "Checkout of ergoemacs-mode; supplies kbd-ergo.svg and the label tables.")

(defvar draw-kbd-layout (or (getenv "KBD_LAYOUT") "us")
  "Keyboard layout to draw, see `ergoemacs-layouts.el'.")

(defvar draw-kbd-output
  (or (getenv "KBD_OUT")
      ;; This file lives in the chezmoi source tree, not in ~/.emacs.d, so
      ;; default to its own directory rather than to `user-emacs-directory'.
      (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Directory the finished <layout>.svg is written to.")

(defvar draw-kbd-title "my bindings"
  "Shown next to the layout name at the top of the picture.")

(defvar draw-kbd-labels
  ;; Labels for commands ergoemacs-mode does not know about.  Keep them under
  ;; ~10 characters or they get truncated to fit the key.  Everything else
  ;; falls back to the command name with the common prefixes stripped.
  '((swiper "search")
    (ace-window "pick pane")
    (duplicate-line "dup line")
    (kill-current-buffer "x buffer")
    (smart-kill-whole-line "⌧ line")
    (revert-buffer-no-confirm "revert")
    (my-scroll-down-one "↓ 1 line")
    (my-scroll-up-one "↑ 1 line"))
  "Extra entries pushed in front of `ergoemacs-function-short-names'.")

(defvar draw-kbd-extra-layers
  '((nil     control meta)
    (meta    control meta)
    (control hyper))
  "Modifiers the extra sheet shows, keyed by the layer the template means.
The template's own four layers are nil (the plain function-key row), `meta'
(the two Alt rows) and `control' (the two Ctrl rows); Shift is carried by the
character, not by a modifier, so each entry covers both of its rows.  Set this
to nil to skip the extra sheet.")

(defvar draw-kbd-extra-name "Ctrl+Alt layer"
  "Title suffix for the extra sheet.")

(defvar draw-kbd-extra-suffix "-ctrl-meta"
  "Appended to the layout name to make the extra sheet's file name.")

(defvar draw-kbd-extra-legend
  '((meta          . "Ctrl+Alt+ == control meta")
    (meta-shift    . "Ctrl+Alt+⇧Shift+ == control meta shift")
    (control       . "Hyper+ == hyper")
    (control-shift . "Hyper+⇧Shift+ == hyper shift"))
  "Legend lines for the extra sheet, replacing the template's Alt/Ctrl ones.
These are written out verbatim: the legend slots are too narrow for
`ergoemacs-theme--svg-elt', which truncates anything it formats to 10
characters.")

;; ergoemacs-theme-engine.el still refers to this variable, but the defvar was
;; dropped in commit dc2e1a6, so generation dies without it.
(defvar ergoemacs-M-O-binding nil)

(defun draw-kbd-inhibit-state-writes ()
  "Keep this throwaway batch session from rewriting ~/.emacs.d state files.
Both `savehist' and `recentf' write their file from `kill-emacs-hook', which
batch Emacs does run on exit.  The hooks are detached by hand rather than by
toggling the modes off, because `(recentf-mode -1)' itself calls
`recentf-save-list'."
  (remove-hook 'kill-emacs-hook #'savehist-autosave)
  (remove-hook 'kill-emacs-hook #'recentf-save-list)
  (when (bound-and-true-p savehist-timer)
    (cancel-timer savehist-timer)
    (setq savehist-timer nil))
  ;; Belt and braces, in case anything else still reaches for the writers.
  (dolist (fn '(savehist-save savehist-autosave recentf-save-list))
    (when (fboundp fn)
      (advice-add fn :override #'ignore))))

(draw-kbd-inhibit-state-writes)

(add-to-list 'load-path draw-kbd-ergoemacs-src)
(require 'ergoemacs-mode)

(defun draw-kbd--remap (elt lay)
  "Re-point one parsed template slot ELT at the `draw-kbd-extra-layers' modifiers.
A (:text . STRING) cons means \"write STRING here verbatim\"; everything else is
left for `ergoemacs-theme--svg-elt'."
  (cond
   ((eq elt 'title)
    (cons :text (format "%s (%s) %s" lay draw-kbd-title draw-kbd-extra-name)))
   ((and (symbolp elt) (assq elt draw-kbd-extra-legend))
    (cons :text (cdr (assq elt draw-kbd-extra-legend))))
   ((consp elt)
    (let* ((mods (cdr elt))
           (layer (cond ((memq 'control mods) 'control)
                        ((memq 'meta mods) 'meta)))
           (to (cdr (assq layer draw-kbd-extra-layers))))
      (if to
          (cons (car elt) (append to (and (memq 'shift mods) '(shift))))
        elt)))
   (t elt)))

(defun draw-kbd--write (elts layout lay file)
  "Write the parsed template ELTS out to FILE for LAYOUT, named LAY."
  (with-temp-file file
    (dolist (w elts)
      (cond
       ((stringp w) (insert w))
       ((and (consp w) (eq (car w) :text))
        (insert ">" (ergoemacs-translate--svg-quote (cdr w)) "<"))
       (t (insert ">" (ergoemacs-theme--svg-elt w layout lay) "<"))))))

(defun draw-kbd-extra (layout lay)
  "Write the extra-layer sheet, reusing the template `draw-kbd' already parsed.
Return its path, or nil when `draw-kbd-extra-layers' is empty."
  (when draw-kbd-extra-layers
    (let ((file (expand-file-name (concat lay draw-kbd-extra-suffix ".svg")
                                  draw-kbd-output)))
      (draw-kbd--write (mapcar (lambda (w)
                                 (if (stringp w) w (draw-kbd--remap w lay)))
                               ergoemacs-theme--svg)
                       layout lay file)
      (message "draw-kbd: wrote %s" file)
      file)))

(defun draw-kbd ()
  "Render the current global keymap onto the ergoemacs keyboard template.
Return the path of the SVG written into `draw-kbd-output'."
  (let* ((ergoemacs-theme draw-kbd-title)
         (ergoemacs-keyboard-layout draw-kbd-layout)
         (ergoemacs-function-short-names
          (append draw-kbd-labels ergoemacs-function-short-names))
         ;; `ergoemacs-theme--svg' caches by file name under
         ;; `user-emacs-directory'; a throwaway directory keeps every run fresh
         ;; and keeps ergoemacs-extras/ out of ~/.emacs.d.
         (user-emacs-directory (file-name-as-directory
                                (make-temp-file "draw-kbd" t)))
         (generated (car (ergoemacs-theme--svg draw-kbd-layout)))
         (final (expand-file-name (concat draw-kbd-layout ".svg")
                                  draw-kbd-output)))
    (unless (and generated (file-exists-p generated))
      (error "draw-kbd: ergoemacs-theme--svg produced nothing"))
    (make-directory draw-kbd-output t)
    (copy-file generated final t)
    (delete-directory user-emacs-directory t)
    (message "draw-kbd: wrote %s" final)
    ;; `ergoemacs-theme--svg' leaves the parsed template in the variable of the
    ;; same name, so the extra sheet costs only a second pass over that list.
    (cons final (draw-kbd-extra (symbol-value (ergoemacs :layout draw-kbd-layout))
                                draw-kbd-layout))))

(draw-kbd)

;;; draw-kbd.el ends here
