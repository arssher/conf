;;; draw-kbd.el --- Draw my keybindings on a keyboard picture  -*- lexical-binding: t -*-

;; Lives in the chezmoi source tree (dot_emacs.d/kbd-diagram), which is listed
;; in .chezmoiignore, so it is version controlled but never applied to $HOME.
;; Run by draw-kbd.sh in batch mode.  It loads init.el itself, rather than being
;; loaded after it, so that it can snapshot the stock global map first and tell
;; my own bindings from Emacs's -- see `draw-kbd-not-drawn'.  It reuses the SVG
;; template and the label tables that ship with ergoemacs-mode, but it never
;; turns `ergoemacs-mode' on: `ergoemacs-theme--svg' falls back to
;; `(current-global-map)', which by then holds exactly the bindings init.el
;; has made.
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

(defvar draw-kbd-init-file
  (or (getenv "KBD_INIT") (expand-file-name "~/.emacs.d/init.el"))
  "The init file whose bindings get drawn.")

(defvar draw-kbd-not-drawn-suffix "-not-drawn.txt"
  "Appended to the layout name to make the leftovers listing's file name.")

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

(defun draw-kbd--collect (map)
  "Every command binding in MAP, as an alist of (KEY-VECTOR . COMMAND).
Descends one level into prefix keymaps, which is as deep as the sheets and the
listing ever reach."
  (let (acc)
    (cl-labels ((walk (m prefix)
                  (map-keymap
                   (lambda (ev def)
                     (let ((key (vconcat prefix (vector ev))))
                       (cond ((and (keymapp def) (< (length key) 2)) (walk def key))
                             ((keymapp def) nil)
                             ((commandp def) (push (cons key def) acc)))))
                   m)))
      (walk map []))
    acc))

;; Must happen before init.el runs: everything it adds to this is "mine".
(defvar draw-kbd--stock (draw-kbd--collect (current-global-map))
  "The global map as Emacs itself ships it.")

(load draw-kbd-init-file)

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

(defun draw-kbd--normalize (key)
  "Fold an ESC-prefixed sequence in KEY back into a single meta event.
`global-set-key' on M-x stores it under the ESC prefix, so without this every
Alt binding looks like a two-key sequence."
  (if (and (= (length key) 2) (eq (aref key 0) 27))
      (let ((ev (aref key 1)))
        (condition-case nil
            (vector (event-convert-list
                     (append (cons 'meta (event-modifiers ev))
                             (list (event-basic-type ev)))))
          (error key)))
    key))

(defun draw-kbd--keyboard-p (key)
  "Non-nil when KEY is typed rather than clicked or picked from a menu."
  (not (string-match-p
        "mouse\\|wheel\\|menu-bar\\|tool-bar\\|tab-bar\\|divider\\|edge\\|corner\\|scroll-bar\\|drag-n-drop\\|touch\\|pinch\\|language-change"
        (key-description key))))

(defun draw-kbd--elt-key (elt layout)
  "The key vector template slot ELT resolves to, or nil if the slot is unused.
Mirrors how `ergoemacs-theme--svg-elt' builds its lookup key."
  (when (and (consp elt) (not (eq (car elt) :text))
             (or (stringp (car elt)) (integerp (car elt))))
    (let ((k (if (stringp (car elt))
                 (if (string= (car elt) "SPC") " " "f#")
               (nth (car elt) layout))))
      (unless (or (null k) (string= k ""))
        (setq k (if (string= k "f#")
                    (aref (read-kbd-macro (concat "<" (downcase (car elt)) ">")) 0)
                  (string-to-char k)))
        (condition-case nil
            (vector (event-convert-list (append (cdr elt) (list k))))
          (error nil))))))

(defun draw-kbd--sheet-coverage (layout lay)
  "What the sheets cover, as (KEYS . BOXED-CHARS).
KEYS is a hash of every key vector that has a slot on either sheet.  BOXED-CHARS
is a hash of the characters that have a key box at all, which is what separates
\"no slot for that modifier\" from \"no box for that key\"."
  (let ((keys (make-hash-table :test 'equal))
        (boxed (make-hash-table :test 'equal)))
    (dolist (elts (list ergoemacs-theme--svg
                        (and draw-kbd-extra-layers
                             (mapcar (lambda (w)
                                       (if (stringp w) w (draw-kbd--remap w lay)))
                                     ergoemacs-theme--svg))))
      (dolist (elt elts)
        (unless (stringp elt)
          (let ((k (draw-kbd--elt-key elt layout)))
            (when k (puthash k t keys)))
          ;; An integer slot is the key's own character, i.e. it has a box.
          (when (integerp elt)
            (let ((c (nth elt layout)))
              (unless (or (null c) (string= c ""))
                (puthash (string-to-char c) t boxed)))))))
    (puthash ?\s t boxed)
    (cons keys boxed)))

(defun draw-kbd-not-drawn (layout lay)
  "Write the listing of my bindings that no sheet shows.  Return its path."
  (let* ((cov (draw-kbd--sheet-coverage layout lay))
         (drawn (car cov))
         (boxed (cdr cov))
         (stock (let ((h (make-hash-table :test 'equal)))
                  (dolist (c draw-kbd--stock) (puthash (car c) (cdr c) h))
                  h))
         (file (expand-file-name (concat lay draw-kbd-not-drawn-suffix)
                                 draw-kbd-output))
         (buckets '(("Under a prefix key" . prefix)
                    ("No slot for that modifier combination" . mods)
                    ("No box for that key" . box)))
         (found (make-hash-table :test 'eq))
         (drawn-count 0))
    (dolist (c (draw-kbd--collect (current-global-map)))
      (let ((key (draw-kbd--normalize (car c))))
        (when (and (draw-kbd--keyboard-p key)
                   (not (eq (gethash (car c) stock) (cdr c))))
          (if (gethash key drawn)
              (setq drawn-count (1+ drawn-count))
            (push (cons (key-description key) (cdr c))
                  (gethash (cond ((> (length key) 1) 'prefix)
                                 ((gethash (event-basic-type (aref key 0)) boxed) 'mods)
                                 (t 'box))
                           found))))))
    (with-temp-file file
      (insert (format "Bindings of %s that the keyboard sheets do not show\n"
                      (abbreviate-file-name draw-kbd-init-file))
              (format "%s layout, \"%s\", generated %s\n\n"
                      lay draw-kbd-title (format-time-string "%Y-%m-%d"))
              (format "  drawn      %3d\n" drawn-count))
      (let ((total 0))
        (maphash (lambda (_k v) (setq total (+ total (length v)))) found)
        (insert (format "  not drawn  %3d\n" total)))
      (dolist (b buckets)
        (let ((rows (gethash (cdr b) found)))
          (when rows
            (insert (format "\n%s (%d)\n" (car b) (length rows)))
            (dolist (row (sort rows (lambda (x y) (string< (car x) (car y)))))
              (insert (format "  %-22s %s\n" (car row) (cdr row))))))))
    (message "draw-kbd: wrote %s" file)
    file))

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
    (let ((layout (symbol-value (ergoemacs :layout draw-kbd-layout))))
      (delq nil (list final
                      (draw-kbd-extra layout draw-kbd-layout)
                      (draw-kbd-not-drawn layout draw-kbd-layout))))))

(draw-kbd)

;;; draw-kbd.el ends here
