;;; draw-kbd.el --- Draw my keybindings on a keyboard picture  -*- lexical-binding: t -*-

;; Lives in the chezmoi source tree (dot_emacs.d/kbd-diagram), which is listed
;; in .chezmoiignore, so it is version controlled but never applied to $HOME.
;;
;; Run by draw-kbd.sh in batch mode.  It loads init.el itself, rather than being
;; loaded after it, so that it can snapshot the stock global map first and tell
;; my own bindings from Emacs's.
;;
;; The output is one text file: a keyboard whose every key cell lists all the
;; modifier layers at once, then a board for the function-key row and one for
;; the navigation cluster, then a section per prefix key, then whatever is
;; left.  Nothing a binding can be is unrepresentable, which is the point --
;; the SVG template that ergoemacs-mode ships has room for four layers per key
;; and no boxes at all for the arrows, so it always left something out.
;;
;; The SVG sheets are still available behind draw-kbd.sh --svg; they look
;; better and say less.

;;; Code:

(require 'cl-lib)

(defvar draw-kbd-ergoemacs-src
  (or (getenv "ERGOEMACS_SRC") (expand-file-name "~/dev/ergoemacs-mode"))
  "Checkout of ergoemacs-mode; supplies kbd-ergo.svg and the label tables.")

(defvar draw-kbd-layout (or (getenv "KBD_LAYOUT") "us")
  "Keyboard layout to draw, see `ergoemacs-layouts.el'.")

(defvar draw-kbd-init-file
  (or (getenv "KBD_INIT") (expand-file-name "~/.emacs.d/init.el"))
  "The init file whose bindings get drawn.")

(defvar draw-kbd-output
  (or (getenv "KBD_OUT")
      ;; This file lives in the chezmoi source tree, not in ~/.emacs.d, so
      ;; default to its own directory rather than to `user-emacs-directory'.
      (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Directory the finished files are written to.")

(defvar draw-kbd-svg-p (and (getenv "KBD_SVG") t)
  "Also draw the ergoemacs SVG sheets.  Set by draw-kbd.sh --svg.")

(defvar draw-kbd-title "my bindings"
  "Shown in the header and in the SVG sheets' titles.")

(defvar draw-kbd-labels
  ;; Labels for commands ergoemacs-mode does not know about.  Anything longer
  ;; than `draw-kbd-ascii-label-width' is truncated, so keep them short.
  '((swiper "search")
    (ace-window "pick pane")
    (duplicate-line "dup line")
    (kill-current-buffer "x buffer")
    (smart-kill-whole-line "⌧ line")
    (revert-buffer-no-confirm "revert")
    (my-scroll-down-one "↓ 1 line")
    (my-scroll-up-one "↑ 1 line"))
  "Extra entries pushed in front of `ergoemacs-function-short-names'.")

;;; The text board

(defvar draw-kbd-ascii-label-width 9
  "Characters available to a command name inside a key cell.")

(defvar draw-kbd-ascii-layers
  '(("M " (meta)         nil)
    ("MS" (meta)         t)
    ("C " (control)      nil)
    ("CS" (control)      t)
    ("CM" (control meta) nil))
  "Rows inside each key cell: LABEL, MODIFIERS, and whether to shift.
Shifting means the character the key makes with Shift held, which is a
different character rather than a modifier -- except on a named key like
<left>, where there is no shifted character and `shift' is added to MODIFIERS
instead.  Add a row here and every board grows one.")

(defvar draw-kbd-ascii-fkey-layers
  '(("  " ()             nil)
    ("M " (meta)         nil)
    ("C " (control)      nil)
    ("CM" (control meta) nil))
  "Cell rows for the function keys, which are worth showing unmodified too.")

(defvar draw-kbd-ascii-nav-layers
  '(("  " ()             nil)
    ("S " ()             t)
    ("M " (meta)         nil)
    ("C " (control)      nil)
    ("CM" (control meta) nil))
  "Cell rows for the named keys, which are worth showing plain and shifted.")

(defvar draw-kbd-ascii-nav-keys
  '(?\s tab return backspace delete insert
    home end prior next up down left right print)
  "The keys drawn on the navigation board, in order.")

(defvar draw-kbd-ascii-key-names
  '((?\s . "SPC") (tab . "TAB") (return . "RET") (backspace . "⌫")
    (delete . "DEL") (insert . "Ins") (home . "Home") (end . "End")
    (prior . "PgUp") (next . "PgDn") (up . "↑") (down . "↓")
    (left . "←") (right . "→") (print . "Print"))
  "Cap legends for the named keys; anything missing prints as itself.")

(defvar draw-kbd-ascii-row-indent 3
  "Characters each keyboard row is indented past the one above it.")

;; ergoemacs-theme-engine.el still refers to this variable, but the defvar was
;; dropped in commit dc2e1a6, so the SVG sheets die without it.
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
Descends one level into prefix keymaps, which is as deep as the boards and the
listings ever reach."
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

;;; Labels

(defun draw-kbd--shorten (name width)
  "Squeeze command NAME down to WIDTH characters."
  (let ((s (replace-regexp-in-string
            (format "^%s-" (regexp-opt ergoemacs-theme-remove-prefixes t)) ""
            name)))
    (dolist (v ergoemacs-theme-replacements)
      (setq s (replace-regexp-in-string (nth 0 v) (nth 1 v) s)))
    (if (> (length s) width) (concat (substring s 0 (1- width)) "…") s)))

(defun draw-kbd--label (binding width)
  "A WIDTH-wide label for BINDING."
  (cond
   ((or (null binding) (eq binding 'ergoemacs-map-undefined)) "")
   ((keymapp binding) "Prefix")
   ((not (symbolp binding)) "λ")
   (t (let ((short (nth 1 (assq binding ergoemacs-function-short-names))))
        (if (and short (<= (string-width short) width))
            short
          (draw-kbd--shorten (symbol-name binding) width))))))

;;; Cells and boards

(defun draw-kbd--pad (s width)
  "Pad S with spaces to WIDTH columns, by display width, not character count."
  (concat s (make-string (max 0 (- width (string-width s))) ?\s)))

(defun draw-kbd--lookup (base mods)
  "The command BASE plus MODS is bound to in the global map, or nil."
  (when base
    (let ((key (ignore-errors
                 (vector (event-convert-list (append mods (list base)))))))
      (when key
        (let ((b (lookup-key (current-global-map) key)))
          (and b (not (integerp b)) (cons key b)))))))

(defun draw-kbd--cell (head base shifted layers width seen)
  "Lines for one key cell, and record every key it shows in SEEN.
HEAD is the cap legend, BASE the unmodified event, SHIFTED the event Shift
makes of it (nil for a named key, where `shift' becomes a modifier instead)."
  (cons
   (draw-kbd--pad (concat " " head) width)
   (mapcar
    (lambda (layer)
      (cl-destructuring-bind (tag mods shift-p) layer
        (let* ((b (if (not shift-p)
                      (draw-kbd--lookup base mods)
                    (if shifted
                        (draw-kbd--lookup shifted mods)
                      (draw-kbd--lookup base (cons 'shift mods))))))
          (when b (puthash (car b) t seen))
          (draw-kbd--pad
           (format "%s %s" tag (draw-kbd--label (cdr-safe b)
                                                draw-kbd-ascii-label-width))
           width))))
    layers)))

(defun draw-kbd--board (cells width indent)
  "Join CELLS, each a list of equal-width lines, into a boxed row."
  (when cells
    (let* ((pad (make-string indent ?\s))
           (bar (make-string width ?─))
           (rows (length (car cells))))
      (append
       (list (concat pad "┌" (mapconcat (lambda (_) bar) cells "┬") "┐"))
       (cl-loop for i below rows
                collect (concat pad "│"
                                (mapconcat (lambda (c) (nth i c)) cells "│")
                                "│"))
       (list (concat pad "└" (mapconcat (lambda (_) bar) cells "┴") "┘"))))))

(defun draw-kbd--char-cells (layout row width seen)
  "Cells for ROW of the layout grid, skipping the empty padding slots."
  (cl-loop for col below 15
           for i = (+ (* row 15) col)
           for c = (nth i layout)
           for s = (nth (+ i 60) layout)
           unless (or (null c) (string= c ""))
           collect (draw-kbd--cell c (string-to-char c)
                                   (and s (not (string= s ""))
                                        (string-to-char s))
                                   draw-kbd-ascii-layers width seen)))

(defun draw-kbd--key-name (k)
  "The cap legend for named key K."
  (or (cdr (assoc k draw-kbd-ascii-key-names))
      (if (and (symbolp k) (string-match-p "\\`f[0-9]+\\'" (symbol-name k)))
          (upcase (symbol-name k))
        (format "%s" k))))

(defun draw-kbd--named-cells (keys layers width seen)
  "Cells for named KEYS such as <left>, which have no shifted character."
  (mapcar (lambda (k)
            (draw-kbd--cell (draw-kbd--key-name k) k nil layers width seen))
          keys))

;;; The file

(defun draw-kbd--sections (seen)
  "Everything of mine that no board showed, grouped by prefix key."
  (let ((stock (let ((h (make-hash-table :test 'equal)))
                 (dolist (c draw-kbd--stock) (puthash (car c) (cdr c) h))
                 h))
        (groups (make-hash-table :test 'equal)))
    (dolist (c (draw-kbd--collect (current-global-map)))
      (let ((key (draw-kbd--normalize (car c))))
        (when (and (draw-kbd--keyboard-p key)
                   (not (eq (gethash (car c) stock) (cdr c)))
                   (not (gethash key seen)))
          (push (cons (key-description key) (cdr c))
                (gethash (if (> (length key) 1)
                             (key-description (vector (aref key 0)))
                           "other keys")
                         groups)))))
    groups))

(defun draw-kbd--normalize (key)
  "Fold an ESC-prefixed sequence in KEY back into a single meta event.
`global-set-key' on M-x stores it under the ESC prefix, so without this every
Alt binding reads back as a two-key sequence."
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

(defun draw-kbd-ascii (layout lay)
  "Write the one-file text diagram.  Return its path."
  (let* ((width (+ 3 draw-kbd-ascii-label-width))
         (seen (make-hash-table :test 'equal))
         (file (expand-file-name (concat lay ".txt") draw-kbd-output))
         (fkeys (cl-loop for n from 1 to 12
                         collect (intern (format "f%d" n))))
         body)
    (setq body
          (append
           (draw-kbd--board (draw-kbd--named-cells
                             fkeys draw-kbd-ascii-fkey-layers width seen)
                            width 0)
           (list "")
           (cl-loop for row below 4
                    append (draw-kbd--board
                            (draw-kbd--char-cells layout row width seen)
                            width (* row draw-kbd-ascii-row-indent)))
           (list "")
           (draw-kbd--board (draw-kbd--named-cells
                             draw-kbd-ascii-nav-keys
                             draw-kbd-ascii-nav-layers width seen)
                            width 0)))
    (let ((groups (draw-kbd--sections seen))
          (shown (hash-table-count seen)))
      (with-temp-file file
        (insert (format "The global keymap after loading %s\n"
                        (abbreviate-file-name draw-kbd-init-file))
                (format "%s layout, \"%s\", generated %s\n\n"
                        lay draw-kbd-title (format-time-string "%Y-%m-%d"))
                "Cell rows: "
                (mapconcat (lambda (l)
                             (format "%s = %s" (string-trim (nth 0 l))
                                     (if (nth 1 l)
                                         (concat (mapconcat #'symbol-name (nth 1 l) "+")
                                                 (if (nth 2 l) "+shift" ""))
                                       "plain")))
                           draw-kbd-ascii-layers "  ")
                "\n\n")
        (dolist (line body) (insert line "\n"))
        (let (names)
          (maphash (lambda (k _v) (push k names)) groups)
          (when names
            (insert "\nNot on a board above.  Only my own bindings are listed"
                    " here, not Emacs's:\n"))
          (dolist (name (sort names #'string<))
            (let ((rows (gethash name groups)))
              (insert (format "\n%s (%d)\n" name (length rows)))
              (dolist (row (sort rows (lambda (x y) (string< (car x) (car y)))))
                (insert (format "  %-22s %s\n" (car row) (cdr row)))))))
        (goto-char (point-max))
        (insert (format "\n%d bound keys are drawn on the boards above.\n" shown)))
      (message "draw-kbd: wrote %s" file))
    file))

;;; The SVG sheets, kept for when a picture is wanted

(defvar draw-kbd-extra-layers
  '((nil     control meta)
    (meta    control meta)
    (control hyper))
  "Modifiers the extra SVG sheet shows, keyed by the layer the template means.")

(defvar draw-kbd-extra-name "Ctrl+Alt layer"
  "Title suffix for the extra SVG sheet.")

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

(defun draw-kbd--remap (elt lay)
  "Re-point one parsed template slot ELT at the `draw-kbd-extra-layers' modifiers."
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

(defun draw-kbd--write-svg (elts layout lay file)
  "Write the parsed template ELTS out to FILE for LAYOUT, named LAY."
  (with-temp-file file
    (dolist (w elts)
      (cond
       ((stringp w) (insert w))
       ((and (consp w) (eq (car w) :text))
        (insert ">" (ergoemacs-translate--svg-quote (cdr w)) "<"))
       (t (insert ">" (ergoemacs-theme--svg-elt w layout lay) "<"))))))

(defun draw-kbd-svg ()
  "Write the ergoemacs SVG sheets.  Return their paths."
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
         (layout (symbol-value (ergoemacs :layout draw-kbd-layout)))
         (final (expand-file-name (concat draw-kbd-layout ".svg")
                                  draw-kbd-output))
         (extra (expand-file-name
                 (concat draw-kbd-layout draw-kbd-extra-suffix ".svg")
                 draw-kbd-output)))
    (unless (and generated (file-exists-p generated))
      (error "draw-kbd: ergoemacs-theme--svg produced nothing"))
    (copy-file generated final t)
    (delete-directory user-emacs-directory t)
    (message "draw-kbd: wrote %s" final)
    ;; `ergoemacs-theme--svg' leaves the parsed template in the variable of the
    ;; same name, so the extra sheet costs only a second pass over that list.
    (draw-kbd--write-svg (mapcar (lambda (w)
                                   (if (stringp w) w (draw-kbd--remap w draw-kbd-layout)))
                                 ergoemacs-theme--svg)
                         layout draw-kbd-layout extra)
    (message "draw-kbd: wrote %s" extra)
    (list final extra)))

(defun draw-kbd ()
  "Draw the current global keymap.  Return the files written."
  (make-directory draw-kbd-output t)
  (let* ((ergoemacs-function-short-names
          (append draw-kbd-labels ergoemacs-function-short-names))
         (layout (symbol-value (ergoemacs :layout draw-kbd-layout))))
    (cons (draw-kbd-ascii layout draw-kbd-layout)
          (and draw-kbd-svg-p (draw-kbd-svg)))))

(draw-kbd)

;;; draw-kbd.el ends here
