;;; draw-kbd.el --- Draw my keybindings on a keyboard picture  -*- lexical-binding: t -*-

;; Lives in the chezmoi source tree (dot_emacs.d/kbd-diagram), which is listed
;; in .chezmoiignore, so it is version controlled but never applied to $HOME.
;;
;; Run by draw-kbd.sh in batch mode.  It loads init.el itself, rather than being
;; loaded after it, so that it can snapshot the stock global map first and tell
;; my own bindings from Emacs's.
;;
;; Everything lands in one picture: a keyboard whose every key cell lists all
;; the modifier layers at once, a board for the function keys, a board for the
;; navigation cluster, and a section per prefix key for what no board can hold
;; -- a two-key sequence not being a key.
;;
;; There are three backends over one model.  `draw-kbd--cell-data' resolves a
;; key's layers into (TAG . LABEL) pairs and is all any of them needs:
;;
;;   draw-kbd-svg    the default; writes <layout>.svg
;;   draw-kbd-ascii  --txt; the same boards in box-drawing characters
;;   draw-kbd-ergo   --ergo; ergoemacs-mode's own SVG template, which looks
;;                   better and says less -- four layers per key, no boxes for
;;                   the arrows

;;; Code:

(require 'cl-lib)

(defvar draw-kbd-ergoemacs-src
  (or (getenv "ERGOEMACS_SRC") (expand-file-name "~/dev/ergoemacs-mode"))
  "Checkout of ergoemacs-mode; supplies the layout vectors and label tables.")

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

(defvar draw-kbd-txt-p (and (getenv "KBD_TXT") t)
  "Also write the text board.  Set by draw-kbd.sh --txt.")

(defvar draw-kbd-ergo-p (and (getenv "KBD_ERGO") t)
  "Also draw ergoemacs-mode's own sheets.  Set by draw-kbd.sh --ergo.")

(defvar draw-kbd-title "my bindings"
  "Shown in the picture's heading.")

(defvar draw-kbd-labels
  ;; Labels for commands ergoemacs-mode does not know about.  Anything longer
  ;; than the label width is truncated, so keep them short.
  '((swiper "search")
    (ace-window "pick pane")
    (duplicate-line "dup line")
    (kill-current-buffer "x buffer")
    (smart-kill-whole-line "⌧ line")
    (revert-buffer-no-confirm "revert")
    (my-scroll-down-one "↓ 1 line")
    (my-scroll-up-one "↑ 1 line"))
  "Extra entries pushed in front of `ergoemacs-function-short-names'.")

;;; What every board shows

(defvar draw-kbd-layers
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

(defvar draw-kbd-fkey-layers
  '(("  " ()             nil)
    ("M " (meta)         nil)
    ("C " (control)      nil)
    ("CM" (control meta) nil))
  "Cell rows for the function keys, which are worth showing unmodified too.")

(defvar draw-kbd-nav-layers
  '(("  " ()             nil)
    ("S " ()             t)
    ("M " (meta)         nil)
    ("C " (control)      nil)
    ("CM" (control meta) nil))
  "Cell rows for the named keys, which are worth showing plain and shifted.")

(defvar draw-kbd-nav-keys
  '(?\s tab return backspace delete insert
    home end prior next up down left right print)
  "The keys drawn on the navigation board, in order.")

(defvar draw-kbd-key-names
  '((?\s . "SPC") (tab . "TAB") (return . "RET") (backspace . "⌫")
    (delete . "DEL") (insert . "Ins") (home . "Home") (end . "End")
    (prior . "PgUp") (next . "PgDn") (up . "↑") (down . "↓")
    (left . "←") (right . "→") (print . "Print"))
  "Cap legends for the named keys; anything missing prints as itself.")

(defvar draw-kbd-layer-names
  '(("M"  . "Alt")        ("MS" . "Alt+Shift")
    ("C"  . "Ctrl")       ("CS" . "Ctrl+Shift")
    ("CM" . "Ctrl+Alt")   ("S"  . "Shift")
    (""   . "plain"))
  "What each cell-row tag means, for the legend.")

;; ergoemacs-theme-engine.el still refers to this variable, but the defvar was
;; dropped in commit dc2e1a6, so the --ergo sheets die without it.
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

;;; The model: keys, their layers, and what is left over

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

(defun draw-kbd--lookup (base mods)
  "(KEY . COMMAND) for BASE plus MODS in the global map, or nil."
  (when base
    (let ((key (ignore-errors
                 (vector (event-convert-list (append mods (list base)))))))
      (when key
        (let ((b (lookup-key (current-global-map) key)))
          (and b (not (integerp b)) (cons key b)))))))

(defun draw-kbd--cell-data (base shifted layers width seen)
  "Resolve one key's LAYERS into (TAG . LABEL) pairs.
BASE is the unmodified event and SHIFTED the event Shift makes of it, nil on a
named key where `shift' becomes a modifier instead.  Every key looked up is
recorded in SEEN, so that whatever is missing at the end is exactly what no
board showed."
  (mapcar
   (lambda (layer)
     (cl-destructuring-bind (tag mods shift-p) layer
       (let ((b (cond ((not shift-p) (draw-kbd--lookup base mods))
                      (shifted      (draw-kbd--lookup shifted mods))
                      (t            (draw-kbd--lookup base (cons 'shift mods))))))
         (when b (puthash (car b) t seen))
         (cons tag (draw-kbd--label (cdr-safe b) width)))))
   layers))

(defun draw-kbd--key-name (k)
  "The cap legend for named key K."
  (or (cdr (assoc k draw-kbd-key-names))
      (if (and (symbolp k) (string-match-p "\\`f[0-9]+\\'" (symbol-name k)))
          (upcase (symbol-name k))
        (format "%s" k))))

(defun draw-kbd--char-row (layout row)
  "Key descriptors (HEAD BASE SHIFTED) for ROW of the layout grid.
The layout vectors are a 4x15 grid padded with empty strings, unshifted first
and shifted at +60, so the physical arrangement comes for free."
  (cl-loop for col below 15
           for i = (+ (* row 15) col)
           for c = (nth i layout)
           for s = (nth (+ i 60) layout)
           unless (or (null c) (string= c ""))
           collect (list c (string-to-char c)
                         (and s (not (string= s "")) (string-to-char s)))))

(defun draw-kbd--named-row (keys)
  "Key descriptors for named KEYS, which have no shifted character."
  (mapcar (lambda (k) (list (draw-kbd--key-name k) k nil)) keys))

(defun draw-kbd--boards (layout)
  "Every board, as (LAYERS . KEY-DESCRIPTORS) in drawing order."
  (append
   (list (cons draw-kbd-fkey-layers
               (draw-kbd--named-row
                (cl-loop for n from 1 to 12 collect (intern (format "f%d" n))))))
   (cl-loop for row below 4
            collect (cons draw-kbd-layers (draw-kbd--char-row layout row)))
   (list (cons draw-kbd-nav-layers (draw-kbd--named-row draw-kbd-nav-keys)))))

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

(defun draw-kbd--sections (seen)
  "Everything of mine that no board showed, as an alist of (PREFIX . ROWS)."
  (let ((stock (let ((h (make-hash-table :test 'equal)))
                 (dolist (c draw-kbd--stock) (puthash (car c) (cdr c) h))
                 h))
        (groups (make-hash-table :test 'equal))
        out)
    (dolist (c (draw-kbd--collect (current-global-map)))
      (let ((key (draw-kbd--normalize (car c))))
        (when (and (draw-kbd--keyboard-p key)
                   (not (eq (gethash (car c) stock) (cdr c)))
                   (not (gethash key seen)))
          (push (cons (key-description key) (format "%s" (cdr c)))
                (gethash (if (> (length key) 1)
                             (key-description (vector (aref key 0)))
                           "other keys")
                         groups)))))
    (maphash (lambda (k v)
               (push (cons k (sort v (lambda (x y) (string< (car x) (car y)))))
                     out))
             groups)
    (sort out (lambda (a b) (string< (car a) (car b))))))

;;; SVG backend (the default)

(defvar draw-kbd-svg-label-width 11
  "Characters available to a command name inside a key cell.")

(defvar draw-kbd-svg-font "'DejaVu Sans Mono','Liberation Mono',monospace"
  "Cells are monospace so that truncating by character count is honest.")

(defvar draw-kbd-svg-font-size 14
  "Cell text size.  Everything else on the picture is derived from it, so this
is the one knob for how big the whole thing comes out.")

(defvar draw-kbd-svg-line-ratio 1.25
  "Line height as a multiple of `draw-kbd-svg-font-size'.")
(defvar draw-kbd-svg-gap 10 "Vertical space between boards.")
(defvar draw-kbd-svg-row-indent 14 "Stagger of each keyboard row.")

(defvar draw-kbd-svg-layer-colors
  '(("M"  . "#1a4b9c") ("MS" . "#b31a1a")
    ("C"  . "#13772f") ("CS" . "#a0199c")
    ("CM" . "#9c5a00") ("S"  . "#555555")
    (""   . "#222222"))
  "Colour per cell row, following the ergoemacs picture: blue Alt, red
Alt+Shift, green Ctrl, magenta Ctrl+Shift.")

(defun draw-kbd--svg-esc (s)
  "XML-escape S."
  (let ((s (replace-regexp-in-string "&" "&amp;" s)))
    (setq s (replace-regexp-in-string "<" "&lt;" s))
    (replace-regexp-in-string ">" "&gt;" s)))

(defun draw-kbd--svg-char-w ()
  "Advance width of the monospace cell font."
  (* draw-kbd-svg-font-size 0.602))

(defun draw-kbd--svg-line-h ()
  "Baseline-to-baseline distance inside a cell."
  (round (* draw-kbd-svg-font-size draw-kbd-svg-line-ratio)))

(defun draw-kbd--svg-pad ()
  "Breathing room between a cell's border and its text."
  (max 3 (round (* draw-kbd-svg-font-size 0.45))))

(defun draw-kbd--svg-cell-w ()
  (+ (* 2 (draw-kbd--svg-pad))
     (* (draw-kbd--svg-char-w) (+ 3 draw-kbd-svg-label-width))))

(defun draw-kbd--svg-cell-h (layers)
  (+ (* 2 (draw-kbd--svg-pad)) (* (draw-kbd--svg-line-h) (1+ (length layers)))))

(defun draw-kbd--svg-cell (x y head rows layers)
  "One key cell at X,Y with cap legend HEAD and (TAG . LABEL) ROWS."
  (let ((w (draw-kbd--svg-cell-w))
        (h (draw-kbd--svg-cell-h layers))
        (i 0))
    (concat
     (format "<g transform=\"translate(%.1f,%.1f)\">" x y)
     (format (concat "<rect x=\".5\" y=\".5\" width=\"%.1f\" height=\"%.1f\""
                     " rx=\"3\" fill=\"#fafafa\" stroke=\"#999\"/>")
             (- w 1) (- h 1))
     (format "<text x=\"%d\" y=\"%d\" font-weight=\"bold\" fill=\"#000\">%s</text>"
             (draw-kbd--svg-pad) (+ (draw-kbd--svg-pad) draw-kbd-svg-font-size)
             (draw-kbd--svg-esc head))
     (mapconcat
      (lambda (row)
        (setq i (1+ i))
        (let* ((tag (string-trim (car row)))
               (colour (or (cdr (assoc tag draw-kbd-svg-layer-colors)) "#222")))
          (if (string= (cdr row) ""
                       )
              ;; Nothing bound: just the tag, so the row still reads as a row.
              (format "<text x=\"%d\" y=\"%d\" fill=\"#bbb\">%s</text>"
                      (draw-kbd--svg-pad)
                      (+ (draw-kbd--svg-pad) draw-kbd-svg-font-size
                         (* i (draw-kbd--svg-line-h)))
                      (draw-kbd--svg-esc (car row)))
            (concat
             (format "<text x=\"%d\" y=\"%d\" fill=\"#aaa\">%s</text>"
                     (draw-kbd--svg-pad)
                     (+ (draw-kbd--svg-pad) draw-kbd-svg-font-size
                        (* i (draw-kbd--svg-line-h)))
                     (draw-kbd--svg-esc (car row)))
             (format "<text x=\"%.1f\" y=\"%d\" fill=\"%s\">%s</text>"
                     (+ (draw-kbd--svg-pad) (* 3 (draw-kbd--svg-char-w)))
                     (+ (draw-kbd--svg-pad) draw-kbd-svg-font-size
                        (* i (draw-kbd--svg-line-h)))
                     colour (draw-kbd--svg-esc (cdr row)))))))
      rows "")
     "</g>")))

(defun draw-kbd-svg (layout lay)
  "Write the picture.  Return its path."
  (let* ((seen (make-hash-table :test 'equal))
         (w (draw-kbd--svg-cell-w))
         (file (expand-file-name (concat lay ".svg") draw-kbd-output))
         ;; Clear of the heading and its subtitle, both sized from the font.
         (y (* draw-kbd-svg-font-size 4.2))
         (max-x 0)
         body)
    (cl-loop for board in (draw-kbd--boards layout)
             for n from 0
             do (let* ((layers (car board))
                       (keys (cdr board))
                       ;; The function-key and navigation boards are their own
                       ;; thing; only the four letter rows stagger.
                       (indent (if (and (> n 0) (< n 5))
                                   (* (1- n) draw-kbd-svg-row-indent)
                                 0))
                       (x indent))
                  (dolist (k keys)
                    (push (draw-kbd--svg-cell x y (nth 0 k)
                                              (draw-kbd--cell-data
                                               (nth 1 k) (nth 2 k) layers
                                               draw-kbd-svg-label-width seen)
                                              layers)
                          body)
                    (setq x (+ x w)))
                  (setq max-x (max max-x x))
                  (setq y (+ y (draw-kbd--svg-cell-h layers)
                             (if (memq n '(0 4)) draw-kbd-svg-gap 2)))))
    ;; Legend, then the prefix sections, then size the canvas to fit.
    (let ((lx 0))
      (setq y (+ y draw-kbd-svg-gap))
      (dolist (layer draw-kbd-layers)
        (let* ((tag (string-trim (car layer)))
               (name (or (cdr (assoc tag draw-kbd-layer-names)) tag)))
          (push (format (concat "<text x=\"%d\" y=\"%.1f\" fill=\"%s\""
                                " font-weight=\"bold\">%s = %s</text>")
                        lx y (or (cdr (assoc tag draw-kbd-svg-layer-colors)) "#222")
                        tag name)
                body)
          ;; Step by the widest legend entry rather than a fixed amount, so
          ;; the row survives a bigger font.
          (setq lx (+ lx (* (draw-kbd--svg-char-w)
                            (+ 4 (apply #'max
                                        (mapcar (lambda (l)
                                                  (let ((tg (string-trim (car l))))
                                                    (length (format "%s = %s" tg
                                                                    (or (cdr (assoc tg draw-kbd-layer-names))
                                                                        tg)))))
                                                draw-kbd-layers))))))))
      (setq max-x (max max-x lx))
      (setq y (+ y (* 2 (draw-kbd--svg-line-h)))))
    ;; The sections are short and many, so they are packed into shelves across
    ;; the width of the keyboard rather than run down the page.
    (let* ((sections (draw-kbd--sections seen))
           (cw (draw-kbd--svg-char-w))
           (gap-x 28)
           (gap-y 10)
           (blocks
            (mapcar
             (lambda (sec)
               (let* ((rows (cdr sec))
                      (keyw (apply #'max 0 (mapcar (lambda (r) (length (car r))) rows)))
                      (cmdw (apply #'max 0 (mapcar (lambda (r) (length (cdr r))) rows)))
                      (headw (length (format "%s (%d)" (car sec) (length rows))))
                      (indent 2))
                 (list sec
                       (* cw (max headw (+ indent keyw 1 cmdw)))   ; width
                       (* (draw-kbd--svg-line-h) (1+ (length rows)))  ; height
                       (* cw indent)                               ; key column
                       (* cw (+ indent keyw 1)))))                 ; command column
             sections)))
      (when blocks
        (push (format (concat "<text x=\"0\" y=\"%.1f\" fill=\"#000\""
                              " font-weight=\"bold\">Not on a board above."
                              "  Only my own bindings, not Emacs's:</text>")
                      y)
              body)
        (setq y (+ y (* 2 (draw-kbd--svg-line-h))))
        (let ((x 0) (shelf 0))
          (dolist (b blocks)
            (cl-destructuring-bind (sec w h key-x cmd-x) b
              (when (and (> x 0) (> (+ x w) max-x))
                (setq y (+ y shelf gap-y) x 0 shelf 0))
              (push (format (concat "<text x=\"%.1f\" y=\"%.1f\""
                                    " font-weight=\"bold\">%s (%d)</text>")
                            x (+ y (draw-kbd--svg-line-h))
                            (draw-kbd--svg-esc (car sec)) (length (cdr sec)))
                    body)
              (cl-loop for row in (cdr sec)
                       for i from 2
                       do (push (format (concat "<text x=\"%.1f\" y=\"%.1f\""
                                                " fill=\"#333\">%s</text>"
                                                "<text x=\"%.1f\" y=\"%.1f\""
                                                " fill=\"#333\">%s</text>")
                                        (+ x key-x) (+ y (* i (draw-kbd--svg-line-h)))
                                        (draw-kbd--svg-esc (car row))
                                        (+ x cmd-x) (+ y (* i (draw-kbd--svg-line-h)))
                                        (draw-kbd--svg-esc (cdr row)))
                                body))
              (setq shelf (max shelf h)
                    x (+ x w gap-x))))
          (setq y (+ y shelf)))))
    (let ((width (+ max-x 24))
          (height (+ y 16)))
      (with-temp-file file
        (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                (format (concat "<svg xmlns=\"http://www.w3.org/2000/svg\""
                                " width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\""
                                " font-family=\"%s\" font-size=\"%d\">\n")
                        width height width height
                        draw-kbd-svg-font draw-kbd-svg-font-size)
                (format "<rect width=\"%d\" height=\"%d\" fill=\"#ffffff\"/>\n"
                        width height)
                "<g transform=\"translate(12,12)\">\n"
                (format (concat "<text x=\"0\" y=\"%d\" font-size=\"%d\""
                                " font-weight=\"bold\" fill=\"#000\">%s (%s)</text>\n")
                        (round (* draw-kbd-svg-font-size 1.4))
                        (round (* draw-kbd-svg-font-size 1.5))
                        (draw-kbd--svg-esc lay) (draw-kbd--svg-esc draw-kbd-title))
                (format (concat "<text x=\"0\" y=\"%d\" fill=\"#666\">"
                                "the global keymap after loading %s, %s</text>\n")
                        (round (* draw-kbd-svg-font-size 2.9))
                        (draw-kbd--svg-esc (abbreviate-file-name draw-kbd-init-file))
                        (format-time-string "%Y-%m-%d"))
                (mapconcat #'identity (nreverse body) "\n")
                "\n</g>\n</svg>\n")))
    (message "draw-kbd: wrote %s" file)
    file))

;;; Text backend (--txt)

(defvar draw-kbd-ascii-label-width 9
  "Characters available to a command name inside a text key cell.")

(defvar draw-kbd-ascii-row-indent 3
  "Characters each text keyboard row is indented past the one above it.")

(defun draw-kbd--pad (s width)
  "Pad S with spaces to WIDTH columns, by display width, not character count."
  (concat s (make-string (max 0 (- width (string-width s))) ?\s)))

(defun draw-kbd--ascii-board (keys layers width indent seen)
  "Lines for one boxed board."
  (let ((cells (mapcar
                (lambda (k)
                  (cons (draw-kbd--pad (concat " " (nth 0 k)) width)
                        (mapcar (lambda (r)
                                  (draw-kbd--pad (format "%s %s" (car r) (cdr r))
                                                 width))
                                (draw-kbd--cell-data
                                 (nth 1 k) (nth 2 k) layers
                                 draw-kbd-ascii-label-width seen))))
                keys)))
    (when cells
      (let ((pad (make-string indent ?\s))
            (bar (make-string width ?─)))
        (append
         (list (concat pad "┌" (mapconcat (lambda (_) bar) cells "┬") "┐"))
         (cl-loop for i below (length (car cells))
                  collect (concat pad "│"
                                  (mapconcat (lambda (c) (nth i c)) cells "│")
                                  "│"))
         (list (concat pad "└" (mapconcat (lambda (_) bar) cells "┴") "┘")))))))

(defun draw-kbd--zip (blocks gap)
  "Lay BLOCKS, each (WIDTH . LINES), side by side separated by GAP spaces."
  (let ((h (apply #'max (mapcar (lambda (b) (length (cdr b))) blocks)))
        (sep (make-string gap ?\s)))
    (cl-loop for i below h
             collect (string-trim-right
                      (mapconcat (lambda (b)
                                   (draw-kbd--pad (or (nth i (cdr b)) "") (car b)))
                                 blocks sep)))))

(defun draw-kbd--ascii-sections (sections width)
  "Pack SECTIONS into shelves at most WIDTH columns wide."
  (let* ((gap 3)
         (blocks
          (mapcar
           (lambda (sec)
             (let* ((rows (cdr sec))
                    (keyw (apply #'max 0 (mapcar (lambda (r) (length (car r))) rows)))
                    (lines (cons (format "%s (%d)" (car sec) (length rows))
                                 (mapcar (lambda (r)
                                           (format "  %s %s"
                                                   (draw-kbd--pad (car r) keyw)
                                                   (cdr r)))
                                         rows))))
               (cons (apply #'max (mapcar #'string-width lines)) lines)))
           sections))
         out shelf (shelf-w 0))
    (dolist (b blocks)
      (cond
       ((null shelf) (setq shelf (list b) shelf-w (car b)))
       ((> (+ shelf-w gap (car b)) width)
        (setq out (append out (draw-kbd--zip shelf gap) (list ""))
              shelf (list b) shelf-w (car b)))
       (t (setq shelf (append shelf (list b))
                shelf-w (+ shelf-w gap (car b))))))
    (when shelf (setq out (append out (draw-kbd--zip shelf gap))))
    out))

(defun draw-kbd-ascii (layout lay)
  "Write the text board.  Return its path."
  (let* ((width (+ 3 draw-kbd-ascii-label-width))
         (seen (make-hash-table :test 'equal))
         (file (expand-file-name (concat lay ".txt") draw-kbd-output))
         body)
    (cl-loop for board in (draw-kbd--boards layout)
             for n from 0
             do (setq body
                      (append body
                              (draw-kbd--ascii-board
                               (cdr board) (car board) width
                               (if (and (> n 0) (< n 5))
                                   (* (1- n) draw-kbd-ascii-row-indent)
                                 0)
                               seen)
                              (if (memq n '(0 4)) (list "") nil))))
    (with-temp-file file
      (insert (format "The global keymap after loading %s\n"
                      (abbreviate-file-name draw-kbd-init-file))
              (format "%s layout, \"%s\", generated %s\n\n"
                      lay draw-kbd-title (format-time-string "%Y-%m-%d"))
              "Cell rows: "
              (mapconcat (lambda (l)
                           (let ((tag (string-trim (car l))))
                             (format "%s = %s" tag
                                     (or (cdr (assoc tag draw-kbd-layer-names)) tag))))
                         draw-kbd-layers "  ")
              "\n\n")
      (dolist (line body) (insert line "\n"))
      (let ((sections (draw-kbd--sections seen)))
        (when sections
          (insert "\nNot on a board above.  Only my own bindings are listed"
                  " here, not Emacs's:\n\n")
          ;; Packed across the width of the boards, not run down the page.
          (dolist (line (draw-kbd--ascii-sections
                         sections (apply #'max (mapcar #'string-width body))))
            (insert line "\n"))))
      (insert (format "\n%d bound keys are drawn on the boards above.\n"
                      (hash-table-count seen))))
    (message "draw-kbd: wrote %s" file)
    file))

;;; ergoemacs-mode's own template (--ergo)

(defvar draw-kbd-ergo-layers
  '((nil     control meta)
    (meta    control meta)
    (control hyper))
  "Modifiers the extra ergoemacs sheet shows, keyed by the layer the template
means.")

(defvar draw-kbd-ergo-legend
  '((meta          . "Ctrl+Alt+ == control meta")
    (meta-shift    . "Ctrl+Alt+⇧Shift+ == control meta shift")
    (control       . "Hyper+ == hyper")
    (control-shift . "Hyper+⇧Shift+ == hyper shift"))
  "Legend lines for the extra sheet, replacing the template's Alt/Ctrl ones.
These are written out verbatim: the legend slots are too narrow for
`ergoemacs-theme--svg-elt', which truncates anything it formats to 10
characters.")

(defun draw-kbd--ergo-remap (elt lay)
  "Re-point one parsed template slot ELT at the `draw-kbd-ergo-layers' modifiers."
  (cond
   ((eq elt 'title)
    (cons :text (format "%s (%s) Ctrl+Alt layer" lay draw-kbd-title)))
   ((and (symbolp elt) (assq elt draw-kbd-ergo-legend))
    (cons :text (cdr (assq elt draw-kbd-ergo-legend))))
   ((consp elt)
    (let* ((mods (cdr elt))
           (layer (cond ((memq 'control mods) 'control)
                        ((memq 'meta mods) 'meta)))
           (to (cdr (assq layer draw-kbd-ergo-layers))))
      (if to
          (cons (car elt) (append to (and (memq 'shift mods) '(shift))))
        elt)))
   (t elt)))

(defun draw-kbd-ergo (lay)
  "Write ergoemacs-mode's own sheets.  Return their paths."
  (let* ((ergoemacs-theme draw-kbd-title)
         (ergoemacs-keyboard-layout lay)
         ;; `ergoemacs-theme--svg' caches by file name under
         ;; `user-emacs-directory'; a throwaway directory keeps every run fresh
         ;; and keeps ergoemacs-extras/ out of ~/.emacs.d.
         (user-emacs-directory (file-name-as-directory
                                (make-temp-file "draw-kbd" t)))
         (generated (car (ergoemacs-theme--svg lay)))
         (layout (symbol-value (ergoemacs :layout lay)))
         (main (expand-file-name (concat lay "-ergo.svg") draw-kbd-output))
         (extra (expand-file-name (concat lay "-ergo-ctrl-meta.svg")
                                  draw-kbd-output)))
    (unless (and generated (file-exists-p generated))
      (error "draw-kbd: ergoemacs-theme--svg produced nothing"))
    (copy-file generated main t)
    (delete-directory user-emacs-directory t)
    ;; `ergoemacs-theme--svg' leaves the parsed template in the variable of the
    ;; same name, so the extra sheet costs only a second pass over that list.
    (with-temp-file extra
      (dolist (w ergoemacs-theme--svg)
        (let ((w (if (stringp w) w (draw-kbd--ergo-remap w lay))))
          (cond
           ((stringp w) (insert w))
           ((and (consp w) (eq (car w) :text))
            (insert ">" (ergoemacs-translate--svg-quote (cdr w)) "<"))
           (t (insert ">" (ergoemacs-theme--svg-elt w layout lay) "<"))))))
    (message "draw-kbd: wrote %s" main)
    (message "draw-kbd: wrote %s" extra)
    (list main extra)))

(defun draw-kbd ()
  "Draw the current global keymap.  Return the files written."
  (make-directory draw-kbd-output t)
  (let* ((ergoemacs-function-short-names
          (append draw-kbd-labels ergoemacs-function-short-names))
         (layout (symbol-value (ergoemacs :layout draw-kbd-layout))))
    (append (list (draw-kbd-svg layout draw-kbd-layout))
            (and draw-kbd-txt-p (list (draw-kbd-ascii layout draw-kbd-layout)))
            (and draw-kbd-ergo-p (draw-kbd-ergo draw-kbd-layout)))))

(draw-kbd)

;;; draw-kbd.el ends here
