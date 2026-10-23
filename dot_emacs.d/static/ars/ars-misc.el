;;; -*- lexical-binding: t -*-
;; misc functions

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation, and then reread local-dir vars."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)
    (reload-dir-locals-for-all-buffer-in-this-directory))

;; taken from
;; http://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (reload-dir-locals-for-current-buffer)))))

(defun find-file-as-root ()
  "Like `find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

;; helps to understand where key was mapped:
;; https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))


;; Emacs has no such command for a file buffer: the one kill-new of a file name
;; in the whole 31.1 tree is image-mode's, on W and only there. Dired is the
;; other half-exception, with dired-copy-filename-as-kill on w -- C-u 0 w for
;; the absolute name, C-u 1 w for one relative to the project -- which is why
;; this answers in dired too rather than refusing.
;;
;; kill-new alone reaches the X clipboard, select-enable-clipboard defaulting
;; to t and nothing here setting it otherwise.
(defun ars-copy-file-path (&optional local)
  "Put this buffer's absolute file name on the kill ring.
In dired, use the directory being listed. With a prefix argument LOCAL,
drop the tramp prefix from a remote name, leaving the path as that host's
own shell would write it -- paste-into-a-terminal rather than
paste-back-into-emacs."
  (interactive "P")
  (let ((path (or buffer-file-name
                  (and (derived-mode-p 'dired-mode) default-directory))))
    (unless path
      (user-error "This buffer is not visiting a file"))
    (when local
      (setq path (file-local-name path)))
    (kill-new path)
    (message "%s" path)))

;; Emacs inserts a date nowhere: time-stamp, the one thing in the tree that
;; comes close, rewrites a stamp a file already carries in its first lines
;; rather than writing one at point. The format is the local one -- day first,
;; dots, four-digit year -- not ISO, since this goes into prose for people to
;; read.
(defun ars-insert-date ()
  "Insert today's date at point, as dd.mm.yyyy."
  (interactive)
  (insert (format-time-string "%d.%m.%Y")))

;; The two rungs embark does not have a command for. embark-bindings covers
;; the middle of the ladder -- the local map plus the minor mode ones -- and
;; embark-bindings-in-keymap the far end, any keymap you can name; these are
;; the narrower and the wider view either side of it. Bound under C-h l.
(defun ars-embark-bindings-in-mode-map ()
  "Explore this major mode's own key bindings with completion."
  (interactive)
  (unless (current-local-map)
    (user-error "This buffer has no local map"))
  (embark-bindings-in-keymap (current-local-map)))

(defun ars-embark-bindings-global ()
  "Explore the current key bindings, the global ones included, with completion.
`embark-bindings' leaves those out unless given a prefix argument."
  (interactive)
  (embark-bindings t))

(message "ars-misc loaded")
(provide 'ars-misc)
