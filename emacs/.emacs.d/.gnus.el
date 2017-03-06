;; I will try to use
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; https://www.emacswiki.org/emacs/GnusGmail#toc2 -- good table with actions

;; Terminology:
;; Summary buffer -- buffer with list of mails
;; Group buffer -- buffer with list of folders, group~buffer

;; Usage:
;; Group manipulation:
;; o -- show all groups, I should do it default
;; u -- subscribe (?) to group at cursor in group buffer
;; U -- subscribe to group with given name
;; q -- exit
;; C-u RET -- while opening group, will load read mail too; perhaps later I will
;;   do it by default

;; Message manipulation:
;; ! -- mark message as important, (!) before letter means exactly that
;; M-u -- mark message as unread and unimportant, wtf
;; B m -- move message to another folder; it is created if it doesn't exists;
;;   it is reported as spam/moved to trash if moved to the corresponding folder
;; B DEL -- in INBOX, removes the message, if in other directory removes the
;;   label, i.e. moves to INBOX, though I didn't check
;; N -- next mail
;; P -- previous mail
;; = -- close currently opened mail
;; ^ -- load direct parent of current message
;; A T -- load the whole thread of current message, not yet worksT
;; / o -- fetch more articles including unread ones

;; Search:
;; G G -- search mails at server side, mark groups with # for it; if cursor
;;   is before the first group, all groups are searched
;; / / -- limit (filter mails locally) by subject
;; / a -- limit by author
;; / w -- cancel current limit

;; package for searching mail
(require 'nnir)

;; Personal information, not related to access
(setq user-full-name "My Name"
      user-mail-address "username@gmail.com")

;;____________________________________________________________
;; Setup access
;; Gnus has a single "primary" select method and multiple "secondary" select
;; methods. The only difference is that the primary select method has a default
;; (tries your local news server), and the groups (folders) in it are not
;; prefixed with the server name.

;; set no primary server, we will always choose some secondary:
(setq gnus-select-method '(nnnil ""))

;; Gnus will try to read auth info from ~/.authinfo file with lines like
;; machine imap.yandex.com login sher-ars@yandex.ru password <yourpassword>
(add-to-list 'gnus-secondary-select-methods
	     ;; setup yandex with IMAP. Port and encryption is deduced
	     ;; automatically
             '(nnimap "yandex"
                      (nnimap-address "imap.yandex.com")
		      (nnimap-fetch-partial-articles t) ;; should speed up things, not sure whether actually does
		      ;; not yet dealed with
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      ;; (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 7)))

;; Setup to send email through SMTP
;; (setq message-send-mail-function 'smtpmail-send-it
      ;; smtpmail-default-smtp-server "smtp.gmail.com"
      ;; smtpmail-smtp-service 587
      ;; smtpmail-local-domain "homepc")



;;____________________________________________________________
;; What and how to show

;; By default, gnus shows only groups (folders) with unread messages.
;; This function forces it show everything (?)
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

;; How each mail entry looks like,
;; see help for this variable to know what different formatters mean
(setq gnus-summary-line-format "%U%R%z%D%I%(%[%4L: %-23,23f%]%) %s\n")

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;;____________________________________________________________
;; What and how to fetch

;; If folder contains more than this-var mails, Gnus will ask how many to load
;; and offer this-var by default
(setq gnus-large-newsgroup 50)
; Docs says it will use cache, but currently I don't understand how
(setq gnus-use-cache t)


;;____________________________________________________________
;; The keys

;; List all the subscribed groups even they contain zero un-read messages
(define-key gnus-group-mode-map (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; TODO most probably we can unset it only once
(defun my-gnus-summary-mode-config ()
  "For use in `'gnus-summary-mode-hook'."
  (message "huemoe")
  (local-set-key (kbd "M-k") nil) ; remove a key
  (local-set-key (kbd "M-i") nil) ; remove a key
  (local-set-key (kbd "M-s") nil) ; remove a key
)
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-config)

(defun my-gnus-group-mode-config ()
  "For use in `'gnus-group-mode-hook'."
  (message "huemoe")
  (local-set-key (kbd "M-k") nil) ; remove a key
  (local-set-key (kbd "M-i") nil) ; remove a key
)
(add-hook 'gnus-group-mode-hook 'my-gnus-group-mode-config)

;; TODO:
;; smtp, sending
;; loading threads properly
;; multiple accounts
;; IMAP speedup?
