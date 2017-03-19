;; I will try to use
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; https://www.emacswiki.org/emacs/GnusGmail#toc2 -- good table with actions
;; http://www.cataclysmicmutation.com/2010/11/multiple-gmail-accounts-in-gnus -- multiple accounts

;; Terminology:
;; Summary buffer -- buffer with list of mails
;; Group buffer -- buffer with list of folders, group~buffer
;; Subscription -- when you are "subscribed", Gnus shows you unread articles in
;;   that group. Otherwise it pretends that group doesn't exist, for most
;;   purposes. Subscribe to groups you read often.
;;   'U' in group line shows that you are NOT subscribed to this group

;; Usage:
;; Group manipulations:
;; q -- exit
;; M-g -- refresh group
;; u -- subscribe to group at cursor in group buffer
;; U -- subscribe to group with given name
;; C-u RET -- while opening group, will load read mail too; perhaps later I will
;;   do it by default
;; By default, all subscribed groups are showed, regardless have they unread mail or not.
;; l -- show subscribed groups with unread messages (?)
;; L --
;; A A -- show ALL groups known to Gnus

;; Message manipulations:
;; ! -- mark message as important, (!) before letter means exactly that
;;   marked mails are cached on disk and may be read offline
;; M-u -- mark message as unread and unimportant, wtf
;; B m -- move message to another folder; it is created if it doesn't exists;
;;   it is reported as spam/moved to trash if moved to the corresponding folder
;; B DEL -- in INBOX, removes the message, if in other directory removes the
;;   label, i.e. moves to INBOX, though I didn't check
;; N -- next mail
;; P -- previous mail
;; =, w -- close currently opened mail
;; / o -- fetch more articles including unread ones

;; Threads manipulations:
;; ^ -- load direct parent of current message; use C-u to load more
;; A R -- load all mails mentioned in the References header of current message;
;;   if we are lucky, it will load the whole thread. Works relatively fast
;; A T -- load the whole thread of current message, requires ALL group downloaded
;; T o -- go to the top of the thread
;; T h -- hide current thread
;; T s -- expand show current thread
;; T H, T S -- the same for all threads

;; Search:
;; G G -- search mails at server side, mark groups with # for it; if cursor
;;   is before the first group, all groups are searched
;; / / -- limit (filter mails locally) by subject
;; / a -- limit by author
;; / w -- cancel current limit

;; Sending:
;; m -- new mail

;; Attachments:
;; K o -- save file. You can also TAB to it and press RET
;; X m -- save all attachments matched to regexp

;; package for searching mail
(require 'nnir)

;; don't ask whether I am actually want to exit
(setq gnus-expert-user t)

;; Personal information, not related to access
(setq user-full-name "Arseny Sher")

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

(setq gnus-secondary-select-methods
      '(
	;; yandex imap setup. Port and encryption is deduced
	;; automatically
	(nnimap "main"
		(nnimap-address "imap.yandex.com")
		;; Don't download attachments. Run A C to get the full
		;; message if you use this.
		;; (nnimap-fetch-partial-articles t)
		;; not yet dealed with
		;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
		;; press 'E' to expire email
		;; (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
		;; (nnmail-expiry-wait 7)
	)
	;; outlook imap setup
	;; (nnimap "outlook_test"
	;; 	(nnimap-address "imap-mail.outlook.com")
	;; 	(nnimap-server-port 993)
	;; 	(nnimap-stream tls)
	;; )
        ;; ispras imap setup
	 (nnimap "work"
		 (nnimap-address "mail.ispras.ru")
		 (nnimap-server-port 993)
		 (nnimap-stream ssl)
	 )
       )
)


;; Setup to send email through SMTP
;; Again, credentials are read from ~/.authinfo
;; To use multiple accounts, for now I will use dirty hack with manual account
;; switching
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(defun choose-smtp-server (choice)
   "CHOICE is one of predefined accs to send mail from."
  (interactive
    (list (completing-read "Choose acc to send mail: " '("main" "ispras")))
  )
  (cond ((string= choice "ispras")
	 ;; ispras smtp setup
	 (setq smtpmail-smtp-server "mail.ispras.ru"
	       smtpmail-stream-type  'starttls
	       smtpmail-smtp-service 25
	       ;; just set header, this is used for auth
	       user-mail-address "sher-ars@ispras.ru"
	       ;; this is needed to copy sent message to proper 'sent' folder
	       gnus-message-archive-group "nnimap+work:Sent")
	 )
	((string= choice "main")
	 ;; yandex smtp setup
	 (setq smtpmail-smtp-server "smtp.yandex.com"
	       smtpmail-stream-type  'ssl
	       smtpmail-smtp-service 465
	       ;; just set header, this is used for auth
	       user-mail-address "sher-ars@yandex.ru"
	       ;; this is needed to copy sent message to proper 'sent' folder
	       gnus-message-archive-group "nnimap+main:Отправленные")
	)
  )
  (message "Sending via %s acc" choice)
)
;; Use main by default
(choose-smtp-server "main")


;;____________________________________________________________
;; What and how to show

;; By default, gnus shows only subscribed groups (folders) with unread messages.
;; This forces it always to show all subscribed groups
(setq gnus-permanently-visible-groups ".*")

;; How each mail entry looks like,
;; see help for this variable to know what different formatters mean
(setq gnus-summary-line-format "%U%R%z%D%I%(%[%4L: %-23,23f%]%) %s\n")

;; Show only the top level message of the thread.
(setq gnus-thread-hide-subtree t)
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-ignore-subject t)

;; sort threads, see manual
;; number ~ time when mail arrived
;; This will show threads with most recent messages first. But each thread
;; is displayed linearly, so the oldest mail is showed in summary.
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-number))

;;____________________________________________________________
;; What and how to fetch

;; If folder contains more than this-var mails, Gnus will ask how many to load
;; and offer this-var by default
(setq gnus-large-newsgroup 50)

;; enable caching, by default only ticked and dormant
;; read related part in the manual, it is pretty clear
(setq gnus-use-cache t)
;; Run gnus-cache-generate-active and if your change this
(setq gnus-cache-directory "~/gnus/cache")
;; Cache everything. Note that in my experience gnus considers caching only
;; after you have opened a mail; it won't cache just preloaded headers, or
;; whatever it first loads to show summary
(setq gnus-cache-enter-articles '(ticked dormant read unread))
;; and NEVER remove articles from the cache. This is dangerous!
;; I should consider setting up expiring and remove too old mails from cache
;; check out ~/gnus/cache dir size
(setq gnus-cache-remove-articles nil)

;; This should gather threads relying on References header, not on subject
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
;; Try to build complete thread for all loaded mails on group entering.
;; Not see how this works...
(setq gnus-fetch-old-headers 'some)

;;____________________________________________________________
;; Attachments
;; This is the default pattern for specifying files to save with X m
(setq gnus-summary-save-parts-default-mime ".*")


;;____________________________________________________________
;; The keys

;; TODO unite common keys?
(defun my-gnus-group-mode-config ()
  "For use in `'gnus-group-mode-hook'."
  (local-set-key (kbd "M-k") nil) ; remove a key
  (local-set-key (kbd "M-i") nil) ; remove a key
)
(add-hook 'gnus-group-mode-hook 'my-gnus-group-mode-config)

(defun my-gnus-summary-mode-config ()
  "For use in `'gnus-summary-mode-hook'."
  (local-set-key (kbd "M-k") nil) ; remove a key
  (local-set-key (kbd "M-i") nil) ; remove a key
  (local-set-key (kbd "M-s") nil) ; remove a key
  (local-set-key (kbd "w") 'gnus-summary-expand-window)
)
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-config)

(defun my-gnus-article-mode-config()
  "For use in `'gnus-article-mode-hook'."
  (local-set-key (kbd "w") 'gnus-summary-expand-window)
)
(add-hook 'gnus-article-mode-hook 'my-gnus-summary-mode-config)


;; TODO:
;; multiple smtp
;; loading whole thread without loading full group?
;; restrict cache size
;; goto bottom of the thread
;; IMAP speedup?
