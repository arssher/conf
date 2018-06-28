;; load mu4e
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; directory with mail in mbox format
(setq mu4e-maildir "/d/mail")

(setq mu4e-view-show-addresses t)

;; allow for updating mail using 'U' in the main view:
(setq
  mu4e-get-mail-command "offlineimap -o"   ;; one-shot
  mu4e-update-interval nil)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
(setq mu4e-maildir-shortcuts
      '( ("/yandex/INBOX" . ?i)
	 ("/pgpro/INBOX" . ?w)
	 ("/yandex/ps-hackers" . ?h)
	 ("/yandex/pgsql-general" . ?g)
	 ("/yandex/Sent" . ?s)))

;; Personal information, not related to access
(setq user-full-name "Arseny Sher"
      mu4e-compose-signature
      (concat
       "Best regards,\n"
       "Arseny Sher\n"))
(setq mu4e-compose-dont-reply-to-self t)

 (setq mu4e-contexts
    `( ,(make-mu4e-context
          :name "yandex"
          :enter-func (lambda () (mu4e-message "Entering yandex context"))
          :leave-func (lambda () (mu4e-message "Leaving yandex context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                            :to "sher-ars@yandex.ru")))
          :vars '( ( user-mail-address . "sher-ars@yandex.ru"  )
		   ( smtpmail-smtp-server . "smtp.yandex.com" )
		   ( smtpmail-stream-type . ssl )
		   ( smtpmail-smtp-service . 465)
		   ( mu4e-sent-folder . "/yandex/Sent" )
		   ( mu4e-trash-folder . "/yandex/Trash" )
		   ( mu4e-drafts-folder . "/yandex/Drafts" )
                   ( mu4e-compose-signature .
                     (concat
                       "Best regards,\n"
                       "Arseny Sher\n"))))
       ,(make-mu4e-context
          :name "pgpro"
          :enter-func (lambda () (mu4e-message "Switch to the pgpro context"))
	  :leave-func (lambda () (mu4e-message "Leaving pgpro context"))
          ;; we match based on the contact-fields of the message
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                            :to "a.sher@postgrespro.ru")))
          :vars '( ( user-mail-address       . "a.sher@postgrespro.ru" )
		   ( smtpmail-smtp-server . "smtp.postgrespro.ru" )
		   ( mu4e-sent-folder . "/pgpro/Sent" )
		   ( mu4e-trash-folder . "/pgpro/Trash" )
		   ( mu4e-drafts-folder . "/pgpro/Drafts" )

                   ( mu4e-compose-signature  .
                     (concat
		      ""
                      "Arseny Sher \n"
                      "Postgres Professional: http://www.postgrespro.com\n"
		      "The Russian Postgres Company"))))))

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

;; Old way, without mu4e contexts
;; Setup to send email through SMTP
;; Again, credentials are read from ~/.authinfo
;; To use multiple accounts, for now I will use hack with manual account
;; switching
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(defun choose-smtp-server (choice)
   "CHOICE is one of predefined accs to send mail from."
  (interactive
    (list (completing-read "Choose acc to send mail: " '("yandex" "ispras" "pgpro")))
  )
  (cond ((string= choice "yandex")
	 ;; yandex smtp setup
	 (setq smtpmail-smtp-server "smtp.yandex.com"
	       smtpmail-stream-type  'ssl
	       smtpmail-smtp-service 465
	       ;; just set header, this is not used for auth
	       user-mail-address "sher-ars@yandex.ru"
	       ;; this is needed to copy sent message to proper 'sent' folder in gnus
	       gnus-message-archive-group "nnimap+yandex:Отправленные"
	       ;; same for mu4e
	       mu4e-sent-messages-behavior 'sent
	       mu4e-sent-folder "/yandex/Sent")
	 )
	((string= choice "pgpro")
	 ;; pgpro smtp setup
	 (setq smtpmail-smtp-server "smtp.postgrespro.ru"
	       ;; just set header, this is not used for auth
	       user-mail-address "a.sher@postgrespro.ru"
	       ;; this is needed to copy sent message to proper 'sent' folder
	       gnus-message-archive-group "nnimap+pgpro:Sent"
	       ;; same for mu4e
	       mu4e-sent-messages-behavior 'sent
	       mu4e-sent-folder "/pgpro/Sent")
	)
  )
  (message "Sending via %s acc" choice)
)
