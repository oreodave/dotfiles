;;; private/mail/config.el -*- lexical-binding: t; -*-
(defconst +mail/signature
  "---------------
Aryadev Chavali")

(use-package notmuch
  :commands notmuch
  :config
  (setq notmuch-show-logo nil
        message-signature +mail/signature
        mail-signature +mail/signature)

  (defun +mail/sync-mail()
    (interactive)
    (start-process "imap-call" "*offlineimap*" "offlineimap" '("-oq")))

  (map!
   (:leader
    "om" #'+mail/open-mail)
   :map (notmuch-hello-mode-map notmuch-tree-mode-map notmuch-show-mode-map)
   "u" #'notmuch-poll-and-refresh-this-buffer
   "gS" #'+mail/sync-mail))

(use-package smtpmail
  :commands mail-send
  :after notmuch
  :init
  (setq smtpmail-smtp-server "mail.aryadevchavali.com"
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-user "aryadev"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls))
