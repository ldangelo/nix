;;; init.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Leo D'Angelo
;;
;; Author: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Maintainer: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Created: May 29, 2025
;; Modified: May 29, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/ldangelo/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(defcustom notmuch-vip-file (expand-file-name "~/Documents/vip.txt")
  "Path to the VIP email addresses file."
  :type 'file
  :group 'notmuch)

(defcustom notmuch-vip-update-script (expand-file-name "~/.config/afew/update-vip-filter.py")
  "Path to the script that updates afew configuration from VIP file."
  :type 'file
  :group 'notmuch)

(defcustom notmuch-vip-auto-update-config t
  "Automatically run update script after adding VIP."
  :type 'boolean
  :group 'notmuch)

(defun notmuch-vip--extract-email (from-header)
  "Extract email address from FROM-HEADER string.
Handles formats like 'Name <email@example.com>' or 'email@example.com'."
  (if (string-match "<\\(.+?\\)>" from-header)
      (match-string 1 from-header)
    (string-trim from-header)))

(defun notmuch-vip--get-sender-email ()
  "Get sender email address from current notmuch context."
  (cond
   ;; In notmuch-show mode
   ((derived-mode-p 'notmuch-show-mode)
    (let* ((msg (notmuch-show-get-message-properties))
           (headers (plist-get msg :headers))
           (from (plist-get headers :From)))
      (when from
        (notmuch-vip--extract-email from))))

   ;; In notmuch-search mode
   ((derived-mode-p 'notmuch-search-mode)
    (let ((authors (notmuch-search-find-authors)))
      (when authors
        (notmuch-vip--extract-email authors))))

   ;; In notmuch-tree mode
   ((derived-mode-p 'notmuch-tree-mode)
    (let ((authors (notmuch-tree-get-authors)))
      (when authors
        (notmuch-vip--extract-email authors))))

   (t
    (error "Not in a notmuch buffer"))))

(defun notmuch-vip--email-in-vip-list-p (email)
  "Check if EMAIL is already in the VIP list."
  (when (file-exists-p notmuch-vip-file)
    (with-temp-buffer
      (insert-file-contents notmuch-vip-file)
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-quote email) "$") nil t))))

(defun notmuch-vip--add-email-to-file (email)
  "Add EMAIL to the VIP file."
  (with-temp-buffer
    (when (file-exists-p notmuch-vip-file)
      (insert-file-contents notmuch-vip-file))
    (goto-char (point-max))
    ;; Add newline if file doesn't end with one
    (unless (or (= (point) (point-min))
                (eq (char-before) ?\n))
      (insert "\n"))
    (insert email "\n")
    (write-region (point-min) (point-max) notmuch-vip-file)))

(defun notmuch-vip--update-afew-config ()
  "Run the Python script to update afew configuration."
  (when (and notmuch-vip-auto-update-config
             (file-exists-p notmuch-vip-update-script))
    (let ((result (shell-command-to-string
                   (format "python3 %s" (shell-quote-argument notmuch-vip-update-script)))))
      (message "Afew config updated: %s" (string-trim result)))))

;;;###autoload
(defun notmuch-vip-add-sender ()
  "Add the sender of the current email to the VIP list.
Works in notmuch-show, notmuch-search, and notmuch-tree modes."
  (interactive)
  (let ((email (notmuch-vip--get-sender-email)))
    (if email
        (if (notmuch-vip--email-in-vip-list-p email)
            (message "%s is already in VIP list" email)
          (notmuch-vip--add-email-to-file email)
          (message "Added %s to VIP list" email)
          (notmuch-vip--update-afew-config))
      (error "Could not extract sender email address"))))

;;;###autoload
(defun notmuch-vip-remove-sender ()
  "Remove the sender of the current email from the VIP list.
Works in notmuch-show, notmuch-search, and notmuch-tree modes."
  (interactive)
  (let ((email (notmuch-vip--get-sender-email)))
    (if email
        (if (not (notmuch-vip--email-in-vip-list-p email))
            (message "%s is not in VIP list" email)
          (with-temp-buffer
            (insert-file-contents notmuch-vip-file)
            (goto-char (point-min))
            (when (re-search-forward (concat "^" (regexp-quote email) "$") nil t)
              (delete-region (line-beginning-position) (1+ (line-end-position))))
            (write-region (point-min) (point-max) notmuch-vip-file))
          (message "Removed %s from VIP list" email)
          (notmuch-vip--update-afew-config))
      (error "Could not extract sender email address"))))

;;;###autoload
(defun notmuch-vip-show-list ()
  "Display the VIP list in a buffer."
  (interactive)
  (if (file-exists-p notmuch-vip-file)
      (view-file notmuch-vip-file)
    (message "VIP file does not exist: %s" notmuch-vip-file)))

;;;###autoload
(defun notmuch-vip-tag-sender ()
  "Add VIP tag to current message and add sender to VIP list."
  (interactive)
  (notmuch-vip-add-sender)
  (cond
   ((derived-mode-p 'notmuch-show-mode)
    (notmuch-show-tag '("+vip")))
   ((derived-mode-p 'notmuch-search-mode)
    (notmuch-search-tag '("+vip")))
   ((derived-mode-p 'notmuch-tree-mode)
    (notmuch-tree-tag '("+vip")))))


(use-package! notmuch-multi
  :defer t
  :after (evil notmuch)
  :hook (evil-collection-setup . (lambda (&rest a) (notmuch-setup)))
  :config
  (require 'notmuch-tree)
  
  (setq notmuch-multi-mark-delete-tags '("+deleted" "-inbox" "-unread"))
  (setq notmuch-multi-delete-tag "deleted")
  (setq +notmuch-sync-backend "/Users/ldangelo/bin/getmail.sh")
  (setq notmuch-show-text/html-blocked-images nil)
  (setq notmuch-show-text/html-mime-handler 'notmuch-show-view-as-html)
  (setq sendmail-program "/opt/homebrew/bin/msmtp")
  (setq mm-text-html-renderer 'gnus-w3m)

  (setq notmuch-hello-sections '(
                                 notmuch-multi-hello-insert-accounts-searches
                                 notmuch-hello-insert-alltags
                                 )
        lad-notmuch-saved-searches `(
                                     ( :name "Inbox"
                                             :query "tag:inbox and not tag:deleted"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "i"))
                                     ( :name "Unread Inbox"
                                             :query "tag:unread and tag:inbox"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "u"))
                                     ( :name "Unread"
                                             :query "tag:unread"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "U"))
                                     ( :name "All"
                                             :query "*"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "a"))
                                     ( :name "Archived"
                                             :query "tag:archived"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "A"))
                                     ( :name "Important"
                                             :query "tag:important"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "I"))
                                     ( :name "Starred"
                                             :query "tag:flagged"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "s"))
                                     ( :name "Unclassified"
                                             :query "tag:read and NOT tag:expire"
                                             :sort-order newest-first
                                             :search-type tree
                                             :key ,(kbd "s"))
                                     ))
  (notmuch-multi-accounts-saved-searches-set
   `(
     (:account (:name "Fortium" :query "tag:fortium" :key-prefix "f")
      :searches ,lad-notmuch-saved-searches)
     (:account (:name "ICloud" :query "tag:icloud" :key-prefix "I")
      :searches ,lad-notmuch-saved-searches)
     (:account (:name "Solo" :query "tag:Solo" :key-prefit "s")
      :searches ,lad-notmuch-saved-searches)
     (:account (:name "Curantis" :query "tag:Curantis" :key-prefix "c")
      :searches, lad-notmuch-saved-searches))))

(after! evil-collection
  (message "Setting notmuch custom keys")
  (evil-collection-define-key 'normal 'notmuch-show-mode-map
    "V" 'notmuch-vip-add-sender)

  (evil-collection-define-key 'normal 'notmuch-tree-mode-map
    "d" 'notmuch-multi-tree-delete-message
    "D" 'notmuch-multi-tree-delete-thread
    "V" 'notmuch-vip-add-sender)

  (evil-collection-define-key 'normal 'notmuch-search-mode-map
    "d" 'notmuch-multi-search-delete-thread
    "D" 'notmuch-multi-search-delete-all
    "V" 'notmuch-vip-add-sender))

;;; init.el ends here
