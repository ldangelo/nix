;;; config/init-email.el -*- lexical-binding: t; -*-
(use-package! htmlize
  :load-path "packages/emacs-htmlize")
;; (use-package org-msg
;;   :after mu4e
;;   :config
;;   (setq mail-user-agent 'mu4e-user-agent)

;;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
;;         org-msg-startup "hidestars indent inlineimages"
;;         org-msg-greeting-fmt "\nHi%s,\n\n"
;;         ;;          org-msg-recipient-names '(("jeremy.compostella@gmail.com" . "Jérémy"))
;;         org-msg-greeting-name-limit 3
;;         org-msg-default-alternatives '((new		. (text html))
;;                                        (reply-to-html	. (text html))
;;                                        (reply-to-text	. (text)))
;;         org-msg-convert-citation t)

;;   (org-msg-mode))

;;
;; setup e-mail mu4e
(setq
 send-mail-function 'message-send-mail-with-sendmail
 message-send-mail-function 'message-send-mail-with-sendmail
 sendmail-program "/etc/profiles/per-user/ldangelo/bin/msmtp"
 mu4e-send-messages-behavior 'delete
 mail-user-agent 'mu4e-user-agent
 mu4e-get-mail-command "mbsync -a"
 ;;   org-mu4e-convert-to-html t
 )

;; use imagemagick, if available
(setq mu4e-inboxes "maildir:/ OR maildir:/icloud/Inbox OR maildir:/Fortium/inbox)")
(setq todays-unread-emails (concat mu4e-inboxes " AND date:today..now"))

(setq mu4e-alert-interesting-mail-query todays-unread-emails)
(setq fortium-signature "#+begin_signature\n--\n#+INCLUDE: ~/Documents/fortium-signature.html export html\n#+end_signature\n")
(setq org-msg-signature fortium-signature)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Fortium"
           :enter-func (lambda () (mu4e-message "Entering Fortium context"))
           :leave-func (lambda () (mu4e-message "Leaving Fortium context"))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Fortium" (mu4e-message-field msg :maildir))))
           :vars '( (user-mail-address . "leo.dangelo@fortiumpartners.com")
                    (user-full-name . "Leo D'Angelo")
                    (mu4e-compose-signature . t)
                    ;;                        (message-signature-file . "~/Documents/fortium-signature.html")
                    ;;                        (org-msg-signature . fortium-signature)
                    (mu4e-drafts-folder . "/Fortium/[Gmail].Drafts")
                    (mu4e-someday-folder . "/Fortium/Someday")
                    (mu4e-waiting-folder . "/Fortium/Waiting")
                    (mu4e-refile-folder . "/Fortium/Archived")
                    (mu4e-review-folder . "/Fortium/Review")
                    (mu4e-sent-folder . "/Fortium/[Gmail].Sent Mail")
                    (mu4e-trash-folder . "/Fortium/[Gmail].Trash")
                    (mu4e-update-interval . 1800)))
         ,(make-mu4e-context
           :name "icloud"
           :enter-func (lambda () (mu4e-message "Entering Fortium context"))
           :leave-func (lambda () (mu4e-message "Leaving Fortium context"))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/icloud" (mu4e-message-field msg :maildir))))
           :vars '(
                   (user-email-address . "ldangelo@mac.com")
                   (user-full-name . "Leo D'Angelo")
                   (mu4e-compose-signature . "Leo A. D'Angelo\ne-mail: ldangelo@mac.com\ncell: (972) 979-0116")
                   (mu4e-drafts-folder . "/icloud/Drafts")
                   (mu4e-someday-folder . "/icloud/Someday")
                   (mu4e-waiting-folder . "/icloud/Waiting")
                   (mu4e-refile-folder . "/icloud/Archive")
                   (mu4e-review-folder . "/icloud/Review")
                   (mu4e-sent-folder . "/icloud/Sent")
                   (mu4e-trash-folder . "/icloud/Trash")
                   (mu4e-update-interval . 1800)))))



;; ;; use the standard bindings as a base
(setq mu4e-bookmarks
      '( (:name "Unread Messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
         (:name "Today's messages" :query "date:1d.. AND NOT flag:trashed" :key ?a)
         (:name "Inbox messages" :query (lambda () (concat mu4e-inboxes))  :key ?i)
         (:name "Today's undeleted" :query (lambda () (concat mu4e-inboxes " date:today..now AND NOT flag:trashed")) :key ?A)
         (:name "Last 2 day's undeleted" :query (lambda () (concat mu4e-inboxes " date:2d..now AND NOT flag:trashed")) :key ?L)
         (:name "Last 7 day's undeleted" :query (lambda () (concat mu4e-inboxes " date:7d..now AND flag:unread AND NOT flag:trashed")) :key ?W)
         (:name "Flagged Messages" :query "flag:flagged"  :key ?g)
         (:name "Follow-up" :query "x:follow-up" :key ?f)
         (:name "Read Review" :query "x:read-review" :key ?r)
         ))

(defun htmlize-and-send ()
  "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  (interactive)
  (org-mime-htmlize))

(defun mymu4e/header-mark-message(msg tags template)
  (mu4e-action-retag-message msg tags)
  (org-capture t template)
  (mu4e-headers-mark-for-refile))

(defun mymu4e/mark-for-travel(msg)
  (interactive)
  (mu4e-action-retag-message msg "+Travel")
  (org-capture t "t")
  (mu4e-headers-mark-for-refile))

(defun mymu4e/mark-for-todo (msg)
  (interactive)
  (mu4e-action-retag-message msg "+TODO")
  (org-capture t "t")
  (mu4e-headers-mark-for-refile))

(defun mymu4e/mark-for-followup (msg)
  (interactive)
  (mu4e-action-retag-message msg "+RESPOND")
  (org-capture t "r")
  (mu4e-headers-mark-for-refile))


(defun mymu4e/mark-for-reference (msg)
  (interactive)
  (mu4e-action-retag-message msg "+REFERENCE")
  (org-capture t "R")
  (mu4e-headers-mark-for-refile))

(defun mymu4e/mark-for-read-review (msg)
  (interactive)
  (mu4e-action-retag-message msg "+READ")
  (mu4e-headers-mark-for-refile))

(require 'mu4e-contrib)
;;  (add-to-list 'mu4e-headers-actions '("aSelect All" . mu4e-headers-mark-all))
(add-to-list 'mu4e-headers-actions '("Tag" . mu4e-action-retag-message))
(add-to-list 'mu4e-headers-actions '("tTodo" . mymu4e/mark-for-todo))
(add-to-list 'mu4e-headers-actions '("vTravel" . mymu4e/mark-for-travel))
(add-to-list 'mu4e-headers-actions '("RReference" . mymu4e/mark-for-reference))
(add-to-list 'mu4e-headers-actions '("FFollow Up" . mymu4e/mark-for-followup))
(add-to-list 'mu4e-headers-actions '("rRead-Review" . mymu4e/mark-for-read-review))
(add-to-list 'mu4e-headers-actions '("fFilter by sender" . mu4e-headers-action-filter-by-sender))

;; (defadvice! mu4e-headers-rerun-search (before reindex-before-search)
;;   (mu4e-update-index))
;; (ad-activate 'mu4e-headers-rerun-search)

(defun mu4e-headers-action-filter-by-sender (msg)
  "Search for all messages by the current sender"
  (let* ((sender (car-safe (mu4e-message-field msg :from)))
         (name (mu4e-contact-name sender))
         (email (mu4e-contact-email sender)))
    (message "name: %s address: %s" name email)
    (mu4e-headers-search (concat "not tag:Trash and from:" email) nil nil nil nil nil)))


(defun my-string-width (str)
  "Return the width in pixels of a string in the current
            window's default font. If the font is mono-spaced, this
            will also be the width of all other printable characters."
  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert str)
      (car (window-text-pixel-size)))))


(cl-defun mu4e~normalised-icon (name &key set colour height v-adjust)
  "Convert :icon declaration to icon"
  (let* ((icon-set (intern (concat "all-the-icons-" (or set "faicon"))))
         (v-adjust (or v-adjust 0.02))
         (height (or height 0.8))
         (icon (if colour
                   (apply icon-set `(,name :face ,(intern (concat "all-the-icons-" colour)) :height ,height :v-adjust ,v-adjust))
                 (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
         (icon-width (my-string-width icon))
         (space-width (my-string-width " "))
         (space-factor (- 2 (/ (float icon-width) space-width))))
    (concat (propertize " " 'display `(space . (:width ,space-factor))) icon)))


(defun mu4e~initialise-icons ()
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark      (cons "D" (mu4e~normalised-icon "pencil"))
        mu4e-headers-flagged-mark    (cons "F" (mu4e~normalised-icon "flag"))
        mu4e-headers-new-mark        (cons "N" (mu4e~normalised-icon "sync" :set "material" :height 0.8 :v-adjust -0.10))
        mu4e-headers-passed-mark     (cons "P" (mu4e~normalised-icon "arrow-right"))
        mu4e-headers-replied-mark    (cons "R" (mu4e~normalised-icon "arrow-right"))
        mu4e-headers-seen-mark       (cons "S" (mu4e~normalised-icon "eye" :height 0.6 :v-adjust 0.07 :colour "dsilver"))
        mu4e-headers-trashed-mark    (cons "T" (mu4e~normalised-icon "trash"))
        mu4e-headers-attach-mark     (cons "a" (mu4e~normalised-icon "file-text-o" :colour "silver"))
        mu4e-headers-encrypted-mark  (cons "x" (mu4e~normalised-icon "lock"))
        mu4e-headers-signed-mark     (cons "s" (mu4e~normalised-icon "certificate" :height 0.7 :colour "dpurple"))
        mu4e-headers-unread-mark     (cons "u" (mu4e~normalised-icon "eye-slash" :v-adjust 0.05)))

  (if (display-graphic-p)
      (mu4e~initialise-icons)
    ;; When it's the server, wait till the first graphical frame
    (add-hook! 'server-after-make-frame-hook
      (defun mu4e~initialise-icons-hook ()
        (when (display-graphic-p)
          (mu4e~initialise-icons)
          (remove-hook #'mu4e~initialise-icons-hook))))))

(defun mu4e-header-colourise (str)
  (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
         (colour (nth (% str-sum (length mu4e-header-colourised-faces))
                      mu4e-header-colourised-faces)))
    (put-text-property 0 (length str) 'face colour str)
    str))


(setq mu4e-headers-fields
      '((:human-date . 12)
        (:from . 25)
        (:to . 20)
        (:recipnum . 2)
        (:flags . 6)
        (:tags . 10)
        (:subject)))


(general-def 'normal mu4e-headers-mode-map
  (kbd "M") #'mu4e-headers-mark-all)

(general-def 'normal  mu4e-headers-mode-map
  "M" #'mu4e-headers-mark-all
  "*" #'mu4e-headers-mark-for-something
  "!" #'mu4e-headers-mark-for-read
  "?" #'mu4e-headers-mark-for-unread
  "u" #'mu4e-headers-mark-for-unread)




(defun mu4e~main-action-prettier-str (str &optional func-or-shortcut)
  "Highlight the first occurrence of [.] in STR.
            If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
            when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT is
            a string, execute the corresponding keyboard action when it is
            clicked."
  :override #'mu4e~main-action-str
  (let ((newstr
         (replace-regexp-in-string
          "\\[\\(..?\\)\\]"
          (lambda(m)
            (format "%s"
                    (propertize (match-string 1 m) 'face '(mode-line-emphasis bold))))
          (replace-regexp-in-string "\t\\*" "\t⚫" str)))
        (map (make-sparse-keymap))
        (func (if (functionp func-or-shortcut)
                  func-or-shortcut
                (if (stringp func-or-shortcut)
                    (lambda()(interactive)
                      (execute-kbd-macro func-or-shortcut))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "[A-Za-z].+$" newstr)
                       (- (length newstr) 1) 'mouse-face 'highlight newstr)
    newstr))

(use-package mu4e-alert
  :ensure t
  :hook (mu4e-index-updated . mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style 'notifier))
