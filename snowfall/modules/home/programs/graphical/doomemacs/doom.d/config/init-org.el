;;; config/init-org.el -*- lexical-binding: t; -*-
(file! "./packages/doct-org-roam/doct-org-roam.el")
(setq org-directory "~/org/")

(add-hook! org (lambda () "Beautify org symbols."
                 (prettify-symbols-mode 1)))

(add-hook! org-indent (lambda ()
                        (diminish 'org-indent-mode)
                        (make-variable-buffer-local 'show-paren-mode)
                        (setq show-paren-mode nil)))

(after! org
  (defun hot-expand (str &optional mod)
    "Expand org template.

        STR is a structure template string recognised by org like <s. MOD is a
        string with additional parameters to add the begin line of the
        structure element. HEADER string includes more parameters that are
        prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; To speed up startup, don't put to init section
  (setq org-modules nil                 ; Faster loading
        org-directory "~/org"
        org-capture-templates
        `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+olp+datetree
                                ,(concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("b" "Book" entry (file+olp+datetree
                             ,(concat org-directory "/book.org"))
           "* Topic: %^{Description}  %^g %? Added: %U"))

        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        ;; Agenda styling
        ;;     org-agenda-files (list org-directory)
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)
  (setq
   org-agenda-start-with-clockreport-mode t
   org-agenda-start-with-follow-mode nil
   org-agenda-start-with-log-mode t
   org-agenda-include-deadlines t
   org-agenda-include-diary t
   org-directory "/Users/ldangelo/Documents/org"
   org-agenda-files "/Users/ldangelo/Documents/org/org-agenda-files.txt"

   org-default-inbox-file (expand-file-name "~/org/inbox.org")
   org-default-someday-file (expand-file-name "~/org/someday.org")
   org-refile-targets '((org-agenda-files . (:maxlevel . 3))))


  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (webkit-browse-url (concat "file://" file) t)))
          org-file-apps))
  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (perl       . t)
      (python     . t)
      (ruby       . t)
      (js         . t)
      (css        . t)
      (sass       . t)
      (C          . t)
      (java       . t)
      (shell      . t)
      (plantuml   . t))
    "Alist of org ob languages.")
  (use-package doct)
  (use-package doct-org-roam
    :load-path "packages/"
    :commands (doct-org-roam)
    :after org-roam
    :init
    (setq org-roam-directory "~/org/roam")
    (setq org-roam-dailies-directory "~/org/roam/daily")
    (setq org-roam-capture-templates
          (doct-org-roam '(
                           ("Default" :keys "d"
                            :file "%<Y%m%d%H%M%S>-${slug}.org"
                            :prepend t
                            :type plain
                            :template ("#+title: ${title}"
                                       "%?"))

                           ("Project" :keys "p"
                            :file "%<Y%m%d%H%M%S>-${slug}.org"
                            :prepend t
                            :type plain
                            :template ("#+title: ${title}"
                                       "* Overview"
                                       "* Goals"
                                       "* Tasks"
                                       "** TODO Add initial Tasks"
                                       "** TODO %?"
                                       "* Dates"
                                       "* Contacts"))
                           ))))

  (use-package org-roam-protocol
    :ensure org-roam
    :config
    (setq org-roam-dailies-capture-templates
          '(
            ("d" "Today"
             plain (file "/Users/ldangelo/org/templates/org-roam-dailies.org")
             :if-new (file "%<%Y-%m-%d>.org")
             )

            ("e" "Email" entry "** TODO Respond to e-mail from %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n:PROPERTIES:\nCREATED: %U\n:END:\n %a"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<Y-%m-%d>\n" ("Tasks")))

            ("p" "Phone Call" entry "** Phone call with %?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<Y-%m-%d>\n" ("Phone Calls")))

            ("m" "Meeting" entry "** Meeting with %?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<Y-%m-%d>\n" ("Meetings")))

            ("t" "Todo" entry "** TODO %?\n%a"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<Y-%m-%d>\n" ("Tasks")))

            ("$" "Trade"
             table-line
             "|%<%H:%M>|%^{Direction|BUY|SELL}|%^{Size|5}|%^{WL|WIN|LOSS}|%^{Gross}|%^{Commissions}|"
             :table-line-pos "II-1" ;; first line before the second horizontal separator line
             :target (file+olp "%<%Y-%m-%d>.org" ("Trading" "Trade Details") )
             )
            ))

    (setq org-roam-capture-ref-templates
          '(
            ("r" "ref" plain "%?" :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :BOOKMARK:") :unnarrowed t)

            ;; used by org-roam-protocol
            ;; look here too see how it is used: [[file:~/.qutebrowser/config.py][qutebrowser]]
            ("R" "ref" plain "%?"
             :target (file+head "web/${slug}.org" "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n#+filetags: :BOOKMARK:\n\n")
             :unnarrowed t)))))
