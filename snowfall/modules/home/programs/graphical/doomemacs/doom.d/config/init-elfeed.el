;;; init-elfeed.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Leo A. D'Angelo
;;
;; Author: Leo A. D'Angelo <ldangelo@mac.com>
;; Maintainer: Leo A. D'Angelo <ldangelo@mac.com>
;; Created: July 04, 2024
;; Modified: July 04, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/ldangelo/init-elfeed
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Initialize elfeed-org for news reading
;;
;;
;;; Code:

(defun elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-handlers ("http[s]://*" . eww-browse-url))
        (elfeed-show-visit use-generic-p))))

(defun browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (and new-window (>= emacs-major-version 23))
      (ns-do-applescript
       (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
                       "tell application \"Safari\" to activate") url))
    (start-process (concat "open " url) nil "open" url)))

(defun elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-handlers ("http[s]://*" . eww-browse-url))
        (elfeed-search-browse-url use-generic-p))))

(defun browse-url-mpv (url &optional single)
  (start-process "mpv" nil "mpv" (shell-quote-argument url)))

(use-package! elfeed
  :bind ("C-c f")
  :hook elfeed-search-mode-hook elfeed-update
  :config
  (setq  elfeed-goodies/entry-pane-position 'bottom)
  (setq elfeed-goodies/feed-source-column-width 32)
  (setq elfeed-search-title-min-width 32)
  (setq browse-url-handlers
        '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
          ("." . browse-url-default-macosx-browser)))
  )

;;          (add-hook 'elfeed-search-mode-hook #'elfeed-update)



;;(setq-hook! 'elfeed-search-mode-hook browse-url-handlers '(("." . #'eww-browse-url)))
;;(setq-hook! 'elfeed-show-mode-hook browse-url-handlers '(("." . xwwp-browse-url-other-window)))

;; download youtube urls with mpv instead of eww

(use-package! elfeed-org
  :config
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))
  (elfeed-org))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
