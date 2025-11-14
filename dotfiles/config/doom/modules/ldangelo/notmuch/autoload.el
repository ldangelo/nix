;;; autoload.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Leo D'Angelo
;;
;; Author: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Maintainer: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Created: May 30, 2025
;; Modified: May 30, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/ldangelo/autoload
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;###autoload
(defun notmuch-setup ()
  "Set up `evil' bindings for `notmuch'."
  (message "notmuch setup hook fired!!!")
  (evil-collection-inhibit-insert-state 'notmuch-show-mode-map)
  (evil-collection-inhibit-insert-state 'notmuch-search-mode-map)
  (evil-collection-inhibit-insert-state 'notmuch-tree-mode-map)

  (evil-set-initial-state 'notmuch-show-mode 'normal)
  (evil-set-initial-state 'notmuch-search-mode 'normal)
  (evil-set-initial-state 'notmuch-hello-mode 'normal)
  (evil-set-initial-state 'notmuch-tree-mode 'normal)

  (evil-collection-define-key 'normal 'notmuch-common-keymap
    "g?" 'notmuch-help
    "q" 'notmuch-bury-or-kill-this-buffer
    "s" 'notmuch-search
    "S" 'notmuch-tree
    "C" 'notmuch-mua-new-mail           ; like mu4e
    "cc" 'notmuch-mua-new-mail          ; like mu4e
    "gr" 'notmuch-refresh-this-buffer
    "gA" 'notmuch-refresh-all-buffers
    "gR" 'notmuch-poll-and-refresh-this-buffer
    "J" 'notmuch-jump-search)

  (evil-collection-define-key 'normal 'notmuch-hello-mode-map
    "g?" 'notmuch-version
    (kbd "TAB") 'widget-forward
    (kbd "RET") 'evil-collection-notmuch-hello-ret
    (kbd "S-TAB") 'widget-backward
    (kbd "<C-tab>") 'widget-backward)

  (evil-collection-define-key 'normal 'notmuch-show-mode-map
    "gd" 'goto-address-at-point
    "p" 'notmuch-show-save-attachments  ; like mu4e
    "A" 'notmuch-show-archive-thread-then-next
    "S" 'notmuch-show-filter-thread
    "K" 'notmuch-tag-jump
    "C" 'notmuch-mua-new-mail           ; like mu4e
    "cc" 'notmuch-mua-new-mail          ; like mu4e
    "cR" 'notmuch-show-reply
    "cf" 'notmuch-show-forward-message
    "X" 'notmuch-show-archive-thread-then-exit
    "zv" 'notmuch-tree-from-show-current-query ; like mu4e-conversation
    "<" 'notmuch-show-toggle-thread-indentation
    "a" 'notmuch-show-archive-message-then-next-or-next-thread
    "d" 'notmuch-multi-show-delete-message
    "=" 'evil-collection-notmuch-show-toggle-flagged
    "H" 'notmuch-show-toggle-visibility-headers
    "gj" 'notmuch-show-next-open-message
    "gk" 'notmuch-show-previous-open-message
    "]]" 'notmuch-show-next-message
    "[[" 'notmuch-show-previous-message
    (kbd "C-j") 'notmuch-show-next-message
    (kbd "C-k") 'notmuch-show-previous-message
    (kbd "M-j") 'notmuch-show-next-thread-show
    (kbd "M-k") 'notmuch-show-previous-thread-show
    "cr" 'notmuch-show-reply-sender
    (kbd "x") 'notmuch-show-archive-message-then-next-or-exit
    "|" 'notmuch-show-pipe-message
    "*" 'notmuch-show-tag-all
    "-" 'notmuch-show-remove-tag
    "+" 'notmuch-show-add-tag
    (kbd "TAB") 'notmuch-show-next-button
    (kbd "<backtab>") 'notmuch-show-previous-button
    (kbd "RET") 'notmuch-show-toggle-message
    "." 'notmuch-show-part-map)

  (evil-collection-define-key 'normal 'notmuch-tree-mode-map
    "g?" 'notmuch-help
    "q" 'notmuch-tree-quit
    "S" 'notmuch-tree-to-search
    "C" 'notmuch-mua-new-mail ; like mu4e
    "cc" 'notmuch-mua-new-mail ; like mu4e
    "J" 'notmuch-jump-search
    "zv" 'notmuch-search-from-tree-current-query ; like mu4e-conversation
    "cr" 'notmuch-show-reply-sender ; like mu4e
    "cR" 'notmuch-show-reply
    "D" 'notmuch-multi-tree-delete-thread
    "d" 'notmuch-multi-tree-delete-message
    "!" 'evil-collection-notmuch-tree-toggle-unread
    "=" 'notmuch-multi-tree-flag-message
    "K" 'notmuch-tag-jump
    (kbd "RET") 'notmuch-tree-show-message
    [mouse-1] 'notmuch-tree-show-message
    "A" 'notmuch-tree-archive-thread-then-next
    "a" 'notmuch-tree-archive-message-then-next
    "s" 'notmuch-tree-to-tree
    "gj" 'notmuch-tree-next-matching-message
    "gk" 'notmuch-tree-prev-matching-message
    "]]" 'notmuch-tree-next-message
    "[[" 'notmuch-tree-prev-message
    (kbd "C-k") 'notmuch-tree-prev-thread
    (kbd "C-j") 'notmuch-tree-next-thread
    "|" 'notmuch-show-pipe-message
    "-" 'notmuch-tree-remove-tag
    "+" 'notmuch-tree-add-tag
    "*" 'notmuch-tree-tag-thread
    "e" 'notmuch-tree-resume-message)

  (dolist (state '(normal visual))
    (evil-collection-define-key state 'notmuch-search-mode-map
      "cC" 'compose-mail-other-frame
      "J" 'notmuch-jump-search
      "S" 'notmuch-search-filter
      "K" 'notmuch-tag-jump
      "o" 'notmuch-search-toggle-order
      "zv" 'notmuch-tree-from-search-current-query
      "*" 'notmuch-search-tag-all
      "a" 'notmuch-search-archive-thread
      "cc" 'compose-mail                ; like mu4e
      "d" 'notmuch-multi-search-delete-thread
      "!" 'evil-collection-notmuch-search-toggle-unread
      "=" 'evil-collection-notmuch-search-toggle-flagged
      "q" 'notmuch-bury-or-kill-this-buffer
      "cr" 'notmuch-search-reply-to-thread-sender
      "cR" 'notmuch-search-reply-to-thread
      "t" 'notmuch-search-filter-by-tag
      [mouse-1] 'notmuch-search-show-thread
      "-" 'notmuch-search-remove-tag
      "+" 'notmuch-search-add-tag
      (kbd "RET") 'notmuch-search-show-thread)))



;;; autoload.el ends here
