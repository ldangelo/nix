;;; config.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Leo D'Angelo
;;
;; Author: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Maintainer: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Created: July 17, 2025
;; Modified: July 17, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/ldangelo/config
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(use-package! gptel
  :config
  (setq! gptel-model ""
         gptel-backend
         (gptel-make-openai "OpenRouter"
           :host "openrouter.ai"
           :endpoint "/api/vi/chat/completions"
           :stream t
           :key (getenv "OPEN_ROUTER_API_KEY")
           :models '(
                     openai/gpt-3.5-turbo
                     ))))
