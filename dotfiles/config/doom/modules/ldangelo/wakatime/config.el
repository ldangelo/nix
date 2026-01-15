;;; config.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Leo D'Angelo
;;
;; Author: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Maintainer: Leo D'Angelo <leo.dangelo@fortiumpartners.com>
;; Created: June 19, 2025
;; Modified: June 19, 2025
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
(use-package! wakatime-mode
  :disabled
  :ensure t
  :config
  (setq wakatime-api-key (getenv "WAKATIME_API_KEY"))
  (global-wakatime-mode))


(provide 'config)
;;; config.el ends here ff
