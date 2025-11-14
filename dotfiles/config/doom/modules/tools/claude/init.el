;; (defun claude-code-toggle-or-start ()
;;   "Toggle or start claude code"
;;   (interactive)
;;   (let ((claude-code-buffer (get-buffer "*claude*")))
;;     (if claude-code-buffer (claude-code-toggle)
;;       (claude-code))))

;; (map!
;;  (:leader
;;   ;; leaderkey bindings
;;   (:prefix-map ("A" . "AI")
;;                ( :prefix-map ("c" . "Claude Code")
;;                              :desc "Claude code " "c" #'claude-code-toggle-or-start))))
;; (use-package! claude-code
;;   :ensure t
;;   :config

;;   (claude-code-mode))

(use-package! claude-code-ide
  :ensure t
  :bind ("C-c C-a" . claude-code-ide-menu)
  :config (setq claude-code-ide-terminal-backend 'eat  ;; vterm has an annoying flashing

                )
  (map!
   (:leader
    ;; leaderkey bindings
    (:prefix-map ("A" . "AI")
                 ( :prefix-map ("c" . "Claude Code")
                               :desc "Claude code " "c" #'claude-code-ide-menu))))
  ;;(setq claude-code-program "/Users/ldangelo/.claude/local/claude")
  (claude-code-ide-emacs-tools-setup))
