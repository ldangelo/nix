(use-package! vterm-anti-flicker-filter
  :ensure t
  :config (vterm-anti-flicker-filter-enable))

(use-package! claude-code-ide
  :ensure t
  :bind ("C-c C-a" . claude-code-ide-menu)
  :config (setq claude-code-ide-terminal-backend 'vterm  ;; vterm has an annoying flashing

                )
  (map!
   (:leader
    ;; leaderkey bindings
    (:prefix-map ("A" . "AI")
                 ( :prefix-map ("c" . "Claude Code")
                               :desc "Claude code " "c" #'claude-code-ide-menu))))
  ;;(setq claude-code-program "/Users/ldangelo/.claude/local/claude")
  
  (claude-code-ide-emacs-tools-setup))
