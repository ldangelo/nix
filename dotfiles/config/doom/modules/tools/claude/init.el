(use-package! vterm-anti-flicker-filter
  :ensure t
  :config (vterm-anti-flicker-filter-enable))

(use-package! claude-code-ide
  :ensure t
  :bind ("C-c C-a" . claude-code-ide-menu)
  :config (setq claude-code-ide-terminal-backend 'vterm  ;; vterm has an annoying flashing

                )
  (dolist (range '((#x23FA . #x23FA)   ; ⏺ bullet
                   (#x2700 . #x27BF)   ; Dingbats (spinner chars ✢✳✻✽)
                   (#x2200 . #x22FF))) ; Math operators (∗)
    (set-char-table-range char-width-table range 1))

  (map!
   (:leader
    ;; leaderkey bindings
    (:prefix-map ("A" . "AI")
                 ( :prefix-map ("c" . "Claude Code")
                               :desc "Claude code " "c" #'claude-code-ide-menu))))
  ;;(setq claude-code-program "/Users/ldangelo/.claude/local/claude")
  (defun my/setup-claude-fontset ()
    (set-fontset-font t '(#x23FA . #x23FA) "STIX Two Math"))
  (my/setup-claude-fontset)
  (add-hook 'after-setting-font-hook #'my/setup-claude-fontset) 

  (claude-code-ide-emacs-tools-setup))
