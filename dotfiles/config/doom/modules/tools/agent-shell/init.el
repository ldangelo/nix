(use-package  agent-shell
  :defer t
  :init
  (setq agent-shell-default-shell 'eshell)
  (setq agent-shell-eshell-command "eshell")
  (setq agent-shell-zsh-command "zsh")
  (setq agent-shell-bash-command "bash")
  (setq agent-shell-fish-command "fish")
  (setq agent-shell-term-command "term")
  (setq agent-shell-term-term-name "eterm-color")
  (setq agent-shell-term-program "/bin/bash")
  (setq agent-shell-term-args '("-c" "exec bash"))
  (setq agent-shell-anothropic-autentication (agent-shell-anthropic-make-authentication :api-key (lambda () (get-env "ANTOPIC_API_KEY"))))
  :config
  (agent-shell-mode 1)
  ;; Optional: Enable acp for auto-completion in agent shells
  (use-package acp
    :after agent-shell
    :config
    (add-hook 'agent-shell-mode-hook 'acp-mode)))
