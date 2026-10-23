# 💤 LazyVim


A starter template for [LazyVim](https://github.com/LazyVim/LazyVim).
Refer to the [documentation](https://lazyvim.github.io/installation) to get started.

## CodeCompanion Plugin

This configuration includes the [olimorris/codecompanion.nvim](https://github.com/olimorris/codecompanion.nvim) plugin for AI-powered programming assistance.

### Official Documentation
  
  For full documentation, advanced configuration, and troubleshooting, visit: [CodeCompanion Documentation](https://codecompanion.olimorris.dev/)
  
  #### Quick Reference
  
  **Adapters & Environment Setup**
  - Configure adapters for different AI models (e.g., Claude, OpenAI) in your plugin options.
  - Set required environment variables (e.g., `ANTHROPIC_API_KEY`) for API access.
  
  **Slash Commands**
  - `/buffer` — Interact with the current buffer.
  - `/fetch` — Retrieve external context or code.
  - `/help` — Get help and usage instructions.
  - `/image` — Insert or fetch images from specified directories.
  
  **Keymap Customization**
  - Keymaps for sending prompts, completions, and executing slash commands can be customized in your config.
  
  **Integration Tips**
  - CodeCompanion integrates with status plugins (e.g., `fidget.nvim`) and pickers (e.g., `fzf-lua`, `mini.pick`).
  - You can extend or override adapters and slash commands for advanced workflows.
  
  **Troubleshooting**
  - Ensure all required environment variables are set.
  - Check the official docs for FAQs and common issues.
  - Use debug logging (`log_level = "DEBUG"`) for detailed output.
  
  **Best Practices**
  - Keep your configuration modular for maintainability.
  - Regularly update plugins and review breaking changes in the official docs.
  
  ---

### Setup
- Ensure you have an Anthropic API key set in your environment:
  ```sh
  export ANTHROPIC_API_KEY=your_api_key_here
  ```

### Commands
- `:CodeCompanion` — Open CodeCompanion
- `:CodeCompanionChat` — Start or toggle a chat buffer
- `:CodeCompanionActions` — Open the action palette

### Keymaps
- `<C-a>` — Open the action palette (normal/visual mode)
- `<Leader>a` — Toggle a chat buffer (normal/visual mode)
- `<LocalLeader>a` — Add code to a chat buffer (visual mode)

### Configuration
See `lua/plugins/codecompanion/init.lua` for advanced options, adapters, and custom slash commands.

### Troubleshooting
- If you see a warning about a missing `ANTHROPIC_API_KEY`, set the environment variable as shown above.

---
