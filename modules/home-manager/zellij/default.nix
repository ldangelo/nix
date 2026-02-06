{ config, pkgs, ... }:

{
  xdg.configFile."zellij/config.kdl".source = ./config.kdl;
  xdg.configFile."zellij/layouts".source = ./layouts;
  xdg.configFile."zellij/plugins/vim-zellij-navigator.wasm".source = ./plugins/vim-zellij-navigator.wasm;

  # Deploy default workspace configuration to home directory
  # Users can override this by creating .zellij-workspace in project directories
  home.file.".zellij-workspace".text = ''
    # Zellij Workspace Configuration
    # This is the default workspace file for your home directory
    # Create project-specific .zellij-workspace files in your project roots
    # Paths must be RELATIVE to where Zellij is launched (current working directory)

    # Standard layouts - relative paths from home directory
    .config/zellij/layouts/simple.kdl
    .config/zellij/layouts/editor.kdl
    .config/zellij/layouts/dev.kdl
    .config/zellij/layouts/claude_default.kdl
    .config/zellij/layouts/monitor.kdl
  '';

  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
  };
}
