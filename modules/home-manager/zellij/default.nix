{ config, pkgs, ... }:

{
  xdg.configFile."zellij/config.kdl".source = ./config.kdl;
  xdg.configFile."zellij/layouts".source = ./layouts;

  # Deploy default workspace configuration to home directory
  # Users can override this by creating .zellij-workspace in project directories
  home.file.".zellij-workspace".text = ''
    # Zellij Workspace Configuration
    # This is the default workspace file for your home directory
    # Create project-specific .zellij-workspace files in your project roots
    # Paths should be absolute (starting with /) or relative to where Zellij is launched

    # Standard layouts (absolute paths work from anywhere)
    ${config.home.homeDirectory}/.config/zellij/layouts/simple.kdl
    ${config.home.homeDirectory}/.config/zellij/layouts/editor.kdl
    ${config.home.homeDirectory}/.config/zellij/layouts/dev.kdl
    ${config.home.homeDirectory}/.config/zellij/layouts/claude_default.kdl
    ${config.home.homeDirectory}/.config/zellij/layouts/monitor.kdl
  '';

  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
  };
}
