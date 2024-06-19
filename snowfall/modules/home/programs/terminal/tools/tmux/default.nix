{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
let
  inherit (lib) mkIf;
  inherit (lib.${namespace}) mkBoolOpt;

  tmux-autoreload = pkgs.tmuxPlugins.mkTmuxPlugin
    {
     pluginName = "tmux-autoreload";
     version = "unstable-2023-1-2";
     src = pkgs.fetchFromGitHub {
       owner = "b0o";
       repo = "tmux-autoreload";
       rev = "main";
       sha256 = "sha256-9Rk+VJuDqgsjc+gwlhvX6uxUqpxVD1XJdQcsc5s4pU4=";
     };
    };
  tome = pkgs.tmuxPlugins.mkTmuxPlugin
    {
     pluginName = "tome";
     version = "unstable-2023-1-1";
     src = pkgs.fetchFromGitHub {
       owner = "laktak";
       repo = "tome";
       rev = "master";
       sha256 = "sha256-ShBTFwOBRyybpLNmtx5/juTnssPjiXqBzH6sYYsVvnE=";
     };
    };
  cfg = config.${namespace}.programs.terminal.tools.tmux;
  configFiles = lib.snowfall.fs.get-files ./config;

  plugins = with pkgs.tmuxPlugins; [
    { plugin = tmux-autoreload; }
    { plugin = tome; }
    sensible
    yank
    {
      plugin = dracula;
      extraConfig = ''
				set -g @dracula-show-battery false
				set -g @dracula-show-powerline true
				set -g @dracula-refresh-rate 10
 '';
    }
    {
      plugin = resurrect;
      extraConfig = ''
        set -g @resurrect-strategy-vim 'session'
        set -g @resurrect-strategy-nvim 'session'
        set -g @resurrect-capture-pane-contents 'on'
        set -g @resurrect-processes 'ssh lazygit yazi'
        set -g @resurrect-dir '~/.tmux/resurrect'
      '';
    }
    {
      plugin = continuum;
      extraConfig = ''
        set -g @continuum-restore 'on'
      '';
    }
    { plugin = tmux-fzf; }
    { plugin = vim-tmux-navigator; }
  ];
in
{
  options.${namespace}.programs.terminal.tools.tmux = {
    enable = mkBoolOpt false "Whether or not to enable tmux.";
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      aggressiveResize = true;
      baseIndex = 1;
      clock24 = true;
      escapeTime = 0;
      historyLimit = 10000;
      keyMode = "vi";
      mouse = true;
      newSession = true;
      prefix = "C-a";
      sensibleOnTop = true;
      tmuxinator.enable = true;
      terminal = "xterm-256color";
      extraConfig = builtins.concatStringsSep "\n" (builtins.map lib.strings.fileContents configFiles);

      inherit plugins;
    };
xdg.configFile."tmux/plugins" = {
      source = lib.cleanSourceWith { src = lib.cleanSource ./plugins/.; };
      recursive = true;
    };

  };
}
