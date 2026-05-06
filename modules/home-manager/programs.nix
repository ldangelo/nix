{ pkgs, misc, ... }: {
  # direnv binary comes from Homebrew; stdlib configured via xdg.configFile
  # (programs.direnv.enable pulls in broken nixpkgs direnv-2.37.1 on macOS aarch64)
  xdg.configFile."direnv/direnvrc".text = ''
    : ''${XDG_CACHE_HOME:=$HOME/.cache}
    declare -A direnv_layout_dirs
    direnv_layout_dir() {
      echo "''${direnv_layout_dirs[$PWD]:=$(
        local hash="$(sha1sum - <<<"''${PWD}" | cut -c-7)"
        local path="''${PWD//[^a-zA-Z0-9]/-}"
        echo "''${XDG_CACHE_HOME}/direnv/layouts/''${hash}''${path}"
      )}"
    }
  '';
  programs.fzf = {
    enable = true;
    enableZshIntegration = false;  # Manual integration in shell.nix with interactive check
    defaultOptions = [ "--bind ctrl-k:kill-line --color=dark" ];
  };

  programs.zsh = {
    enable = true;
    shellAliases = {
      #am    = "cd ~/mcp_agent_mail; scripts/run_server_with_token.sh";
      ag    = "ag --color-line-number='0;33' --color-path='0;32'";
      #      bd    = "br";
      cc    = "claude --chrome";
      ccc   = "claude --continue --chrome";
      cp    = "nocorrect cp";
      gm    = "git machete";
      grep  = "grep --color";
      gst   = "git status";
      j     = "julia --project --thread=auto";
      l     = "eza --all --header --long --group";
      lg    = "eza --all --header --long --group --git";
      ll    = "eza --all --header --long";
      llm   = "eza --all --header --long --sort=modified";
      lt    = "eza --tree";
      tree  = "eza --tree";
      ln    = "nocorrect ln";
      lnr   = "nocorrect ln -s --relative";
      mkdir = "nocorrect mkdir";
      mux   = "tmuxinator";
      mv    = "nocorrect mv";
      scp   = "${pkgs.rsync}/bin/rsync -aP --inplace";
      sudo  = "nocorrect sudo";
      touch = "nocorrect touch";
      which = "nocorrect which";
      vault = "cd ~/Library/Mobile\\ Documents/iCloud~md~obsidian/Documents/ldangelo && nvim .";
      ot    = "nvim ~/Library/Mobile\\ Documents/iCloud~md~obsidian/Documents/ldangelo/Daily\\ Notes/$(date +%Y-%m-%d).md";
    };

    # Powerlevel10k configuration
    initContent = ''
      [[ -f ~/.config/zsh/rc/powerlevel10k.zsh ]] && source ~/.config/zsh/rc/powerlevel10k.zsh
    '';
  };

  # User specified programs
  programs.dircolors.enable = true;
}
