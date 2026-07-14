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
      bell  = "printf '\\a'";
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
      mux   = "tmuxp";
      muxi  = "tmuxinator";
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

  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "https";
      prompt = "enabled";
      prefer_editor_prompt = "disabled";
      aliases.co = "pr checkout";
      color_labels = "disabled";
      accessible_colors = "disabled";
      accessible_prompter = "disabled";
      spinner = "enabled";
    };
    gitCredentialHelper.enable = false;
    extensions = with pkgs; [
      gh-enhance
    ];
  };

  programs.gh-dash = {
    enable = true;
    settings = {
      prSections = [
        {
          title = "My Pull Requests";
          filters = "is:open author:@me";
        }
        {
          title = "Needs My Review";
          filters = "is:open review-requested:@me";
        }
        {
          title = "Involved";
          filters = "is:open involves:@me -author:@me";
        }
      ];
      issuesSections = [
        {
          title = "My Issues";
          filters = "is:open author:@me";
        }
        {
          title = "Assigned";
          filters = "is:open assignee:@me";
        }
        {
          title = "Involved";
          filters = "is:open involves:@me -author:@me";
        }
      ];
      notificationsSections = [
        {
          title = "All";
          filters = "";
        }
        {
          title = "Created";
          filters = "reason:author";
        }
        {
          title = "Participating";
          filters = "reason:participating";
        }
        {
          title = "Mentioned";
          filters = "reason:mention";
        }
        {
          title = "Review Requested";
          filters = "reason:review-requested";
        }
        {
          title = "Assigned";
          filters = "reason:assign";
        }
        {
          title = "Subscribed";
          filters = "reason:subscribed";
        }
        {
          title = "Team Mentioned";
          filters = "reason:team-mention";
        }
      ];
      repo = {
        branchesRefetchIntervalSeconds = 30;
        prsRefetchIntervalSeconds = 60;
      };
      defaults = {
        preview = {
          open = true;
          width = 0.45;
          height = 0.6;
          position = "auto";
        };
        prsLimit = 20;
        prApproveComment = "LGTM";
        issuesLimit = 20;
        notificationsLimit = 20;
        view = "prs";
        layout = {
          prs = {
            updatedAt.width = 5;
            createdAt.width = 5;
            repo.width = 20;
            author.width = 15;
            authorIcon.hidden = false;
            labels = {
              width = 22;
              hidden = true;
            };
            assignees = {
              width = 20;
              hidden = true;
            };
            base = {
              width = 15;
              hidden = true;
            };
            lines.width = 15;
          };
          issues = {
            updatedAt.width = 5;
            createdAt.width = 5;
            repo.width = 15;
            creator.width = 10;
            creatorIcon.hidden = false;
            assignees = {
              width = 20;
              hidden = true;
            };
          };
        };
        refetchIntervalMinutes = 30;
      };
      keybindings = {};
      repoPaths = {};
      theme.ui = {
        sectionsShowCount = true;
        table = {
          showSeparator = true;
          compact = false;
        };
      };
      pager.diff = "diffnav";
      confirmQuit = false;
      showAuthorIcons = true;
      smartFilteringAtLaunch = true;
      includeReadNotifications = true;
    };
  };

  xdg.configFile."gh/config.yml".force = true;
  xdg.configFile."gh-dash/config.yml".force = true;
  xdg.dataFile."gh/extensions".force = true;

  # User specified programs
  programs.dircolors.enable = true;
}
