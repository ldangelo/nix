{ options, config, lib, pkgs, namespace, inputs, ... }:
with lib;
with lib.${namespace};
let

  cfg = config.${namespace}.programs.terminal.shells.zsh;
in {
  options.${namespace}.programs.terminal.shells.zsh = with types; {
    enable = mkEnableOption "ZSH";
    PATH = mkOpt types.str "";
    rcFiles = mkOpt types.str "";
  };

  config = mkIf cfg.enable {
    programs = {
      zsh = {
        enable = true;

        package = pkgs.zsh;

        autocd = true;
        autosuggestion.enable = true;
        dotDir = ".config/zsh";
        enableCompletion = true;
        enableVteIntegration = true;

        # Disable /etc/{zshrc,zprofile} that contains the "sane-default" setup out of the box
        # in order avoid issues with incorrect precedence to our own zshrc.
        # See `/etc/zshrc` for more info.
        envExtra = ''
          setopt no_global_rcs
        '';                     #

        history = {
          # share history between different zsh sessions
          share = true;

          # avoid cluttering $HOME with the histfile
          path = "${config.xdg.dataHome}/zsh/zsh_history";

          # saves timestamps to the histfile
          extended = true;

          # optimize size of the histfile by avoiding duplicates
          # or commands we don't need remembered
          save = 100000;
          size = 100000;
          expireDuplicatesFirst = true;
          ignoreDups = true;
          ignoreSpace = true;
        };

        sessionVariables = {
          LC_ALL = "en_US.UTF-8";
          KEYTIMEOUT = 0;
        };

        syntaxHighlighting = {
          enable = true;
          package = pkgs.zsh-syntax-highlighting;
        };

    oh-my-zsh.enable = true;
    oh-my-zsh.plugins = [
      "1password"
      "fzf"
      "keychain"
      "podman"
      "thefuck"
      "systemd"
      "zoxide"
    ];
    initExtraBeforeCompInit = ''
      # Make tramp work (https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html)
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

      # Where to look for autoloaded function definitions
      fpath=(~/.zfunc $fpath)
    '';
    initExtra = ''
      DIRSTACKSIZE=100

      setopt notify interactivecomments recexact longlistjobs
      setopt autoresume pushdsilent autopushd pushdminus

      d() {
        local dir
        dir=$(dirs -l -p | fzf +m) &&
        cd $dir
      }

      gcd() {
        local dir="$(git ls-tree -d -r --name-only --full-name HEAD $(git rev-parse --show-cdup) | fzf +m -0)" &&
        cd "./$(git rev-parse --show-cdup)/$dir"
      }

      # Rebind fzf-cd to a sane key
      bindkey '\eC' fzf-cd-widget
      bindkey '\ec' capitalize-word

      # Restore Alt-L behaviour overridden by Oh-My-Zsh (https://github.com/ohmyzsh/ohmyzsh/issues/5071)
      bindkey '^[l' down-case-word

      source ${pkgs.mc}/libexec/mc/mc.sh
      # if [[ -f /usr/share/mc/bin/mc.sh ]]; then
      #     source /usr/share/mc/bin/mc.sh
      # else
      #     if [[ -f /usr/libexec/mc/mc-wrapper.sh ]]; then
      #         alias mc='. /usr/libexec/mc/mc-wrapper.sh'
      #     fi
      # fi

      # # Autoload all shell functions from all directories in $fpath (following
      # # symlinks) that have the executable bit on (the executable bit is not
      # # necessary, but gives you an easy way to stop the autoloading of a
      # # particular shell function). $fpath should not be empty for this to work.
      # for func in $^fpath/*(N-.x:t); autoload $func

      source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

      # Source localhost specific settings
      source ~/.zshrc.local

      autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
      add-zsh-hook chpwd chpwd_recent_dirs
      zstyle ':completion:*:*:cdr:*:*' menu selection

      # autoload bashcompinit
      # bashcompinit

      vterm_printf(){
          if [ -n "$TMUX" ]; then
              # Tell tmux to pass the escape sequences through
              # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }

      # Finally include zsh snippets
#      for zshrc_snipplet in ~/.zshrc.d/S[0-9][0-9]*[^~] ; do
#        source $zshrc_snipplet
#      done

      ZSH_BASH_COMPLETIONS_FALLBACK_PATH=${pkgs.bash-completion}/share/bash-completion
      #ZSH_BASH_COMPLETIONS_FALLBACK_WHITELIST=(openssl)

      ranger_cd() {
          temp_file="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
          ranger --choosedir="$temp_file" -- "''${@:-$PWD}"
          if chosen_dir="$(cat -- "$temp_file")" && [ -n "$chosen_dir" ] && [ "$chosen_dir" != "$PWD" ]; then
              cd -- "$chosen_dir"
          fi
          rm -f -- "$temp_file"
      }

      if test -n "$KITTY_INSTALLATION_DIR"; then
          export KITTY_SHELL_INTEGRATION="enabled"
          autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
          kitty-integration
          unfunction kitty-integration
      fi


      # Setup ROS 2 auto completion for nix-direnv environments
      eval "$(${pkgs.python3Packages.argcomplete}/bin/register-python-argcomplete ros2)"
      eval "$(${pkgs.python3Packages.argcomplete}/bin/register-python-argcomplete colcon)"

      # Integrate run-nix-help (https://github.com/NixOS/nix/blob/master/misc/zsh/run-help-nix#L14)
      (( $+aliases[run-help] )) && unalias run-help
      autoload -Uz run-help run-help-nix

      NOTMUCH_CONFIG=~/.config/notmuch/default/config
      # vterm (emacs) related functions for prompt tracking, etc...
      source ~/.vterm/emacs-vterm-zsh.sh
      '';
      #    initExtra = ''
      #       # avoid duplicated entries in PATH
      #       typeset -U PATH

      #       # try to correct the spelling of commands
      #       setopt correct
      #       # autosuggests otherwise breaks these widgets.
      #       # <https://github.com/zsh-users/zsh-autosuggestions/issues/619>
      #       ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(history-beginning-search-backward-end history-beginning-search-forward-end)

      #       # Do this early so fast-syntax-highlighting can wrap and override this
      #       if autoload history-search-end; then
      #         zle -N history-beginning-search-backward-end history-search-end
      #         zle -N history-beginning-search-forward-end  history-search-end
      #       fi

      #       source <(${lib.getExe config.programs.fzf.package} --zsh)
      #       source ${config.programs.git.package}/share/git/contrib/completion/git-prompt.sh
      #       # Prevent the command from being written to history before it's
      #       # executed; save it to LASTHIST instead.  Write it to history
      #       # in precmd.
      #       #
      #       # called before a history line is saved.  See zshmisc(1).
      #       function zshaddhistory() {
      #         # Remove line continuations since otherwise a "\" will eventually
      #         # get written to history with no newline.
      #         LASTHIST=''${1//\\$'\n'/}
      #         # Return value 2: "... the history line will be saved on the internal
      #         # history list, but not written to the history file".
      #         return 2
      #       }

      #       # zsh hook called before the prompt is printed.  See zshmisc(1).
      #       function precmd() {
      #         # Write the last command if successful, using the history buffered by
      #         # zshaddhistory().
      #         if [[ $? == 0 && -n ''${LASTHIST//[[:space:]\n]/} && -n $HISTFILE ]] ; then
      #           print -sr -- ''${=''${LASTHIST%%'\n'}}
      #         fi
      #       }

      #       ${fileContents ./rc/unset.zsh}
      #       ${fileContents ./rc/set.zsh}

      #       # binds, zsh modules and everything else
      #       ${fileContents ./rc/binds.zsh}
      #       ${fileContents ./rc/modules.zsh}
      #       ${fileContents ./rc/fzf-tab.zsh}
      #       ${fileContents ./rc/misc.zsh}
      #       ${fileContents ./rc/vterm.zsh}

      #       # Set LS_COLORS by parsing dircolors output
      #       LS_COLORS="$(${pkgs.coreutils}/bin/dircolors --sh)"
      #       LS_COLORS="''${''${LS_COLORS#*\'}%\'*}"
      #       export LS_COLORS

      #       ${lib.optionalString config.programs.fastfetch.enable "fastfetch"}

      #       # enable oh-my-posh prompt
      #       eval "$(oh-my-posh prompt init zsh --config ~/.config/oh-my-posh/atomic.omp.json)"
      #     '';

      #   plugins = [
      #     {
      #       # Must be before plugins that wrap widgets, such as zsh-autosuggestions or fast-syntax-highlighting
      #       name = "fzf-tab";
      #       file = "share/fzf-tab/fzf-tab.plugin.zsh";
      #       src = pkgs.zsh-fzf-tab;
      #     }
      #     {
      #       name = "zsh-nix-shell";
      #       file = "share/zsh-nix-shell/nix-shell.plugin.zsh";
      #       src = pkgs.zsh-nix-shell;
      #     }
      #     {
      #       name = "zsh-vi-mode";
      #       src = pkgs.zsh-vi-mode;
      #       file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
      #     }
      #     {
      #       name = "fast-syntax-highlighting";
      #       file =
      #         "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh";
      #       src = pkgs.zsh-fast-syntax-highlighting;
      #     }
      #     {
      #       name = "zsh-autosuggestions";
      #       file = "share/zsh-autosuggestions/zsh-autosuggestions.zsh";
      #       src = pkgs.zsh-autosuggestions;
      #     }
      #     {
      #       name = "zsh-better-npm-completion";
      #       src = pkgs.zsh-better-npm-completion;
      #     }
      #     {
      #       name = "zsh-command-time";
      #       src = pkgs.zsh-command-time;
      #     }
      #     {
      #       name = "zsh-history-to-fish";
      #       src = pkgs.zsh-history-to-fish;
      #     }
      #     {
      #       name = "zsh-you-should-use";
      #       src = pkgs.zsh-you-should-use;
      #     }
      #   ];
      };
    };
  };
}
