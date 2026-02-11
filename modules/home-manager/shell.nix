{ pkgs,  ... }: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    history.path = "$HOME/.history";
    history.share = false;
    defaultKeymap = "emacs";
    shellAliases = {
      ag    = "ag --color-line-number='0;33' --color-path='0;32'";
      cc    = "claude";
      ccc   = "claude --continue";
      cp    = "nocorrect cp"; # no spelling correction on cp
      gm    = "git machete";
      grep  = "grep --color";
      gst   = "git status";
      j     = "julia --project --thread=auto";
      jc    = "journalctl";
      l     = "exa -la --group --header";
      lg    = "exa -la --group --header --git";
      ln    = "nocorrect ln"; # no spelling correction on ln
      lnr   = "nocorrect ln -s --relative";
      ls    = "ls --color=auto";
      lsa   = "ls -ld .*"; # List only file beginning with "."
      lsd   = "ls -ld *(-/DN)"; # List only directories and symbolic links that point to directories
      mkdir = "nocorrect mkdir"; # no spelling correction on mkdir
      mv    = "nocorrect mv"; # no spelling correction on mv
      o     = "octave -f --no-gui";
      r     = "ranger_cd";
      sc    = "systemctl";
      scp   = "${pkgs.rsync}/bin/rsync -aP --inplace";
      scs   = "systemctl status";
      scu   = "systemctl --user";
      scus  = "scu status";
      sudo  = "nocorrect sudo";
      touch = "nocorrect touch";
      which = "nocorrect which";
    };

# Set .envrc variables for common API keys (github, openrouter, anthropic, openai, etc...)
    oh-my-zsh.enable = true;
    oh-my-zsh.plugins = [ "systemd" ];
    initContent = ''
      # Make tramp work (https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html)
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

      # Where to look for autoloaded function definitions
      fpath=(~/.zfunc $fpath)
 
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

          EDITOR='nvim'
          export EDITOR
##
          export DOTNET_ROOT=/Users/ldangelo/.dotnet

          path=('/Users/ldangelo/.local/bin' $path)
          path=("$HOME/Library/Application Support/JetBrains/Toolbox/scripts" $path)
          path=('/Users/ldangelo/.npm-global/bin' $path)
          path=('/Users/ldangelo/.cargo/bin' $path)
          path=('/Users/ldangelo/.config/emacs/bin' $path)
          path=('/Users/ldangelo/Development/omnisharp' $path)
          path=('/opt/homebrew/bin' $path)
          path=('/Users/ldangelo/bin' $path)
          path=('/Users/ldangelo/.config/doom/bin' $path)
          path=('/opt/homebrew/opt/python@3.13/bin/' $path)
          eval "$(/usr/libexec/path_helper)"
          path=("/opt/homebrew/opt/postgresql@17/bin" $path)

          DIRSTACKSIZE=100

          setopt notify interactivecomments recexact longlistjobs
          setopt autoresume pushdsilent autopushd pushdminus

          d() {
            local dir
            dir=$(dirs -l -p | fzf +m) &&
            cd $dir
          }

          # Claude Code session picker with fzf
          cccs() {
            local session
            session=$(
              for project_dir in ~/.claude/projects/*/; do
                project_name=$(basename "$project_dir" | sed 's/^-//' | tr '-' '/')
                for session_file in "$project_dir"*.jsonl; do
                  [ -f "$session_file" ] || continue
                  session_id=$(basename "$session_file" .jsonl)
                  first_msg=$(grep '"type":"user"' "$session_file" 2>/dev/null | head -1 | jq -r '.message.content // empty' 2>/dev/null | head -c 80 | tr '\n' ' ')
                  timestamp=$(grep '"type":"user"' "$session_file" 2>/dev/null | head -1 | jq -r '.timestamp // empty' 2>/dev/null | cut -d'T' -f1)
                  [ -n "$first_msg" ] && echo -e "$session_id\t$timestamp\t$project_name\t$first_msg"
                done
              done | sort -t$'\t' -k2 -r | fzf --delimiter='\t' --with-nth=2,3,4 --preview-window=hidden
            )
            [ -n "$session" ] && claude --resume "$(echo "$session" | cut -f1)"
          }

      gcd() {
        local dir="$(git ls-tree -d -r --name-only --full-name HEAD $(git rev-parse --show-cdup) | fzf +m -0)" &&
        cd "./$(git rev-parse --show-cdup)/$dir"
      }

          # FZF initialization (only in interactive shells)
          if [[ -o interactive ]]; then
            source <(/etc/profiles/per-user/ldangelo/bin/fzf --zsh)

            bindkey '\eC' fzf-cd-widget
            bindkey '\ec' capitalize-word
          fi

          # Restore Alt-L behaviour overridden by Oh-My-Zsh (https://github.com/ohmyzsh/ohmyzsh/issues/5071)
          bindkey '^[l' down-case-word

          ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

          # Source localhost specific settings
          source ~/.zshrc.local

          autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
          add-zsh-hook chpwd chpwd_recent_dirs
          zstyle ':completion:*:*:cdr:*:*' menu selection

          autoload bashcompinit
          bashcompinit

          ZSH_BASH_COMPLETIONS_FALLBACK_PATH=/opt/homebrew/share/bash-completion

  # NVM initialization (only in interactive shells)
  if [[ -o interactive ]]; then
        eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
          path=($DOTNET_ROOT $path)
          path=('/Users/ldangelo/.dotnet/tools' $path)
          export PATH
          eval "$(/opt/homebrew/bin/brew shellenv)"
 
    export NVM_DIR="$HOME/.nvm"
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

          bindkey -v
          NOTMUCH_CONFIG=~/.config/notmuch/default/config
          # vterm (emacs) related functions for prompt tracking, etc...
#          eval "$(oh-my-posh init zsh)"
          source ~/.config/zsh/rc/alias.zsh
          source ~/.config/zsh/rc/homebrew.zsh
          source ~/.config/zsh/rc/modules.zsh
          source ~/.config/zsh/rc/set.zsh
          source ~/.config/zsh/rc/unset.zsh
          source ~/.config/zsh/rc/misc.zsh
#          source ~/.config/zsh/rc/java.zsh
          source ~/.config/zsh/rc/rbenv.zsh
          source ~/.config/zsh/rc/binds.zsh

          # Only load interactive plugins in interactive mode
            source ~/.config/zsh/rc/atuin.zsh
            source ~/.config/zsh/rc/comp.zsh
            source ~/.config/zsh/rc/fzf-tab.zsh
            source ~/.config/zsh/rc/vterm.zsh
            source ~/.config/zsh/rc/starship.zsh
            test -e "$HOME/.iterm2_shell_integration.zsh" && source "$HOME/.iterm2_shell_integration.zsh"
            test -d "$HOME/.iterm2" && export PATH="$HOME/.iterm2:$PATH"
            
            if [[ -z "$INSIDE_EMACS" ]]; then
#             source ~/.config/zsh/rc/zsh-autosuggestions.zsh
#             source ~/.config/zsh/rc/zsh-syntax-highlighting.zsh
              source ~/.config/zsh/rc/ohmyzsh.zsh
            fi
      #
      # Setup ROS 2 auto completion for nix-direnv environments
      eval "$(${pkgs.python3Packages.argcomplete}/bin/register-python-argcomplete ros2)"
      eval "$(${pkgs.python3Packages.argcomplete}/bin/register-python-argcomplete colcon)"

      # Integrate run-nix-help (https://github.com/NixOS/nix/blob/master/misc/zsh/run-help-nix#L14)
      (( $+aliases[run-help] )) && unalias run-help
      autoload -Uz run-help run-help-nix

fi

    '';
  };
}
