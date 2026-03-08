{ pkgs,  ... }: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    history.path = "$HOME/.history";
    history.share = false;
    defaultKeymap = "viins";
    shellAliases = {
      #am    = "cd ~/mcp_agent_mail; scripts/run_server_with_token.sh";
      ag    = "ag --color-line-number='0;33' --color-path='0;32'";
      #      bd    = "br";
      cc    = "claude";
      ccc   = "claude --continue";
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


      #
      #  Set .envrc variables for common API keys (github, openrouter, anthropic, openai, etc...)
    oh-my-zsh.enable = true;
    oh-my-zsh.plugins = [
      "1password"
      "alias-finder"
      "aws"
      "docker"
      "docker-compose"
      "dotnet"
      "eza"
      "fzf"
      "gh"
      "git"
      "kubectl"
      "ruby"
      "ssh"
      "sudo"
      "terraform"
      "tmuxinator"
      "vi-mode"
      "zoxide"
    ];
    initContent = ''
      # Make tramp work (https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html)
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return


      [[ -f "/Users/ldangelo/.config/gastown/shell-hook.sh" ]] && source "/Users/ldangelo/.config/gastown/shell-hook.sh"

      # ntm (Named Tmux Manager) shell integration
      command -v ntm &>/dev/null && eval "$(ntm shell zsh)"


      # Where to look for autoloaded function definitions
      fpath=(~/.zfunc $fpath)

      # Tool completions (not covered by oh-my-zsh plugins)
      source <(jj util completion zsh 2>/dev/null)
      source <(helm completion zsh 2>/dev/null)
      source <(just --completions zsh 2>/dev/null)
      source <(glab completion -s zsh 2>/dev/null)
      source <(starship completions zsh 2>/dev/null)
      source <(atuin gen-completions --shell zsh 2>/dev/null)
      source <(git-town completions zsh 2>/dev/null)
      source <(uv generate-shell-completion zsh 2>/dev/null)
      eval "$(fd --gen-completions zsh 2>/dev/null)"
      eval "$(rg --generate complete-zsh 2>/dev/null)"
 
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

          EDITOR='nvim'
          export EDITOR

          # sudo askpass helper for non-TTY contexts (e.g. Claude Code)
          export SUDO_ASKPASS="$HOME/.local/bin/sudo-askpass"
##
          export DOTNET_ROOT="$HOME/.dotnet"

          # Let path_helper establish the base system PATH first (/usr/bin, /bin, etc.)
          # so that custom entries prepended below take priority over system paths.
          eval "$(/usr/libexec/path_helper)"
          path=("$HOME/Library/Application Support/JetBrains/Toolbox/scripts" $path)
          path=("$HOME/.npm-global/bin" $path)
          path=("$HOME/.cargo/bin" $path)
          path=("$HOME/.config/emacs/bin" $path)
          path=('/opt/homebrew/bin' $path)
          path=('/opt/homebrew/opt/gnu-tar/libexec/gnubin' $path)
          path=("$HOME/bin" $path)
          path=('/opt/homebrew/opt/python@3.13/bin/' $path)
          path=("/opt/homebrew/opt/postgresql@17/bin" $path)
          path=("$HOME/.local/bin" $path)

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

      unalias gcd 2>/dev/null
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
          path=("$HOME/.dotnet/tools" $path)
          export PATH
          eval "$(/opt/homebrew/bin/brew shellenv)"
 
    export NVM_DIR="$HOME/.nvm"
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

          NOTMUCH_CONFIG=~/.config/notmuch/default/config
          # vterm (emacs) related functions for prompt tracking, etc...
#          eval "$(oh-my-posh init zsh)"
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
            
          # alias-finder configuration
          zstyle ':omz:plugins:alias-finder' autoload yes
          zstyle ':omz:plugins:alias-finder' longer yes
          zstyle ':omz:plugins:alias-finder' exact yes
          zstyle ':omz:plugins:alias-finder' cheaper yes

            if [[ -z "$INSIDE_EMACS" ]]; then
              source ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
              source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
            fi
      #
      # Setup ROS 2 auto completion for nix-direnv environments
      eval "$(${pkgs.python3Packages.argcomplete}/bin/register-python-argcomplete ros2)"
      eval "$(${pkgs.python3Packages.argcomplete}/bin/register-python-argcomplete colcon)"

      # Integrate run-nix-help (https://github.com/NixOS/nix/blob/master/misc/zsh/run-help-nix#L14)
      (( $+aliases[run-help] )) && unalias run-help
      autoload -Uz run-help run-help-nix

      # Re-assert priority paths last — brew shellenv (called above) internally
      # re-invokes path_helper which pushes /usr/bin and /bin back to the front.
      path=(
        '/opt/homebrew/opt/postgresql@17/bin'
        '/opt/homebrew/opt/python@3.13/bin'
        "$HOME/bin"
        '/opt/homebrew/bin'
        "$HOME/.config/emacs/bin"
        "$HOME/.cargo/bin"
        "$HOME/.npm-global/bin"
        "$HOME/Library/Application Support/JetBrains/Toolbox/scripts"
        "$HOME/.local/bin"
        $path
      )
      export PATH

fi

    '';
  };
}
