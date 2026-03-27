{ pkgs, ... }:

{
  programs.lazygit = {
    enable = true;
    settings = {
      gui = {
        # Side-by-side diffs instead of inline
        sideBySideDiff = true;

        # Show file tree in status panel
        showFileTree = true;

        # Show divergence from base branch in branches panel
        showDivergenceFromBaseBranch = "arrowAndNumber";

        # Expand focused side panel for more room
        expandFocusedSidePanel = true;

        # Mouse support
        mouseEvents = true;

        # Show random tip on startup
        showRandomTip = false;

        # Nerd font icons
        nerdFontsVersion = "3";

        # Border style
        border = "rounded";

        # Animated loader
        animateExplosion = false;
      };

      git = {
        # Use delta as pager for better diffs (array format)
        pagers = [
          {
            colorArg = "always";
            command = "diff";
            pager = "delta --dark --paging=never --side-by-side --line-numbers";
          }
          {
            colorArg = "always";
            command = "log";
            pager = "delta --dark --paging=never";
          }
          {
            colorArg = "always";
            command = "reflog";
            pager = "delta --dark --paging=never";
          }
          {
            colorArg = "always";
            command = "show";
            pager = "delta --dark --paging=never --side-by-side --line-numbers";
          }
        ];

        # Auto-fetch every 60 seconds
        autoFetch = true;
        autoRefresh = true;

        # Show whole graph in log
        log = {
          showGraph = "always";
          showWholeGraph = false;
        };

        # Parse emoji in commits
        parseEmoji = true;
      };

      # Confirmation prompts
      promptToReturnFromSubprocess = false;

      # Custom commands
      customCommands = [
        {
          # jj status (since this repo uses jj)
          key = "J";
          command = "jj st";
          context = "global";
          description = "Jujutsu status";
          output = "popup";
        }
      ];

      os = {
        # Use neovim for editing
        editPreset = "nvim";
      };
    };
  };
}
