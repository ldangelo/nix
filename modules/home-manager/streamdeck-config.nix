# Leo's Stream Deck Configuration
# Import this in your home-manager config: imports = [ ./streamdeck-config.nix ];

{ config, lib, pkgs, ... }:

{
  programs.streamdeck = {
    enable = true;

    # Device info from existing profile
    deviceId = "@(1)[4057/128/DL51K1A60852]";
    deviceModel = "20GBA9901";

    profiles.main = {
      name = "Leo's Profile";

      pages = {
        # Main page
        default = {
          buttons = {
            # Row 0: Quick Actions
            "0,0" = {
              name = "Apps";
              type = "folder";
              title = "Apps";
              # icon = ./icons/folder.png;
            };

            "1,0" = {
              name = "Links";
              type = "folder";
              title = "Links";
            };

            "2,0" = {
              name = "Screenshot";
              type = "hotkey";
              title = "Screen\n\n\n\nShot";
              keys = [ "cmd" "shift" "4" ];
            };

            # Row 1: Meeting Controls
            "0,1" = {
              name = "Tutorials";
              type = "folder";
              title = "Tutorials";
            };

            "1,1" = {
              name = "Emojis";
              type = "folder";
              title = "Emojis";
            };

            "2,1" = {
              name = "ScreenRecord";
              type = "hotkey";
              title = "Screen\n\n\n\nRecord";
              keys = [ "cmd" "shift" "5" ];
            };

            # Row 2: Media Controls
            "0,2" = {
              name = "Mute";
              type = "multimedia";
              actionIdx = 4; # Mute
            };

            "1,2" = {
              name = "VolumeUp";
              type = "multimedia";
              actionIdx = 6; # Volume Up
            };

            "2,2" = {
              name = "VolumeDown";
              type = "multimedia";
              actionIdx = 5; # Volume Down
            };

            # Navigation
            "4,2" = {
              name = "NextPage";
              type = "nextPage";
            };
          };
        };

        # Apps page
        apps = {
          name = "Apps";
          buttons = {
            "0,0" = {
              name = "Back";
              type = "backToParent";
            };

            "0,1" = {
              name = "Mail";
              type = "open";
              path = "/System/Applications/Mail.app";
            };

            "0,2" = {
              name = "Calculator";
              type = "open";
              path = "/System/Applications/Calculator.app";
            };

            "1,1" = {
              name = "Chrome";
              type = "open";
              path = "/Applications/Google Chrome.app";
            };

            "1,2" = {
              name = "FaceTime";
              type = "open";
              path = "/System/Applications/FaceTime.app";
            };

            "2,0" = {
              name = "Finder";
              type = "open";
              path = "/";
            };

            "2,1" = {
              name = "Firefox";
              type = "open";
              path = "/Applications/Firefox.app";
            };

            "2,2" = {
              name = "Messages";
              type = "open";
              path = "/System/Applications/Messages.app";
            };

            "3,1" = {
              name = "Safari";
              type = "open";
              path = "/Applications/Safari.app";
            };

            "3,2" = {
              name = "Notes";
              type = "open";
              path = "/System/Applications/Notes.app";
            };

            "4,1" = {
              name = "StreamDeck";
              type = "open";
              path = "/Applications/Elgato Stream Deck.app";
              fontSize = 9;
            };

            "4,2" = {
              name = "Chess";
              type = "open";
              path = "/System/Applications/Chess.app";
            };
          };
        };

        # Links page
        links = {
          name = "Links";
          buttons = {
            "0,0" = {
              name = "Back";
              type = "backToParent";
            };

            "1,0" = {
              name = "Elgato";
              type = "website";
              url = "https://elgato.com";
            };

            "1,1" = {
              name = "YouTube";
              type = "website";
              url = "https://youtube.com";
            };

            "2,0" = {
              name = "Marketplace";
              type = "website";
              url = "https://marketplace.elgato.com/";
              fontSize = 10;
            };

            "2,1" = {
              name = "Twitch";
              type = "website";
              url = "https://twitch.tv";
            };

            "3,0" = {
              name = "Corsair";
              type = "website";
              url = "https://corsair.com";
            };

            "3,1" = {
              name = "Twitter";
              type = "website";
              url = "https://x.com/";
            };

            "4,0" = {
              name = "Help";
              type = "website";
              url = "https://help.elgato.com";
              fontSize = 11;
            };

            "4,1" = {
              name = "TikTok";
              type = "website";
              url = "https://tiktok.com";
            };
          };
        };
      };
    };

    # Alternative: Custom work profile
    profiles.work = {
      name = "Work Profile";

      pages.default = {
        buttons = {
          # Row 0: Communication
          "0,0" = {
            name = "Slack";
            type = "open";
            path = "/Applications/Slack.app";
            # icon = ./icons/slack.png;
          };

          "1,0" = {
            name = "Zoom";
            type = "open";
            path = "/Applications/zoom.us.app";
          };

          "2,0" = {
            name = "Teams";
            type = "open";
            path = "/Applications/Microsoft Teams.app";
          };

          "3,0" = {
            name = "Mail";
            type = "open";
            path = "/System/Applications/Mail.app";
          };

          "4,0" = {
            name = "Calendar";
            type = "open";
            path = "/Applications/Fantastical.app";
          };

          # Row 1: Development
          "0,1" = {
            name = "Terminal";
            type = "open";
            path = "/Applications/Ghostty.app";
          };

          "1,1" = {
            name = "Cursor";
            type = "open";
            path = "/Applications/Cursor.app";
          };

          "2,1" = {
            name = "GitHub";
            type = "website";
            url = "https://github.com";
          };

          "3,1" = {
            name = "Obsidian";
            type = "website";
            url = "obsidian://open?vault=Curantis";
            openInBrowser = false;
          };

          "4,1" = {
            name = "Todoist";
            type = "website";
            url = "https://todoist.com/app";
          };

          # Row 2: Media & Navigation
          "0,2" = {
            name = "Prev";
            type = "hotkey";
            keys = [ "f7" ];
          };

          "1,2" = {
            name = "Play";
            type = "hotkey";
            keys = [ "f8" ];
          };

          "2,2" = {
            name = "Next";
            type = "hotkey";
            keys = [ "f9" ];
          };

          "3,2" = {
            name = "Mute";
            type = "multimedia";
            actionIdx = 4;
          };

          "4,2" = {
            name = "Lock";
            type = "hotkey";
            keys = [ "ctrl" "cmd" "q" ];
          };
        };
      };
    };
  };
}
