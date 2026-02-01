# Leo's Stream Deck Configuration
# Import this in your home-manager config: imports = [ ./streamdeck-config.nix ];

{ config, lib, pkgs, ... }:

{
  programs.streamdeck = {
    enable = true;
    autoRestart = true;  # Restart Stream Deck after deployment

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
              targetPage = "apps";
              # icon = ./icons/folder.png;
            };

            "1,0" = {
              name = "Links";
              type = "folder";
              title = "Links";
              targetPage = "links";
            };

            "2,0" = {
              name = "Screenshot";
              type = "hotkey";
              title = "Screen\n\n\n\nShot";
              keys = [ "cmd" "shift" "4" ];
            };

            # Row 1: Meeting Controls (placeholder folders - pages not defined yet)
            "0,1" = {
              name = "Tutorials";
              type = "folder";
              title = "Tutorials";
              # targetPage = "tutorials";  # uncomment when tutorials page exists
            };

            "1,1" = {
              name = "Emojis";
              type = "folder";
              title = "Emojis";
              # targetPage = "emojis";  # uncomment when emojis page exists
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

      pages = {
        default = {
          buttons = {
          # Row 0: Communication
          "0,0" = {
            name = "Slack";
            type = "open";
            path = "/Applications/Slack.app";
            icon = ./streamdeck/icons/slack.png;
          };

          "1,0" = {
            name = "Zoom";
            type = "open";
            path = "/Applications/zoom.us.app";
            icon = ./streamdeck/icons/zoom.png;
          };

          "2,0" = {
            name = "Teams";
            type = "open";
            path = "/Applications/Microsoft Teams.app";
            icon = ./streamdeck/icons/teams.png;
          };

          "3,0" = {
            name = "Mail";
            type = "open";
            path = "/System/Applications/Mail.app";
            icon = ./streamdeck/icons/mail.png;
          };

          "4,0" = {
            name = "Calendar";
            type = "open";
            path = "/Applications/Fantastical.app";
            icon = ./streamdeck/icons/fantastical.png;
          };

          # Row 1: Development
          "0,1" = {
            name = "iTerm";
            type = "open";
            path = "/Applications/iTerm.app";
            icon = ./streamdeck/icons/iterm.png;
          };

          "1,1" = {
            name = "Cursor";
            type = "open";
            path = "/Applications/Cursor.app";
            icon = ./streamdeck/icons/cursor.png;
          };

          "2,1" = {
            name = "GitHub";
            type = "website";
            url = "https://github.com";
            icon = ./streamdeck/icons/github.png;
          };

          "3,1" = {
            name = "Obsidian";
            type = "website";
            url = "obsidian://open?vault=Curantis";
            openInBrowser = false;
            icon = ./streamdeck/icons/obsidian.png;
          };

          "4,1" = {
            name = "Meetings";
            type = "folder";
            title = "Meetings";
            targetPage = "meetings";
            icon = ./streamdeck/icons/meetings.png;
          };

          # Row 2: Media & Navigation
          "0,2" = {
            name = "Prev";
            type = "hotkey";
            keys = [ "f7" ];
            icon = ./streamdeck/icons/prev.png;
          };

          "1,2" = {
            name = "Play";
            type = "hotkey";
            keys = [ "f8" ];
            icon = ./streamdeck/icons/play.png;
          };

          "2,2" = {
            name = "Next";
            type = "hotkey";
            keys = [ "f9" ];
            icon = ./streamdeck/icons/next.png;
          };

          "3,2" = {
            name = "Mute";
            type = "multimedia";
            actionIdx = 4;
            icon = ./streamdeck/icons/mute.png;
          };

          "4,2" = {
            name = "Lock";
            type = "hotkey";
            keys = [ "ctrl" "cmd" "q" ];
            icon = ./streamdeck/icons/lock.png;
          };
        };
      };  # closes default page

        # Meetings page - Zoom & Teams controls
        meetings = {
          name = "Meeting Controls";
          buttons = {
            # Back button
            "0,0" = {
              name = "Back";
              type = "backToParent";
            };

            # === ZOOM CONTROLS (Row 0) ===
            "1,0" = {
              name = "ZoomMute";
              type = "hotkey";
              title = "Mute\n(Zoom)";
              keys = [ "cmd" "shift" "a" ];
              icon = ./streamdeck/icons/mic-off.png;
            };

            "2,0" = {
              name = "ZoomVideo";
              type = "hotkey";
              title = "Video\n(Zoom)";
              keys = [ "cmd" "shift" "v" ];
              icon = ./streamdeck/icons/video-off.png;
            };

            "3,0" = {
              name = "ZoomShare";
              type = "hotkey";
              title = "Share\n(Zoom)";
              keys = [ "cmd" "shift" "s" ];
              icon = ./streamdeck/icons/screen-share.png;
            };

            "4,0" = {
              name = "ZoomLeave";
              type = "hotkey";
              title = "Leave\n(Zoom)";
              keys = [ "cmd" "w" ];
              icon = ./streamdeck/icons/leave.png;
            };

            # === TEAMS CONTROLS (Row 1) ===
            "0,1" = {
              name = "Zoom";
              type = "open";
              path = "/Applications/zoom.us.app";
              icon = ./streamdeck/icons/zoom.png;
            };

            "1,1" = {
              name = "TeamsMute";
              type = "hotkey";
              title = "Mute\n(Teams)";
              keys = [ "cmd" "shift" "m" ];
              icon = ./streamdeck/icons/mic-off.png;
            };

            "2,1" = {
              name = "TeamsVideo";
              type = "hotkey";
              title = "Video\n(Teams)";
              keys = [ "cmd" "shift" "o" ];
              icon = ./streamdeck/icons/video-off.png;
            };

            "3,1" = {
              name = "TeamsShare";
              type = "hotkey";
              title = "Share\n(Teams)";
              keys = [ "cmd" "shift" "e" ];
              icon = ./streamdeck/icons/screen-share.png;
            };

            "4,1" = {
              name = "TeamsLeave";
              type = "hotkey";
              title = "Leave\n(Teams)";
              keys = [ "cmd" "shift" "b" ];
              icon = ./streamdeck/icons/leave.png;
            };

            # === EXTRAS (Row 2) ===
            "0,2" = {
              name = "Teams";
              type = "open";
              path = "/Applications/Microsoft Teams.app";
              icon = ./streamdeck/icons/teams.png;
            };

            "1,2" = {
              name = "ZoomHand";
              type = "hotkey";
              title = "Hand\n(Zoom)";
              keys = [ "option" "y" ];
              icon = ./streamdeck/icons/hand-raise.png;
            };

            "2,2" = {
              name = "TeamsHand";
              type = "hotkey";
              title = "Hand\n(Teams)";
              keys = [ "cmd" "shift" "k" ];
              icon = ./streamdeck/icons/hand-raise.png;
            };
          };
        };
      };  # closes pages
    };  # closes profiles.work
  };  # closes programs.streamdeck
}
