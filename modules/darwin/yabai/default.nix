{ pkgs, lib, ... }:
{
  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;

    config = {
      layout                       = "bsp";
      window_placement             = "second_child";
      split_ratio                  = "0.50";
      auto_balance                 = "off";   # manual balance via keybind

      focus_follows_mouse          = "off";
      mouse_follows_focus          = "off";
      mouse_modifier               = "fn";
      mouse_action1                = "move";
      mouse_action2                = "resize";
      mouse_drop_action            = "stack";

      window_opacity               = "on";
      window_opacity_duration      = "0.0";
      active_window_opacity        = "1.0";
      normal_window_opacity        = "0.95";

      top_padding                  = 10;
      bottom_padding               = 10;
      left_padding                 = 10;
      right_padding                = 10;
      window_gap                   = 10;
    };

    extraConfig = ''
      # Reload scripting addition after Dock restart
      yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

      # Label spaces to match AeroSpace workspace names
      # Note: space 1 cannot be labeled (yabai rejects numeric-only labels) — reference by index
      yabai -m space 2 --label "D"
      yabai -m space 3 --label "C"
      yabai -m space 4 --label "E"
      yabai -m space 5 --label "O"
      yabai -m space 6 --label "T"

      # Space O: float layout so o-layout.sh can use --grid for precise placement
      yabai -m config --space O layout float

      # ── Float rules ──────────────────────────────────────────────────────────
      yabai -m rule --add app="^System (Preferences|Settings)$" manage=off
      yabai -m rule --add app="^System Information$" title="About This Mac" manage=off
      yabai -m rule --add app="^Finder$" title="(Co(py|nnect)|Move|Info|Pref)" manage=off
      yabai -m rule --add app="^(Calculator|Dictionary|Activity Monitor|App Store)$" manage=off
      yabai -m rule --add app="^(Raycast|1Password|Alfred)$" manage=off
      yabai -m rule --add app="^zoom\.us$" manage=off

      # ── App → space assignment ────────────────────────────────────────────────
      yabai -m rule --add app="^iTerm2$"               space=1
      yabai -m rule --add app="^WezTerm$"              space=1

      yabai -m rule --add app="^IntelliJ IDEA$"        space=D

      yabai -m rule --add app="^Slack$"                space=C
      yabai -m rule --add app="^zoom\.us$"             space=C
      yabai -m rule --add app="^Messages$"             space=C
      yabai -m rule --add app="^Discord$"              space=C
      yabai -m rule --add app="^Telegram$"             space=C
      yabai -m rule --add app="^Spark$"                space=C

      yabai -m rule --add app="^Music$"                space=E

      yabai -m rule --add app="^Fantastical$"          space=O
      yabai -m rule --add app="^Calendar$"             space=O
      yabai -m rule --add app="^Obsidian$"             space=O
      yabai -m rule --add app="^Timing$"               space=O
      yabai -m rule --add app="^Rize$"                 space=O

      yabai -m rule --add app="^TradingView$"          space=T

      # ── Signals ──────────────────────────────────────────────────────────────
      yabai -m signal --add event=window_focused  action="sketchybar --trigger window_focus"
      yabai -m signal --add event=window_resized  action="sketchybar --trigger window_focus"
      yabai -m signal --add event=window_moved    action="sketchybar --trigger window_on_spaces"
      yabai -m signal --add event=window_created  action="sketchybar --trigger windows_on_spaces"
      yabai -m signal --add event=window_destroyed action="sketchybar --trigger windows_on_spaces"

      # Trigger O-workspace layout when any managed window appears on space O
      yabai -m signal --add event=window_created \
        action="if yabai -m query --windows --window \$YABAI_WINDOW_ID | grep -q '\"space\":5'; then ~/.config/yabai/o-layout.sh; fi"
    '';
  };
}
