{{channels, ...}: final: prev: {
  inherit (channels.unstable) yabai;
  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;

    config = {
          window_border = "on";
          window_border_width = 2;
          active_window_border_color = "0xff81a1c1";
          normal_window_border_color = "0xff3b4252";
          focus_follows_mouse = "off";
          mouse_follows_focus = "off";
          mouse_drop_action = "stack";
          window_placement = "second_child";
          window_opacity = "on";
          window_topmost = "on";
          split_ratio = "0.50";
          auto_balance = "on";
          mouse_modifier = "fn";
          mouse_action1 = "move";
          mouse_action2 = "resize";
          layout = "bsp";
          top_padding = 25;
          # bottom_padding = 5;
          # left_padding = 10;
          # right_padding = 10;
          window_gap = 5;
    };
        # https://github.com/koekeishiya/yabai/blob/master/doc/yabai.asciidoc#signal
        # https://felixkratz.github.io/SketchyBar/config/events#triggering-custom-events
        # https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(from-HEAD)
        extraConfig = ''
          yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
          sudo yabai --load-sa
          # rules
          yabai -m rule --add label="About This Mac" app="System Information" title="About This Mac" manage=off
          yabai -m rule --add app='System Preferences' manage=off
          yabai -m rule --add app='mono-stretchly' manage=off
          # Any other arbitrary config here
          yabai -m signal --add event=window_focused action="sketchybar --trigger window_focus"
          yabai -m signal --add event=window_resized action="sketchybar --trigger window_focus"
          yabai -m signal --add event=window_moved action="sketchybar --trigger window_on_spaces"
          yabai -m signal --add event=window_created action="sketchybar --trigger windows_on_spaces"
          yabai -m signal --add event=window_destroyed action="sketchybar --trigger windows_on_spaces"
        '';
  };
}
