{
  lib,
  pkgs,
  config,
  namespace,
  ...
}: let
  cfg = config.${namespace}.desktop.yabai;

  inherit (lib) types mkEnableOption mkIf;
  inherit (lib.${namespace}) mkOpt enabled;
in {
  options.${namespace}.desktop.yabai = {
    enable = mkEnableOption "Yabai";
    enable-scripting-addition = mkOpt types.bool true "Whether to enable the scripting addition for Yabai. (Requires SIP to be disabled)";
  };

  config = mkIf cfg.enable {
    oftheangels.desktop.addons = {
      skhd = enabled;
      spacebar = enabled;
    };

    services.yabai = {
      enable = true;
      enableScriptingAddition = cfg.enable-scripting-addition;

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
          debug_output = "on";


          window_gap = 6;
          window_shadow = "float";

          external_bar = "all:${builtins.toString config.services.spacebar.config.height}:0";
      };

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
         yabai -m rule --add label="Finder" app="^Finder$" title="(Co(py|nnect)|Move|Info|Pref)" manage=off
        yabai -m rule --add label="Safari" app="^Safari$" title="^(General|(Tab|Password|Website|Extension)s|AutoFill|Se(arch|curity)|Privacy|Advance)$" manage=off
        yabai -m rule --add label="System Preferences" app="^System Preferences$" title=".*" manage=off
        yabai -m rule --add label="App Store" app="^App Store$" manage=off
        yabai -m rule --add label="Activity Monitor" app="^Activity Monitor$" manage=off
        yabai -m rule --add label="Calculator" app="^Calculator$" manage=off
        yabai -m rule --add label="Dictionary" app="^Dictionary$" manage=off
        yabai -m rule --add label="mpv" app="^mpv$" manage=off
        yabai -m rule --add label="Software Update" title="Software Update" manage=off
        yabai -m rule --add label="About This Mac" app="System Information" title="About This Mac" manage=off
      '';
    };
  };
}
