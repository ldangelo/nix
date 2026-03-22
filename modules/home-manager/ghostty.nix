{ ... }:

{
  # Ghostty terminal configuration
  # Config lives at ~/Library/Application Support/com.mitchellh.ghostty/config
  home.file."Library/Application Support/com.mitchellh.ghostty/config".text = ''
    # tmux session shortcuts — send ESC+key so tmux M- bindings fire
    # CMD+t = new tmux session; CMD+1..9 = switch to Nth tmux session
    # Must use digit_N (physical key) to override Ghostty's goto_tab defaults
    keybind = super+t=text:\x1bt
    keybind = super+digit_1=unbind
    keybind = super+digit_1=text:\x1b1
    keybind = super+digit_2=unbind
    keybind = super+digit_2=text:\x1b2
    keybind = super+digit_3=unbind
    keybind = super+digit_3=text:\x1b3
    keybind = super+digit_4=unbind
    keybind = super+digit_4=text:\x1b4
    keybind = super+digit_5=unbind
    keybind = super+digit_5=text:\x1b5
    keybind = super+digit_6=unbind
    keybind = super+digit_6=text:\x1b6
    keybind = super+digit_7=unbind
    keybind = super+digit_7=text:\x1b7
    keybind = super+digit_8=unbind
    keybind = super+digit_8=text:\x1b8
    keybind = super+digit_9=unbind
    keybind = super+digit_9=text:\x1b9
  '';
}
