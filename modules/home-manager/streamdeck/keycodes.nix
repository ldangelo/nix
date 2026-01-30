# macOS Key Codes for Stream Deck
# NativeCode = macOS CGKeyCode
# VKeyCode = Windows Virtual Key Code (used for compatibility)
# QTKeyCode = Qt key code

{
  # Modifier key flags for KeyModifiers field
  # These are bitmasks that get OR'd together
  # Based on actual Stream Deck profile analysis:
  # cmd+shift gives KeyModifiers=9 (1+8), not 3
  modifiers = {
    cmd = 1;      # bit 0
    ctrl = 4;     # bit 2
    option = 2;   # bit 1 (alt)
    shift = 8;    # bit 3
  };

  # Calculate KeyModifiers from boolean flags
  # Based on observed values: cmd=1, option=2, ctrl=4, shift=8
  calcModifiers = { cmd ? false, shift ? false, ctrl ? false, option ? false }:
    (if cmd then 1 else 0) +
    (if option then 2 else 0) +
    (if ctrl then 4 else 0) +
    (if shift then 8 else 0);

  # Key code mappings: key name -> { native, vkey, qt }
  # NativeCode = macOS CGKeyCode
  # VKeyCode = Windows Virtual-Key code  
  # QTKeyCode = Qt::Key value
  keys = {
    # Letters (macOS CGKeyCodes)
    a = { native = 0; vkey = 0; qt = 65; };
    b = { native = 11; vkey = 11; qt = 66; };
    c = { native = 8; vkey = 8; qt = 67; };
    d = { native = 2; vkey = 2; qt = 68; };
    e = { native = 14; vkey = 14; qt = 69; };
    f = { native = 3; vkey = 3; qt = 70; };
    g = { native = 5; vkey = 5; qt = 71; };
    h = { native = 4; vkey = 4; qt = 72; };
    i = { native = 34; vkey = 34; qt = 73; };
    j = { native = 38; vkey = 38; qt = 74; };
    k = { native = 40; vkey = 40; qt = 75; };
    l = { native = 37; vkey = 37; qt = 76; };
    m = { native = 46; vkey = 46; qt = 77; };
    n = { native = 45; vkey = 45; qt = 78; };
    o = { native = 31; vkey = 31; qt = 79; };
    p = { native = 35; vkey = 35; qt = 80; };
    q = { native = 12; vkey = 12; qt = 81; };
    r = { native = 15; vkey = 15; qt = 82; };
    s = { native = 1; vkey = 1; qt = 83; };
    t = { native = 17; vkey = 17; qt = 84; };
    u = { native = 32; vkey = 32; qt = 85; };
    v = { native = 9; vkey = 9; qt = 86; };
    w = { native = 13; vkey = 13; qt = 87; };
    x = { native = 7; vkey = 7; qt = 88; };
    y = { native = 16; vkey = 16; qt = 89; };
    z = { native = 6; vkey = 6; qt = 90; };

    # Numbers (top row)
    "0" = { native = 29; vkey = 29; qt = 48; };
    "1" = { native = 18; vkey = 18; qt = 49; };
    "2" = { native = 19; vkey = 19; qt = 50; };
    "3" = { native = 20; vkey = 20; qt = 51; };
    "4" = { native = 21; vkey = 21; qt = 52; };
    "5" = { native = 23; vkey = 23; qt = 53; };
    "6" = { native = 22; vkey = 22; qt = 54; };
    "7" = { native = 26; vkey = 26; qt = 55; };
    "8" = { native = 28; vkey = 28; qt = 56; };
    "9" = { native = 25; vkey = 25; qt = 57; };

    # Function keys
    f1 = { native = 122; vkey = 122; qt = 16777264; };
    f2 = { native = 120; vkey = 120; qt = 16777265; };
    f3 = { native = 99; vkey = 99; qt = 16777266; };
    f4 = { native = 118; vkey = 118; qt = 16777267; };
    f5 = { native = 96; vkey = 96; qt = 16777268; };
    f6 = { native = 97; vkey = 97; qt = 16777269; };
    f7 = { native = 98; vkey = 98; qt = 16777270; };   # Media previous
    f8 = { native = 100; vkey = 100; qt = 16777271; }; # Media play/pause
    f9 = { native = 101; vkey = 101; qt = 16777272; }; # Media next
    f10 = { native = 109; vkey = 109; qt = 16777273; };
    f11 = { native = 103; vkey = 103; qt = 16777274; };
    f12 = { native = 111; vkey = 111; qt = 16777275; };

    # Special keys
    space = { native = 49; vkey = 49; qt = 32; };
    tab = { native = 48; vkey = 48; qt = 16777217; };
    enter = { native = 36; vkey = 36; qt = 16777220; };
    return = { native = 36; vkey = 36; qt = 16777220; };
    escape = { native = 53; vkey = 53; qt = 16777216; };
    esc = { native = 53; vkey = 53; qt = 16777216; };
    backspace = { native = 51; vkey = 51; qt = 16777219; };
    delete = { native = 117; vkey = 117; qt = 16777223; };

    # Arrow keys
    left = { native = 123; vkey = 123; qt = 16777234; };
    right = { native = 124; vkey = 124; qt = 16777236; };
    up = { native = 126; vkey = 126; qt = 16777235; };
    down = { native = 125; vkey = 125; qt = 16777237; };

    # Punctuation/symbols
    minus = { native = 27; vkey = 27; qt = 45; };
    "-" = { native = 27; vkey = 27; qt = 45; };
    equal = { native = 24; vkey = 24; qt = 61; };
    "=" = { native = 24; vkey = 24; qt = 61; };
    bracketleft = { native = 33; vkey = 33; qt = 91; };
    "[" = { native = 33; vkey = 33; qt = 91; };
    bracketright = { native = 30; vkey = 30; qt = 93; };
    "]" = { native = 30; vkey = 30; qt = 93; };
    backslash = { native = 42; vkey = 42; qt = 92; };
    semicolon = { native = 41; vkey = 41; qt = 59; };
    ";" = { native = 41; vkey = 41; qt = 59; };
    quote = { native = 39; vkey = 39; qt = 39; };
    "'" = { native = 39; vkey = 39; qt = 39; };
    comma = { native = 43; vkey = 43; qt = 44; };
    "," = { native = 43; vkey = 43; qt = 44; };
    period = { native = 47; vkey = 47; qt = 46; };
    "." = { native = 47; vkey = 47; qt = 46; };
    slash = { native = 44; vkey = 44; qt = 47; };
    "/" = { native = 44; vkey = 44; qt = 47; };
    grave = { native = 50; vkey = 50; qt = 96; };
    "`" = { native = 50; vkey = 50; qt = 96; };

    # Page navigation
    pageup = { native = 116; vkey = 116; qt = 16777238; };
    pagedown = { native = 121; vkey = 121; qt = 16777239; };
    home = { native = 115; vkey = 115; qt = 16777232; };
    end = { native = 119; vkey = 119; qt = 16777233; };
  };

  # Modifier name aliases
  modifierAliases = {
    cmd = "cmd";
    command = "cmd";
    meta = "cmd";
    super = "cmd";
    ctrl = "ctrl";
    control = "ctrl";
    option = "option";
    alt = "option";
    opt = "option";
    shift = "shift";
  };

  # Parse a key name (case insensitive, handles aliases)
  normalizeKey = key:
    let lowerKey = builtins.replaceStrings 
          ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]
          ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]
          key;
    in lowerKey;

  # Check if a key is a modifier
  isModifier = key:
    let normalized = builtins.replaceStrings 
          ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]
          ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]
          key;
    in builtins.elem normalized ["cmd" "command" "meta" "super" "ctrl" "control" "option" "alt" "opt" "shift"];
}
