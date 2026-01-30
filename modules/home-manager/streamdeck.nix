{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.streamdeck;
  keycodes = import ./streamdeck/keycodes.nix;

  # Stream Deck profile directory
  profilesDir = "Library/Application Support/com.elgato.StreamDeck/ProfilesV3";

  # Generate a deterministic UUID from a string (formatted properly)
  makeUUID = seed:
    let
      hash = builtins.hashString "sha256" seed;
      # Format: 8-4-4-4-12
      p1 = builtins.substring 0 8 hash;
      p2 = builtins.substring 8 4 hash;
      p3 = builtins.substring 12 4 hash;
      p4 = builtins.substring 16 4 hash;
      p5 = builtins.substring 20 12 hash;
    in
    "${p1}-${p2}-${p3}-${p4}-${p5}";

  # Action UUID mappings
  actionUUIDs = {
    hotkey = "com.elgato.streamdeck.system.hotkey";
    open = "com.elgato.streamdeck.system.open";
    website = "com.elgato.streamdeck.system.website";
    multiAction = "com.elgato.streamdeck.multiactions.multiaction";
    folder = "com.elgato.streamdeck.profile.openchild";
    backToParent = "com.elgato.streamdeck.profile.backtoparent";
    multimedia = "com.elgato.streamdeck.system.multimedia";
    nextPage = "com.elgato.streamdeck.page.next";
    prevPage = "com.elgato.streamdeck.page.previous";
  };

  # Plugin names for action types
  pluginNames = {
    hotkey = "Activate a Key Command";
    open = "Open";
    website = "Website";
    multiAction = "Multi Action";
    folder = "Create Folder";
    backToParent = "Parent Folder";
    multimedia = "Multimedia";
    nextPage = "Pages";
    prevPage = "Pages";
  };

  # Normalize a key name to lowercase
  normalizeKey = key:
    builtins.replaceStrings
      [ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" ]
      [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" ]
      key;

  # Check if key is a modifier
  isModifier = key:
    builtins.elem (normalizeKey key) [ "cmd" "command" "meta" "super" "ctrl" "control" "option" "alt" "opt" "shift" ];

  # Get canonical modifier name
  getModifier = key:
    let
      normalized = normalizeKey key;
      aliases = {
        cmd = "cmd"; command = "cmd"; meta = "cmd"; super = "cmd";
        ctrl = "ctrl"; control = "ctrl";
        option = "option"; alt = "option"; opt = "option";
        shift = "shift";
      };
    in
    aliases.${normalized} or null;

  # Parse a list of keys into modifiers and main key
  parseKeys = keys:
    let
      modifiers = builtins.filter isModifier keys;
      mainKeys = builtins.filter (k: !isModifier k) keys;
      mainKey = if mainKeys == [ ] then null else builtins.head mainKeys;
      modSet = builtins.listToAttrs (map (m: { name = getModifier m; value = true; }) modifiers);
    in
    {
      hasCmd = modSet.cmd or false;
      hasCtrl = modSet.ctrl or false;
      hasOption = modSet.option or false;
      hasShift = modSet.shift or false;
      key = mainKey;
    };

  # Build a hotkey object for Stream Deck
  buildHotkey = keys:
    let
      parsed = parseKeys keys;
      keyInfo =
        if parsed.key == null then
          { native = -1; vkey = -1; qt = 33554431; }
        else
          keycodes.keys.${normalizeKey parsed.key} or { native = -1; vkey = -1; qt = 33554431; };
      modValue = keycodes.calcModifiers {
        cmd = parsed.hasCmd;
        shift = parsed.hasShift;
        ctrl = parsed.hasCtrl;
        option = parsed.hasOption;
      };
    in
    {
      KeyCmd = parsed.hasCmd;
      KeyCtrl = parsed.hasCtrl;
      KeyOption = parsed.hasOption;
      KeyShift = parsed.hasShift;
      KeyModifiers = modValue;
      NativeCode = keyInfo.native;
      VKeyCode = keyInfo.vkey;
      QTKeyCode = keyInfo.qt;
    };

  # Empty hotkey slot
  emptyHotkey = {
    KeyCmd = false;
    KeyCtrl = false;
    KeyOption = false;
    KeyShift = false;
    KeyModifiers = 0;
    NativeCode = -1;
    VKeyCode = -1;
    QTKeyCode = 33554431;
  };

  # Build settings for different action types
  buildSettings = button:
    if button.type == "hotkey" then
      {
        Coalesce = true;
        Hotkeys = [
          (buildHotkey button.keys)
          emptyHotkey
          emptyHotkey
          emptyHotkey
        ];
      }
    else if button.type == "open" then
      { path = "\"${button.path}\""; }
    else if button.type == "website" then
      {
        openInBrowser = button.openInBrowser or true;
        path = button.url;
      }
    else if button.type == "folder" then
      { ProfileUUID = button.targetProfile or ""; }
    else if button.type == "multimedia" then
      { actionIdx = button.actionIdx or 0; }
    else
      button.settings or { };

  # Build a button/action object
  buildAction = profileName: pos: button:
    let
      actionId = makeUUID "${profileName}-${pos}-${button.name}";
      uuid = actionUUIDs.${button.type} or button.type;
      pluginName = pluginNames.${button.type} or button.name;
    in
    {
      ActionID = actionId;
      LinkedTitle = true;
      Name = pluginName;
      Plugin = {
        Name = pluginName;
        UUID = uuid;
        Version = "1.0";
      };
      Resources = null;
      Settings = buildSettings button;
      State = 0;
      States = [
        ({
          FontFamily = "";
          FontSize = button.fontSize or 12;
          FontStyle = "";
          FontUnderline = false;
          OutlineThickness = 2;
          ShowTitle = button.showTitle or true;
          Title = button.title or button.name;
          TitleAlignment = button.titleAlignment or "bottom";
          TitleColor = button.titleColor or "#ffffff";
        } // (optionalAttrs (button.icon != null) {
          Image = "Images/${button.name}.png";
        }))
      ];
      UUID = uuid;
    };

  # Build a page manifest
  buildPageManifest = profileName: pageName: page:
    let
      actions = mapAttrs (pos: button: buildAction profileName pos button) page.buttons;
    in
    {
      Controllers = [{
        Actions = if actions == { } then null else actions;
        Type = "Keypad";
      }];
      Icon = "";
      Name = pageName;
    };

  # Build the root profile manifest
  buildRootManifest = profile:
    let
      pageIds = attrNames profile.pages;
      defaultPage = if pageIds == [ ] then "" else builtins.head pageIds;
    in
    {
      Device = {
        Model = cfg.deviceModel;
        UUID = cfg.deviceId;
      };
      Name = profile.name;
      Pages = {
        Current = makeUUID "${profile.name}-${defaultPage}";
        Default = makeUUID "${profile.name}-${defaultPage}";
        Pages = map (p: makeUUID "${profile.name}-${p}") pageIds;
      };
      Version = "3.0";
    };

  # Collect all icons from a profile
  collectIcons = profile:
    let
      pagesIcons = lib.flatten (lib.mapAttrsToList
        (pageName: page:
          lib.mapAttrsToList
            (pos: button:
              if button.icon != null then
                { name = "${button.name}.png"; source = button.icon; }
              else
                null)
            page.buttons)
        profile.pages);
    in
    builtins.filter (x: x != null) pagesIcons;

  # Generate all profile files for home.file
  generateProfileFiles = profileName: profile:
    let
      profileId = makeUUID profileName;
      profileDir = "${profilesDir}/${profileId}.sdProfile";

      # Root manifest
      rootManifest = {
        "${profileDir}/manifest.json".text = builtins.toJSON (buildRootManifest profile);
      };

      # Page manifests
      pageManifests = lib.mapAttrs'
        (pageName: page:
          let pageId = makeUUID "${profile.name}-${pageName}";
          in
          nameValuePair
            "${profileDir}/Profiles/${pageId}/manifest.json"
            { text = builtins.toJSON (buildPageManifest profile.name pageName page); })
        profile.pages;

      # Icons for each page
      pageIcons = lib.mapAttrs'
        (pageName: page:
          let
            pageId = makeUUID "${profile.name}-${pageName}";
            icons = lib.mapAttrs'
              (pos: button:
                nameValuePair
                  "${profileDir}/Profiles/${pageId}/Images/${button.name}.png"
                  { source = button.icon; })
              (lib.filterAttrs (pos: button: button.icon != null) page.buttons);
          in
          nameValuePair pageName icons)
        profile.pages;

      flatIcons = lib.foldl (acc: icons: acc // icons) { } (attrValues pageIcons);
    in
    rootManifest // pageManifests // flatIcons;

  # Button submodule type
  buttonType = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        description = "Button display name";
      };

      type = mkOption {
        type = types.enum [ "hotkey" "open" "website" "multiAction" "folder" "backToParent" "multimedia" "nextPage" "prevPage" ];
        description = "Action type";
      };

      # For hotkey type
      keys = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = [ "cmd" "shift" "a" ];
        description = "Key combination (modifiers + key)";
      };

      # For open type
      path = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "/Applications/Slack.app";
        description = "Path to application/file to open";
      };

      # For website type
      url = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "https://github.com";
        description = "URL to open";
      };

      openInBrowser = mkOption {
        type = types.bool;
        default = true;
        description = "Open in default browser (for website type)";
      };

      # For folder type
      targetProfile = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "UUID of target profile for folder action";
      };

      # For multimedia type
      actionIdx = mkOption {
        type = types.int;
        default = 0;
        description = "Multimedia action index (4=mute, 5=vol down, 6=vol up)";
      };

      # Visual options
      icon = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "Path to icon image file";
      };

      title = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Button title (defaults to name)";
      };

      showTitle = mkOption {
        type = types.bool;
        default = true;
        description = "Show title on button";
      };

      fontSize = mkOption {
        type = types.int;
        default = 12;
        description = "Font size for title";
      };

      titleColor = mkOption {
        type = types.str;
        default = "#ffffff";
        description = "Title color in hex";
      };

      titleAlignment = mkOption {
        type = types.enum [ "top" "middle" "bottom" ];
        default = "bottom";
        description = "Title alignment";
      };

      # Raw settings override
      settings = mkOption {
        type = types.attrs;
        default = { };
        description = "Raw settings (advanced, overrides type-specific settings)";
      };
    };
  };

  # Page submodule type
  pageType = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        default = "";
        description = "Page name";
      };

      buttons = mkOption {
        type = types.attrsOf buttonType;
        default = { };
        example = {
          "0,0" = { name = "Mute"; type = "hotkey"; keys = [ "cmd" "shift" "m" ]; };
        };
        description = "Button configurations keyed by position (e.g., '0,0' = column 0, row 0)";
      };
    };
  };

  # Profile submodule type
  profileType = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        description = "Profile display name";
      };

      pages = mkOption {
        type = types.attrsOf pageType;
        default = { };
        description = "Pages in this profile";
      };

      # Convenience: single-page profile with buttons directly
      buttons = mkOption {
        type = types.attrsOf buttonType;
        default = { };
        description = "Buttons for single-page profile (shorthand for pages.default.buttons)";
      };
    };
  };

in
{
  options.programs.streamdeck = {
    enable = mkEnableOption "Stream Deck declarative configuration";

    deviceId = mkOption {
      type = types.str;
      default = "@(1)[4057/128/DL51K1A60852]";
      description = "Stream Deck device UUID (from System Information or existing profile)";
    };

    deviceModel = mkOption {
      type = types.str;
      default = "20GBA9901";
      description = "Stream Deck device model ID";
    };

    profiles = mkOption {
      type = types.attrsOf profileType;
      default = { };
      description = "Stream Deck profiles";
    };

    activationScript = mkOption {
      type = types.bool;
      default = true;
      description = "Generate activation script to restart Stream Deck app";
    };
  };

  config = mkIf cfg.enable {
    # Generate all profile files plus debug output
    home.file = lib.foldl
      (acc: profileName:
        let
          profile = cfg.profiles.${profileName};
          # Convert single-page buttons shorthand to pages
          effectiveProfile =
            if profile.pages != { } then profile
            else profile // { pages = { default = { buttons = profile.buttons; name = ""; }; }; };
        in
        acc // (generateProfileFiles profileName effectiveProfile))
      {
        # Debug output - write parsed configuration
        ".config/streamdeck/debug.json".text = builtins.toJSON {
          deviceId = cfg.deviceId;
          deviceModel = cfg.deviceModel;
          profileNames = attrNames cfg.profiles;
        };
      }
      (attrNames cfg.profiles);

    # Activation script to restart Stream Deck
    home.activation = mkIf cfg.activationScript {
      streamdeckRestart = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        # Restart Stream Deck app to pick up new profiles
        if pgrep -x "Stream Deck" > /dev/null 2>&1; then
          $DRY_RUN_CMD osascript -e 'quit app "Stream Deck"' || true
          $DRY_RUN_CMD sleep 2
          $DRY_RUN_CMD open -a "Elgato Stream Deck" || true
        fi
      '';
    };
  };
}
