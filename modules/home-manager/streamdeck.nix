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
  buildSettings = profileName: button:
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
      { 
        # If targetPage is set, auto-generate UUID (lowercase to match Stream Deck format)
        ProfileUUID = 
          if button.targetPage != null then 
            makeUUID "${profileName}-${button.targetPage}"
          else if button.targetProfile != null then button.targetProfile
          else ""; 
      }
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
      # Use custom title if set, otherwise use button name
      displayName = if button.title != null then button.title else button.name;
      hasCustomTitle = button.title != null;
      # Build complete state with all required styling attributes
      stateAttrs = {
        FontFamily = "";
        FontSize = button.fontSize;
        FontStyle = "";
        FontUnderline = false;
        OutlineThickness = 2;
        ShowTitle = button.showTitle;
        TitleAlignment = button.titleAlignment;
        TitleColor = button.titleColor;
      }
        // (optionalAttrs (button.icon != null) { Image = "Images/${button.name}.png"; })
        // (optionalAttrs hasCustomTitle { Title = displayName; });
    in
    {
      ActionID = actionId;
      LinkedTitle = !hasCustomTitle;  # false if we have custom title
      Name = displayName;
      # Plugin block is required for Stream Deck to recognize the action
      Plugin = {
        Name = pluginName;
        UUID = uuid;
        Version = "1.0";
      };
      Resources = null;
      Settings = buildSettings profileName button;
      State = 0;
      States = [ stateAttrs ];
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
      # Prefer "default" page, otherwise fall back to first alphabetically
      defaultPage = 
        if pageIds == [ ] then "" 
        else if builtins.elem "default" pageIds then "default"
        else builtins.head pageIds;
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
        description = "UUID of target profile for folder action (use targetPage for same-profile navigation)";
      };

      targetPage = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "apps";
        description = "Target page name within the same profile (auto-generates UUID)";
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
      description = "Generate activation script to write Stream Deck profiles";
    };

    autoRestart = mkOption {
      type = types.bool;
      default = false;
      description = "Automatically restart Stream Deck app after writing profiles";
    };
  };

  config = mkIf cfg.enable (
    let
      # Generate JSON content for all profiles (for use in activation script)
      allProfilesJson = lib.mapAttrs (profileName: profile:
        let
          effectiveProfile =
            if profile.pages != { } then profile
            else profile // { pages = { default = { buttons = profile.buttons; name = ""; }; }; };
          profileId = makeUUID profileName;
        in {
          id = profileId;
          manifest = buildRootManifest effectiveProfile;
          pages = lib.mapAttrs (pageName: page: {
            id = makeUUID "${effectiveProfile.name}-${pageName}";
            manifest = buildPageManifest effectiveProfile.name pageName page;
          }) effectiveProfile.pages;
        }
      ) cfg.profiles;

      # Collect all icons from all profiles for staging
      allIcons = lib.flatten (lib.mapAttrsToList (profileName: profile:
        let
          effectiveProfile =
            if profile.pages != { } then profile
            else profile // { pages = { default = { buttons = profile.buttons; name = ""; }; }; };
        in
        lib.flatten (lib.mapAttrsToList (pageName: page:
          lib.mapAttrsToList (pos: button:
            if button.icon != null then {
              name = button.name;
              source = button.icon;
              profile = profileName;
              page = pageName;
            } else null
          ) page.buttons
        ) effectiveProfile.pages)
      ) cfg.profiles);

      iconsList = builtins.filter (x: x != null) allIcons;

      # Generate icon staging files
      iconFiles = lib.listToAttrs (map (icon: {
        name = ".config/streamdeck/icons/${icon.profile}/${icon.page}/${icon.name}.png";
        value = { source = icon.source; };
      }) iconsList);

    in {
    # Write debug config, profiles JSON, and stage icons
    home.file = {
      ".config/streamdeck/debug.json".text = builtins.toJSON {
        deviceId = cfg.deviceId;
        deviceModel = cfg.deviceModel;
        profileNames = attrNames cfg.profiles;
      };
      # Write profile JSON to staging area for activation script to use
      ".config/streamdeck/profiles.json".text = builtins.toJSON allProfilesJson;
    } // iconFiles;

    # Activation script to create and import Stream Deck profiles
    home.activation = mkIf cfg.activationScript {
      streamdeckImport = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        STAGING_FILE="$HOME/.config/streamdeck/profiles.json"
        EXPORT_DIR="$HOME/.config/streamdeck/exports"
        PROFILES_DIR="$HOME/Library/Application Support/com.elgato.StreamDeck/ProfilesV3"
        
        if [ -f "$STAGING_FILE" ]; then
          echo "Creating Stream Deck profiles..."
          $DRY_RUN_CMD mkdir -p "$EXPORT_DIR"
          $DRY_RUN_CMD mkdir -p "$PROFILES_DIR"
          
          # Process each profile from staging JSON
          for PROFILE_KEY in $(${pkgs.jq}/bin/jq -r 'keys[]' "$STAGING_FILE"); do
            PROFILE_ID=$(${pkgs.jq}/bin/jq -r ".\"$PROFILE_KEY\".id" "$STAGING_FILE")
            PROFILE_NAME=$(${pkgs.jq}/bin/jq -r ".\"$PROFILE_KEY\".manifest.Name" "$STAGING_FILE")
            PROFILE_MANIFEST=$(${pkgs.jq}/bin/jq -c ".\"$PROFILE_KEY\".manifest" "$STAGING_FILE")
            
            # Get profile ID in uppercase for folder name
            PROFILE_ID_UPPER=$(echo "$PROFILE_ID" | tr '[:lower:]' '[:upper:]')
            
            echo "Creating profile: $PROFILE_NAME ($PROFILE_ID_UPPER)"
            
            # === Direct write to ProfilesV3 ===
            DIRECT_PATH="$PROFILES_DIR/$PROFILE_ID_UPPER.sdProfile"
            $DRY_RUN_CMD mkdir -p "$DIRECT_PATH/Profiles"
            
            # Write profile manifest (keep lowercase UUIDs in manifest, use device UUID)
            echo "$PROFILE_MANIFEST" > "$DIRECT_PATH/manifest.json"
            
            # Process each page
            for PAGE_KEY in $(${pkgs.jq}/bin/jq -r ".\"$PROFILE_KEY\".pages | keys[]" "$STAGING_FILE"); do
              PAGE_ID=$(${pkgs.jq}/bin/jq -r ".\"$PROFILE_KEY\".pages.\"$PAGE_KEY\".id" "$STAGING_FILE")
              PAGE_MANIFEST=$(${pkgs.jq}/bin/jq -c ".\"$PROFILE_KEY\".pages.\"$PAGE_KEY\".manifest" "$STAGING_FILE")
              
              # Folder names are uppercase, but manifest references stay lowercase
              PAGE_ID_UPPER=$(echo "$PAGE_ID" | tr '[:lower:]' '[:upper:]')
              PAGE_PATH="$DIRECT_PATH/Profiles/$PAGE_ID_UPPER"
              $DRY_RUN_CMD mkdir -p "$PAGE_PATH/Images"
              
              # Write page manifest
              echo "$PAGE_MANIFEST" | ${pkgs.jq}/bin/jq '.' > "$PAGE_PATH/manifest.json"
              
              # Copy staged icons for this page
              ICONS_STAGING="$HOME/.config/streamdeck/icons/$PROFILE_KEY/$PAGE_KEY"
              if [ -d "$ICONS_STAGING" ]; then
                echo "  Copying icons for page: $PAGE_KEY"
                cp "$ICONS_STAGING"/*.png "$PAGE_PATH/Images/" 2>/dev/null || true
              fi
            done
            
            echo "  Written to: $DIRECT_PATH"
            
            # === Also create portable export (.streamDeckProfile) ===
            TEMP_DIR=$(mktemp -d)
            EXPORT_PATH="$TEMP_DIR/Profiles/$PROFILE_ID_UPPER.sdProfile"
            mkdir -p "$EXPORT_PATH/Profiles"
            
            # Write package.json for export
            cat > "$TEMP_DIR/package.json" << PKGJSON
{
  "AppVersion": "7.1.1.22340",
  "DeviceModel": "${cfg.deviceModel}",
  "DeviceSettings": null,
  "FormatVersion": 1,
  "OSType": "macOS",
  "OSVersion": "26.2.0",
  "RequiredPlugins": [
    "com.elgato.streamdeck.system.open",
    "com.elgato.streamdeck.system.hotkey",
    "com.elgato.streamdeck.system.website",
    "com.elgato.streamdeck.system.multimedia",
    "com.elgato.streamdeck.profile.openchild",
    "com.elgato.streamdeck.profile.backtoparent",
    "com.elgato.streamdeck.page"
  ]
}
PKGJSON
            
            # For export, use generic device UUID so it can be imported on any device
            echo "$PROFILE_MANIFEST" | ${pkgs.jq}/bin/jq '.Device.UUID = "a3848928-c425-4653-b20c-d2578efeac88"' > "$EXPORT_PATH/manifest.json"
            
            # Copy pages to export
            for PAGE_KEY in $(${pkgs.jq}/bin/jq -r ".\"$PROFILE_KEY\".pages | keys[]" "$STAGING_FILE"); do
              PAGE_ID=$(${pkgs.jq}/bin/jq -r ".\"$PROFILE_KEY\".pages.\"$PAGE_KEY\".id" "$STAGING_FILE")
              PAGE_MANIFEST=$(${pkgs.jq}/bin/jq -c ".\"$PROFILE_KEY\".pages.\"$PAGE_KEY\".manifest" "$STAGING_FILE")
              PAGE_ID_UPPER=$(echo "$PAGE_ID" | tr '[:lower:]' '[:upper:]')
              PAGE_PATH="$EXPORT_PATH/Profiles/$PAGE_ID_UPPER"
              mkdir -p "$PAGE_PATH/Images"
              echo "$PAGE_MANIFEST" | ${pkgs.jq}/bin/jq '.' > "$PAGE_PATH/manifest.json"
              
              # Copy staged icons to export
              ICONS_STAGING="$HOME/.config/streamdeck/icons/$PROFILE_KEY/$PAGE_KEY"
              if [ -d "$ICONS_STAGING" ]; then
                cp "$ICONS_STAGING"/*.png "$PAGE_PATH/Images/" 2>/dev/null || true
              fi
            done
            
            # Create zip export
            EXPORT_FILE="$EXPORT_DIR/$PROFILE_KEY.streamDeckProfile"
            (cd "$TEMP_DIR" && ${pkgs.zip}/bin/zip -qr "$EXPORT_FILE" .)
            rm -rf "$TEMP_DIR"
            
            echo "  Export: $EXPORT_FILE"
          done
          
          echo ""
          echo "Stream Deck profiles written to: $PROFILES_DIR"
          echo "Portable exports in: $EXPORT_DIR"
          ${if cfg.autoRestart then ''
          echo "Restarting Stream Deck..."
          killall 'Elgato Stream Deck' 2>/dev/null || true
          sleep 1
          /usr/bin/open -a 'Elgato Stream Deck'
          '' else ''
          echo ""
          echo "Restart Stream Deck to load changes:"
          echo "  killall 'Elgato Stream Deck' 2>/dev/null; sleep 1; open -a 'Elgato Stream Deck'"
          ''}
        fi
      '';
    };
  });
}
