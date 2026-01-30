# Stream Deck Nix Module

Declarative configuration for Elgato Stream Deck using home-manager.

## Features

- **Full key code support**: Letters A-Z, numbers 0-9, function keys F1-F12, special keys
- **Modifier keys**: cmd, ctrl, option/alt, shift (with proper KeyModifiers calculation)
- **Action types**: hotkey, open, website, folder, backToParent, multimedia, nextPage, prevPage
- **Multi-page profiles**: Support for folders and nested pages
- **Automatic activation**: Restarts Stream Deck app when configuration changes
- **Icon support**: Copy custom icons to profile directories

## Usage

### Basic Configuration

```nix
# In your home-manager config
programs.streamdeck = {
  enable = true;
  
  # Device info (get from existing profile or System Information)
  deviceId = "@(1)[4057/128/DL51K1A60852]";
  deviceModel = "20GBA9901";
  
  profiles.main = {
    name = "My Profile";
    
    pages.default = {
      buttons = {
        # Position: "column,row" (0-indexed)
        "0,0" = {
          name = "Screenshot";
          type = "hotkey";
          keys = ["cmd" "shift" "4"];
        };
        
        "1,0" = {
          name = "Chrome";
          type = "open";
          path = "/Applications/Google Chrome.app";
        };
        
        "2,0" = {
          name = "GitHub";
          type = "website";
          url = "https://github.com";
        };
      };
    };
  };
};
```

### Key Names

**Modifiers:**
- `cmd`, `command`, `meta`, `super` → Command key
- `ctrl`, `control` → Control key
- `option`, `alt`, `opt` → Option/Alt key
- `shift` → Shift key

**Letters:** `a` through `z` (case insensitive)

**Numbers:** `0` through `9`

**Function Keys:** `f1` through `f12`

**Special Keys:**
- `space`, `tab`, `enter`, `return`, `escape`, `esc`
- `backspace`, `delete`
- `left`, `right`, `up`, `down`
- `pageup`, `pagedown`, `home`, `end`
- Symbols: `minus`, `equal`, `bracketleft`, `bracketright`, `semicolon`, `quote`, `comma`, `period`, `slash`, `grave`

### Action Types

| Type | Required Options | Description |
|------|-----------------|-------------|
| `hotkey` | `keys = [...]` | Send keyboard shortcut |
| `open` | `path = "..."` | Open application/file |
| `website` | `url = "..."` | Open URL in browser |
| `folder` | `targetProfile = "..."` | Navigate to subfolder |
| `backToParent` | (none) | Go back to parent folder |
| `multimedia` | `actionIdx = N` | Media control (4=mute, 5=vol down, 6=vol up) |
| `nextPage` | (none) | Go to next page |
| `prevPage` | (none) | Go to previous page |

### Button Options

```nix
"0,0" = {
  name = "Button Name";        # Required
  type = "hotkey";             # Required: action type
  
  # Type-specific:
  keys = ["cmd" "shift" "a"];  # For hotkey
  path = "/Applications/...";  # For open
  url = "https://...";         # For website
  openInBrowser = true;        # For website (default: true)
  actionIdx = 4;               # For multimedia
  targetProfile = "uuid";      # For folder
  
  # Visual options:
  icon = ./icons/myicon.png;   # Custom icon (path)
  title = "Custom Title";      # Override displayed title
  showTitle = true;            # Show/hide title
  fontSize = 12;               # Title font size
  titleColor = "#ffffff";      # Title color (hex)
  titleAlignment = "bottom";   # top, middle, bottom
};
```

## Files

- `streamdeck.nix` - Main module
- `streamdeck/keycodes.nix` - macOS key code mappings
- `streamdeck-config.nix` - Example configuration

## Testing

```bash
# Verify key code generation
cd ~/nix/modules/home-manager/streamdeck
nix-instantiate --eval --strict ./test.nix --json | jq .

# Verify profile generation
nix-instantiate --eval --strict ./test-profile.nix
```

## Finding Your Device Info

Your device UUID and model can be found in existing profiles:

```bash
cat ~/Library/Application\ Support/com.elgato.StreamDeck/ProfilesV3/*/manifest.json | jq .Device
```

## How It Works

The module generates profile files in the Stream Deck format (v3.0):

```
~/Library/Application Support/com.elgato.StreamDeck/ProfilesV3/
└── <profile-uuid>.sdProfile/
    ├── manifest.json          # Root manifest with device info
    ├── Images/                 # Profile-level images
    └── Profiles/
        └── <page-uuid>/
            ├── manifest.json   # Page manifest with button actions
            └── Images/         # Page-level button icons
```

When activated, the module:
1. Generates all profile JSON files
2. Copies icon images to correct locations
3. Restarts the Stream Deck app to load changes
