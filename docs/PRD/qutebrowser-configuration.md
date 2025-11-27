# Product Requirements Document: Qutebrowser Configuration

**Version:** 1.1.0
**Created:** 2025-11-27
**Updated:** 2025-11-27
**Status:** Approved

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-11-27 | Initial draft |
| 1.1.0 | 2025-11-27 | Refined based on user feedback: Space leader key, 1Password integration, JS enabled by default, keep org-capture, modular structure, pre-populated quickmarks |

---

## 1. Product Summary

### 1.1 Problem Statement

The current qutebrowser configuration in this nix repository (`modules/home-manager/programs.nix`) provides only basic settings (fonts, tabs, hints, colors). It lacks:

- Custom keybindings beyond defaults
- Userscript integration for extended functionality
- Search engine configuration
- Quickmarks for rapid navigation
- Greasemonkey script support
- Password manager integration (1Password)
- Per-domain settings for site-specific behavior
- Aliases for custom commands

### 1.2 Solution

Create a comprehensive, modular qutebrowser configuration using home-manager's native `programs.qutebrowser` module that:

1. Organizes configuration into a dedicated module (`modules/home-manager/qutebrowser/`)
2. Implements vim-style keybindings with **Space as leader key** (Spacemacs/Doom-style)
3. Integrates userscripts for **1Password**, media playback, and org-mode capture
4. Configures search engines with quick-access shortcuts
5. Applies Catppuccin theming (already enabled via `catppuccin.qutebrowser.enable`)

### 1.3 Value Proposition

- **Declarative**: All configuration in nix, reproducible across machines
- **Keyboard-first**: Spacemacs-style navigation without mouse dependency
- **Extensible**: Modular structure for easy customization
- **Integrated**: Works with existing tools (1Password, mpv, emacs org-capture)

---

## 2. User Analysis

### 2.1 Primary User

**Developer/Power User** who:
- Uses keyboard-driven workflows extensively
- Manages dotfiles declaratively with nix/home-manager
- Values privacy and minimal browser chrome
- Integrates browser with terminal tools (mpv, 1Password, emacs)
- Uses Spacemacs/Doom Emacs style keybindings

### 2.2 User Pain Points

| Pain Point | Current State | Desired State |
|------------|---------------|---------------|
| No custom keybindings | Using defaults | Space-leader bindings |
| No password integration | Manual password entry | 1Password CLI integration |
| No media handling | Links open in browser | Video links open in mpv |
| No org-mode capture | Manual copy/paste | One-key capture to org |
| No search shortcuts | Default search only | Custom engines (gh, nix, etc.) |

### 2.3 User Journey

1. **Daily Browsing**: Navigate with keyboard (hjkl, f for hints)
2. **Authentication**: Press `SPC p` to autofill credentials via 1Password
3. **Research**: Use `o gh` for GitHub, `o nix` for Nix search
4. **Media**: Press `SPC m` to open video in mpv
5. **Note-taking**: Press `SPC c` to capture page to org-mode

---

## 3. Goals & Non-Goals

### 3.1 Goals

| Priority | Goal | Success Metric |
|----------|------|----------------|
| P0 | Modular nix configuration | Dedicated `qutebrowser/` module with 6 files |
| P0 | Custom keybindings | Space-leader bindings for all common actions |
| P1 | 1Password integration | qute-1pass working with `SPC p` |
| P1 | Search engine shortcuts | 14 custom search engines |
| P1 | Media player integration | Videos open in mpv |
| P2 | Userscript directory | Executable scripts in config |
| P2 | Pre-populated quickmarks | 10+ developer sites configured |
| P2 | Per-domain settings | Site-specific JS/cookie overrides |
| P3 | Greasemonkey scripts | Ad-blocking, site fixes |
| P3 | Org-mode capture | Emacs integration via userscript |

### 3.2 Non-Goals

- GUI configuration (loadAutoconfig remains false)
- Sync/profile management across browsers
- Extension store integration (use greasemonkey instead)
- Built-in ad-blocking (use uBlock via greasemonkey or DNS)
- JavaScript disabled by default (user prefers compatibility)

---

## 4. Functional Requirements

### 4.1 Module Structure

```
modules/home-manager/qutebrowser/
├── default.nix          # Main module, imports others
├── settings.nix         # Core qutebrowser settings
├── keybindings.nix      # All key mappings by mode
├── search-engines.nix   # Search engine definitions
├── quickmarks.nix       # Quickmark definitions
├── userscripts.nix      # Userscript installation (1Password, org-capture, etc.)
└── greasemonkey.nix     # Greasemonkey script packages
```

### 4.2 Keybindings

#### Normal Mode (Vim-style)

| Key | Action | Description |
|-----|--------|-------------|
| `j/k` | scroll down/up | Vim navigation |
| `h/l` | scroll left/right | Vim navigation |
| `gg/G` | scroll to top/bottom | Vim navigation |
| `d/u` | half-page down/up | Vim navigation |
| `f/F` | hint links / new tab | Quick link access |
| `;y` | hint + yank URL | Copy link URL |
| `r/R` | reload / hard reload | Page refresh |
| `H/L` | back / forward | History navigation |
| `J/K` | prev tab / next tab | Tab navigation |
| `x` | close tab | Tab management |
| `X` | undo close | Tab management |
| `o/O` | open URL / new tab | URL entry |
| `t/T` | open in new tab / background | Tab opening |
| `yy` | yank URL | Copy current URL |
| `yt` | yank title | Copy page title |
| `p/P` | open clipboard / new tab | Paste and go |
| `/` | search | Find in page |
| `n/N` | next/prev match | Search navigation |

#### Leader Key Bindings (Space prefix - Spacemacs/Doom style)

| Key | Action | Description |
|-----|--------|-------------|
| `SPC p` | spawn --userscript qute-1pass | 1Password autofill |
| `SPC P` | spawn --userscript qute-1pass --totp | 1Password TOTP |
| `SPC m` | spawn mpv {url} | Open in mpv |
| `SPC M` | hint links spawn mpv {hint-url} | Hint + mpv |
| `SPC c` | spawn --userscript org-capture | Capture to org |
| `SPC d` | download | Download page |
| `SPC D` | hint links download | Download link |
| `SPC b` | bookmark-add | Add bookmark |
| `SPC q` | quickmark-save | Save quickmark |
| `SPC Q` | set-cmd-text -s :quickmark-load | Load quickmark |
| `SPC s` | set-cmd-text -s :session-save | Save session |
| `SPC l` | set-cmd-text -s :session-load | Load session |
| `SPC v` | spawn --userscript view-source | View source |
| `SPC r` | spawn --userscript readability | Reader mode |
| `SPC h` | help | Open help |
| `SPC /` | set-cmd-text / | Search in page |

#### Insert Mode (Emacs-style)

| Key | Action | Description |
|-----|--------|-------------|
| `Ctrl-[` | mode-leave | Escape to normal |
| `Ctrl-a` | fake-key <Home> | Beginning of line |
| `Ctrl-e` | fake-key <End> | End of line |
| `Ctrl-h` | fake-key <Backspace> | Delete char |
| `Ctrl-w` | fake-key <Ctrl-Backspace> | Delete word |
| `Ctrl-u` | fake-key <Shift-Home><Delete> | Delete to start |
| `Ctrl-k` | fake-key <Shift-End><Delete> | Delete to end |

#### Key Mappings (Global remaps)

| From | To | Description |
|------|-----|-------------|
| `<Ctrl-[>` | `<Escape>` | Vim-style escape |

### 4.3 Search Engines

| Shortcut | URL Pattern | Description |
|----------|-------------|-------------|
| `DEFAULT` | `https://duckduckgo.com/?q={}` | Default search |
| `g` | `https://www.google.com/search?q={}` | Google |
| `gh` | `https://github.com/search?q={}` | GitHub |
| `nix` | `https://search.nixos.org/packages?query={}` | Nix packages |
| `nixopt` | `https://search.nixos.org/options?query={}` | NixOS options |
| `hm` | `https://home-manager-options.extranix.com/?query={}` | Home Manager options |
| `w` | `https://en.wikipedia.org/wiki/{}` | Wikipedia |
| `yt` | `https://www.youtube.com/results?search_query={}` | YouTube |
| `r` | `https://www.reddit.com/search?q={}` | Reddit |
| `so` | `https://stackoverflow.com/search?q={}` | Stack Overflow |
| `mdn` | `https://developer.mozilla.org/en-US/search?q={}` | MDN Web Docs |
| `rs` | `https://doc.rust-lang.org/std/?search={}` | Rust docs |
| `py` | `https://docs.python.org/3/search.html?q={}` | Python docs |
| `npm` | `https://www.npmjs.com/search?q={}` | NPM packages |
| `crates` | `https://crates.io/search?q={}` | Rust crates |

### 4.4 Quickmarks (Pre-populated)

| Name | URL | Description |
|------|-----|-------------|
| `gh` | `https://github.com` | GitHub home |
| `ghme` | `https://github.com/ldangelo` | GitHub profile |
| `nixpkgs` | `https://github.com/NixOS/nixpkgs` | Nixpkgs repo |
| `hm` | `https://github.com/nix-community/home-manager` | Home Manager repo |
| `hmopt` | `https://home-manager-options.extranix.com/` | HM options search |
| `nixsearch` | `https://search.nixos.org/packages` | Nix package search |
| `claude` | `https://claude.ai` | Claude AI |
| `hn` | `https://news.ycombinator.com` | Hacker News |
| `reddit` | `https://www.reddit.com` | Reddit |
| `yt` | `https://www.youtube.com` | YouTube |
| `mail` | `https://mail.google.com` | Gmail |
| `cal` | `https://calendar.google.com` | Google Calendar |

### 4.5 Userscripts

| Script | Purpose | Trigger | Source |
|--------|---------|---------|--------|
| `qute-1pass` | 1Password CLI integration | `SPC p` / `SPC P` | [qutebrowser/misc/userscripts](https://github.com/qutebrowser/qutebrowser/blob/main/misc/userscripts/qute-1pass) |
| `org-capture` | Capture to emacs org-mode | `SPC c` | Custom or [doom emacs](https://github.com/doomemacs/doomemacs) |
| `view-source` | View page source in $EDITOR | `SPC v` | qutebrowser built-in |
| `readability` | Extract article content | `SPC r` | [qutebrowser/misc/userscripts](https://github.com/qutebrowser/qutebrowser/tree/main/misc/userscripts) |

#### 1Password Integration Details

The `qute-1pass` userscript requires:
- **1Password CLI** (`op`) installed and configured
- **rofi** or **dmenu** for selection UI
- **jq** for JSON parsing

Features supported:
- Credential autofill (`fill_credentials`)
- TOTP code fill (`fill_totp`)
- Biometric unlock support (`--biometric`)
- Session caching (`--cache-session`)

### 4.6 Per-Domain Settings

```nix
perDomainSettings = {
  # Sites that need specific settings
  "*.github.com" = {
    "content.cookies.accept" = "all";  # GitHub needs cookies
  };
  "*.google.com" = {
    "content.cookies.accept" = "no-3rdparty";
  };
  "file://*" = {
    "content.local_content_can_access_remote_urls" = true;
  };
  # Privacy-focused sites can have stricter settings
  "*.duckduckgo.com" = {
    "content.cookies.accept" = "no-3rdparty";
  };
};
```

### 4.7 Settings Overview

| Category | Setting | Value | Rationale |
|----------|---------|-------|-----------|
| UI | `tabs.position` | `left` | Tree-style tabs |
| UI | `tabs.width` | `15%` | Narrow sidebar |
| UI | `tabs.show` | `multiple` | Hide when single tab |
| UI | `statusbar.show` | `in-mode` | Minimal UI |
| Privacy | `content.cookies.accept` | `no-3rdparty` | Block tracking |
| Privacy | `content.javascript.enabled` | `true` | **Enabled by default** for compatibility |
| Content | `content.pdfjs` | `true` | Built-in PDF viewer |
| Content | `colors.webpage.darkmode.enabled` | `true` | Dark mode |
| Downloads | `downloads.location.directory` | `~/Downloads` | Standard location |
| Downloads | `downloads.location.prompt` | `false` | Auto-save |
| Fonts | `fonts.default_family` | `Source Code Pro` | Consistent with terminal |
| Fonts | `fonts.hints` | `bold 18pt Source Code Pro` | Readable hints |
| Hints | `hints.uppercase` | `true` | Easier to read |
| Hints | `hints.radius` | `3` | Rounded corners |

---

## 5. Non-Functional Requirements

### 5.1 Performance

- Startup time: < 500ms
- Memory usage: < 500MB for 10 tabs
- No userscript-induced lag

### 5.2 Security

- Third-party cookies blocked by default
- 1Password integration via CLI (no browser extension)
- Password manager uses biometric or master password unlock

### 5.3 Accessibility

- High-contrast hint labels
- Configurable font sizes
- Keyboard-only operation possible

### 5.4 Maintainability

- Modular nix files (6 separate files) for separation of concerns
- Comments explaining non-obvious settings
- Version-controlled in nix repository

---

## 6. Acceptance Criteria

### 6.1 Module Structure

- [ ] `modules/home-manager/qutebrowser/default.nix` exists and imports sub-modules
- [ ] All 6 module files created (default, settings, keybindings, search-engines, quickmarks, userscripts)
- [ ] Module is imported in `modules/home-manager/default.nix`
- [ ] `darwin-rebuild switch` succeeds without errors
- [ ] Qutebrowser launches with applied configuration

### 6.2 Keybindings

- [ ] All normal mode bindings work as documented
- [ ] Space leader key triggers custom commands
- [ ] `SPC p` opens 1Password credential selection
- [ ] Insert mode emacs-style bindings work
- [ ] `Ctrl-[` exits insert mode

### 6.3 Userscripts

- [ ] Userscripts are installed to `~/.local/share/qutebrowser/userscripts/`
- [ ] Scripts have executable bit set
- [ ] `SPC p` opens 1Password via qute-1pass
- [ ] `SPC m` opens current URL in mpv
- [ ] `SPC c` triggers org-capture

### 6.4 Search Engines

- [ ] `o nix home-manager` searches NixOS packages
- [ ] `o gh nix-community/home-manager` searches GitHub
- [ ] `o mdn flexbox` searches MDN
- [ ] DuckDuckGo is default search engine

### 6.5 Quickmarks

- [ ] All 12 pre-populated quickmarks accessible
- [ ] `SPC Q gh` opens GitHub
- [ ] `b gh<Enter>` opens GitHub

### 6.6 Integration

- [ ] Catppuccin theme applied (via existing catppuccin module)
- [ ] Config file generated at `~/.config/qutebrowser/config.py`
- [ ] No conflicts with chezmoi-managed files
- [ ] 1Password CLI (`op`) integration working

---

## 7. Test Scenarios

### 7.1 Basic Navigation

1. Open qutebrowser
2. Press `o`, type URL, press Enter
3. Press `f`, type hint chars to click link
4. Press `H` to go back
5. Press `J` to switch to next tab

**Expected**: All navigation works without mouse

### 7.2 Password Entry with 1Password

1. Navigate to login page (e.g., github.com)
2. Press `SPC p`
3. Authenticate with 1Password (biometric/password)
4. Select credential from rofi menu
5. Observe username/password filled

**Expected**: Credentials entered automatically via 1Password CLI

### 7.3 Search Engine Usage

1. Press `o`
2. Type `gh home-manager`
3. Press Enter

**Expected**: GitHub search for "home-manager" opens

### 7.4 Media Playback

1. Navigate to YouTube video
2. Press `SPC m`

**Expected**: Video opens in mpv player

### 7.5 Org-mode Capture

1. Navigate to interesting article
2. Press `SPC c`
3. Emacs org-capture window opens

**Expected**: Page URL and title captured to org-mode

### 7.6 Quickmark Access

1. Press `SPC Q`
2. Type `gh`
3. Press Enter

**Expected**: GitHub.com opens

---

## 8. Dependencies

### 8.1 System Packages

| Package | Purpose | Required | Notes |
|---------|---------|----------|-------|
| `qutebrowser` | Browser | Yes | Already in homebrew |
| `mpv` | Video player | Optional | For media playback |
| `_1password-cli` | Password manager | Yes | `op` command |
| `rofi` | Selection UI | Yes | For 1Password selection |
| `jq` | JSON parsing | Yes | For 1Password parsing |

### 8.2 Nix Configuration

| Component | Status | Notes |
|-----------|--------|-------|
| `programs.qutebrowser` | Existing | Enhance configuration |
| `catppuccin.qutebrowser.enable` | Existing | Already true |
| `home.packages` | Update | Add rofi, jq if missing |

### 8.3 External Requirements

| Requirement | Status | Notes |
|-------------|--------|-------|
| 1Password account | User has | Required for password integration |
| 1Password CLI setup | User to verify | `op signin` must work |
| Emacs with org-mode | User has | For org-capture integration |

---

## 9. Implementation Notes

### 9.1 Migration Path

1. Create new module structure in `modules/home-manager/qutebrowser/`
2. Move existing config from `programs.nix` to new module
3. Remove qutebrowser config from `programs.nix`
4. Update imports in `default.nix`
5. Install required dependencies (rofi, jq)
6. Test with `darwin-rebuild switch`

### 9.2 Userscript Installation

Custom userscripts installed via home-manager:

```nix
# In userscripts.nix
xdg.dataFile = {
  "qutebrowser/userscripts/qute-1pass" = {
    source = "${pkgs.qutebrowser}/share/qutebrowser/userscripts/qute-1pass";
    executable = true;
  };
  "qutebrowser/userscripts/org-capture" = {
    source = ./scripts/org-capture;
    executable = true;
  };
};
```

### 9.3 1Password CLI Setup

User must complete before using:

```bash
# Install 1Password CLI (already in homebrew)
brew install --cask 1password-cli

# Sign in (one time)
op signin

# Verify working
op item list --limit 1
```

### 9.4 Catppuccin Integration

The existing `catppuccin.qutebrowser.enable = true` in `default.nix` will:
- Apply Catppuccin Mocha colors automatically
- Override any color settings in our config
- No additional color configuration needed

---

## 10. References

- [Home Manager Qutebrowser Module](https://github.com/nix-community/home-manager/blob/master/modules/programs/qutebrowser.nix)
- [Qutebrowser Userscripts Documentation](https://qutebrowser.org/doc/userscripts.html)
- [qute-1pass Official Script](https://github.com/qutebrowser/qutebrowser/blob/main/misc/userscripts/qute-1pass)
- [fmartingr/qute-1password](https://github.com/fmartingr/qute-1password) - Alternative 1Password integration
- [MyNixOS Qutebrowser Options](https://mynixos.com/home-manager/option/programs.qutebrowser.keyBindings)
- [Catppuccin Qutebrowser](https://github.com/catppuccin/qutebrowser)
