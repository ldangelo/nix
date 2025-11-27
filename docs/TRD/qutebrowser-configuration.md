# Technical Requirements Document: Qutebrowser Configuration

**Version:** 1.1.0
**Created:** 2025-11-27
**Updated:** 2025-11-27
**Status:** Approved - Ready for Implementation
**PRD Reference:** [docs/PRD/qutebrowser-configuration.md](../PRD/qutebrowser-configuration.md) v1.1.0

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-11-27 | Initial TRD based on PRD v1.1.0 |
| 1.1.0 | 2025-11-27 | Refined: Consolidated to 2 sprints, choose-gui for macOS, implementation-ready |

---

## 1. Executive Summary

This TRD translates the approved PRD for qutebrowser configuration into actionable technical tasks. The implementation creates a modular home-manager configuration with Space-leader keybindings, 1Password integration, custom search engines, and userscript support.

### Scope

- **In Scope**: 6 nix module files, keybindings, search engines, quickmarks, userscripts, 1Password integration
- **Out of Scope**: Greasemonkey scripts (P3, deferred), GUI configuration, browser sync

### Technical Approach

Migrate existing qutebrowser config from `programs.nix` to a dedicated `qutebrowser/` module directory, following the established pattern used by `nvim/`, `emacs/`, and `borders/` modules.

---

## 2. System Architecture

### 2.1 Module Dependency Graph

```
modules/home-manager/default.nix
    │
    ├── imports ./qutebrowser (NEW)
    │       │
    │       ├── default.nix ─────────────────┐
    │       │       │                        │
    │       │       ├── imports ./settings.nix
    │       │       ├── imports ./keybindings.nix
    │       │       ├── imports ./search-engines.nix
    │       │       ├── imports ./quickmarks.nix
    │       │       └── imports ./userscripts.nix
    │       │
    │       └── scripts/
    │               └── org-capture (shell script)
    │
    ├── programs.nix (MODIFY: remove qutebrowser section)
    │
    └── (existing catppuccin.qutebrowser.enable = true)
```

### 2.2 File Structure

```
modules/home-manager/qutebrowser/
├── default.nix          # Module entry point, imports all sub-modules
├── settings.nix         # Core settings (UI, privacy, content, downloads)
├── keybindings.nix      # Normal, insert, command mode bindings
├── search-engines.nix   # 15 search engine definitions
├── quickmarks.nix       # 12 pre-populated quickmarks
├── userscripts.nix      # xdg.dataFile for userscript installation
└── scripts/
    └── org-capture      # Custom org-capture userscript
```

### 2.3 Data Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                    darwin-rebuild switch                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   home-manager activation                        │
└─────────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
    ┌─────────────┐   ┌─────────────┐   ┌─────────────┐
    │ config.py   │   │ quickmarks  │   │ userscripts │
    │ generated   │   │ file        │   │ installed   │
    └─────────────┘   └─────────────┘   └─────────────┘
              │               │               │
              ▼               ▼               ▼
    ~/.config/qutebrowser/   ~/.config/qutebrowser/   ~/.local/share/qutebrowser/
         config.py                quickmarks              userscripts/
```

### 2.4 Integration Points

| Component | Integration Method | Notes |
|-----------|-------------------|-------|
| Catppuccin theme | `catppuccin.qutebrowser.enable` | Already configured in default.nix |
| 1Password CLI | `op` command via userscript | Requires choose-gui, jq |
| mpv | `spawn mpv {url}` command | Direct shell spawn |
| Emacs org-capture | Custom userscript | Calls emacsclient |
| choose-gui | Selection UI for 1Password | Native macOS fuzzy finder |

---

## 3. Master Task List

### 3.1 Task Overview

| Phase | Tasks | Priority | Dependencies |
|-------|-------|----------|--------------|
| Phase 1: Setup | T01-T03 | P0 | None |
| Phase 2: Core Config | T04-T07 | P0 | Phase 1 |
| Phase 3: Keybindings | T08-T10 | P0 | Phase 2 |
| Phase 4: Extensions | T11-T14 | P1 | Phase 3 |
| Phase 5: Integration | T15-T17 | P1 | Phase 4 |
| Phase 6: Testing | T18-T21 | P0 | Phase 5 |

### 3.2 Detailed Task Breakdown

#### Phase 1: Project Setup (P0)

| ID | Task | Description | Acceptance Criteria | Dependencies |
|----|------|-------------|---------------------|--------------|
| **T01** | Create module directory | Create `modules/home-manager/qutebrowser/` directory structure | Directory exists with proper structure | None |
| **T02** | Create default.nix scaffold | Create entry point that imports all sub-modules | File compiles without errors | T01 |
| **T03** | Update parent imports | Add `./qutebrowser` to `modules/home-manager/default.nix` imports | Module loads in nix evaluation | T02 |

#### Phase 2: Core Configuration (P0)

| ID | Task | Description | Acceptance Criteria | Dependencies |
|----|------|-------------|---------------------|--------------|
| **T04** | Create settings.nix | Implement UI, privacy, content, download settings | All settings from PRD 4.7 applied | T02 |
| **T05** | Migrate existing config | Move settings from programs.nix to settings.nix | No duplicate configuration | T04 |
| **T06** | Remove old config | Remove qutebrowser section from programs.nix | programs.nix has no qutebrowser config | T05 |
| **T07** | Test basic launch | Verify qutebrowser launches with new config | Browser opens with correct settings | T06 |

#### Phase 3: Keybindings (P0)

| ID | Task | Description | Acceptance Criteria | Dependencies |
|----|------|-------------|---------------------|--------------|
| **T08** | Create keybindings.nix | Implement normal mode vim-style bindings | All PRD 4.2 normal mode bindings work | T04 |
| **T09** | Add Space leader bindings | Implement SPC-prefix command bindings | All PRD 4.2 leader bindings registered | T08 |
| **T10** | Add insert mode bindings | Implement emacs-style insert mode bindings | Ctrl-a/e/h/w/u/k work in insert mode | T08 |

#### Phase 4: Search & Navigation (P1)

| ID | Task | Description | Acceptance Criteria | Dependencies |
|----|------|-------------|---------------------|--------------|
| **T11** | Create search-engines.nix | Define 15 search engine shortcuts | All PRD 4.3 engines accessible via `o` | T04 |
| **T12** | Create quickmarks.nix | Define 12 pre-populated quickmarks | All PRD 4.4 quickmarks accessible | T04 |
| **T13** | Add per-domain settings | Configure site-specific cookie/JS settings | GitHub, Google, DuckDuckGo have custom settings | T04 |
| **T14** | Test search engines | Verify each search engine works | `o gh test` searches GitHub | T11 |

#### Phase 5: Userscripts & Integration (P1)

| ID | Task | Description | Acceptance Criteria | Dependencies |
|----|------|-------------|---------------------|--------------|
| **T15** | Create userscripts.nix | Set up xdg.dataFile for userscript installation | Userscripts directory created | T04 |
| **T16** | Install qute-1pass | Copy 1Password userscript with exec bit | `SPC p` triggers 1Password | T15, T09 |
| **T17** | Create org-capture script | Write custom org-capture userscript | `SPC c` opens emacs capture | T15, T09 |

#### Phase 6: Testing & Validation (P0)

| ID | Task | Description | Acceptance Criteria | Dependencies |
|----|------|-------------|---------------------|--------------|
| **T18** | Run darwin-rebuild | Execute full rebuild with new config | Build succeeds without errors | T01-T17 |
| **T19** | Test navigation | Verify vim-style navigation works | hjkl, f, H/L, J/K all functional | T18 |
| **T20** | Test 1Password | Verify credential autofill works | Login credentials fill automatically | T18 |
| **T21** | Document any issues | Create notes on workarounds or limitations | README updated if needed | T18-T20 |

### 3.3 Task Checklist

```
Phase 1: Project Setup
- [ ] T01: Create module directory structure
- [ ] T02: Create default.nix scaffold
- [ ] T03: Update parent imports in default.nix

Phase 2: Core Configuration
- [ ] T04: Create settings.nix with all UI/privacy settings
- [ ] T05: Migrate existing config from programs.nix
- [ ] T06: Remove qutebrowser section from programs.nix
- [ ] T07: Test basic qutebrowser launch

Phase 3: Keybindings
- [ ] T08: Create keybindings.nix with normal mode bindings
- [ ] T09: Add Space leader key bindings
- [ ] T10: Add insert mode emacs bindings

Phase 4: Search & Navigation
- [ ] T11: Create search-engines.nix with 15 engines
- [ ] T12: Create quickmarks.nix with 12 bookmarks
- [ ] T13: Add per-domain settings for GitHub/Google
- [ ] T14: Test all search engine shortcuts

Phase 5: Userscripts & Integration
- [ ] T15: Create userscripts.nix framework
- [ ] T16: Install qute-1pass userscript
- [ ] T17: Create custom org-capture script

Phase 6: Testing & Validation
- [ ] T18: Run darwin-rebuild switch successfully
- [ ] T19: Test all navigation keybindings
- [ ] T20: Test 1Password integration end-to-end
- [ ] T21: Document any issues or limitations
```

---

## 4. Technical Specifications

### 4.1 Module: default.nix

```nix
# modules/home-manager/qutebrowser/default.nix
{ config, pkgs, lib, ... }:

{
  imports = [
    ./settings.nix
    ./keybindings.nix
    ./search-engines.nix
    ./quickmarks.nix
    ./userscripts.nix
  ];

  # Enable qutebrowser
  programs.qutebrowser.enable = true;
}
```

### 4.2 Module: settings.nix

```nix
# modules/home-manager/qutebrowser/settings.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.settings = {
    # UI Settings
    tabs.position = "left";
    tabs.width = "15%";
    tabs.show = "multiple";
    tabs.padding = { top = 4; bottom = 4; left = 8; right = 8; };
    tabs.indicator.width = 0;
    tabs.favicons.scale = 1.2;
    tabs.title.format = "{audio}{current_title}";
    statusbar.show = "in-mode";
    scrolling.smooth = true;
    window.title_format = "{perc}{current_title}";

    # Fonts
    fonts.default_family = "Source Code Pro";
    fonts.hints = "bold 18pt Source Code Pro";

    # Hints
    hints.radius = 3;
    hints.padding = { top = 2; bottom = 2; left = 4; right = 4; };
    hints.uppercase = true;
    hints.border = "none";

    # Privacy
    content.cookies.accept = "no-3rdparty";
    content.javascript.enabled = true;  # Enabled by default per PRD

    # Content
    colors.webpage.preferred_color_scheme = "dark";
    colors.webpage.darkmode.enabled = true;
    content.pdfjs = true;

    # Downloads
    downloads.location.directory = "~/Downloads";
    downloads.location.prompt = false;
    downloads.position = "bottom";

    # Completion
    completion.height = "30%";
    completion.scrollbar.width = 0;
  };

  # Per-domain settings
  programs.qutebrowser.settings."content.cookies.accept".patterns = {
    "*.github.com" = "all";
    "*.google.com" = "no-3rdparty";
    "*.duckduckgo.com" = "no-3rdparty";
  };
}
```

### 4.3 Module: keybindings.nix

```nix
# modules/home-manager/qutebrowser/keybindings.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.keyBindings = {
    normal = {
      # Space leader bindings (Spacemacs/Doom style)
      "<Space>p" = "spawn --userscript qute-1pass";
      "<Space>P" = "spawn --userscript qute-1pass --totp";
      "<Space>m" = "spawn mpv {url}";
      "<Space>M" = "hint links spawn mpv {hint-url}";
      "<Space>c" = "spawn --userscript org-capture";
      "<Space>d" = "download";
      "<Space>D" = "hint links download";
      "<Space>b" = "bookmark-add";
      "<Space>q" = "quickmark-save";
      "<Space>Q" = "set-cmd-text -s :quickmark-load";
      "<Space>s" = "set-cmd-text -s :session-save";
      "<Space>l" = "set-cmd-text -s :session-load";
      "<Space>v" = "spawn --userscript view-source";
      "<Space>r" = "spawn --userscript readability";
      "<Space>h" = "help";
      "<Space>/" = "set-cmd-text /";

      # Tab navigation (vim-style)
      "J" = "tab-prev";
      "K" = "tab-next";
      "x" = "tab-close";
      "X" = "undo";

      # History navigation
      "H" = "back";
      "L" = "forward";
    };

    insert = {
      # Emacs-style editing
      "<Ctrl-a>" = "fake-key <Home>";
      "<Ctrl-e>" = "fake-key <End>";
      "<Ctrl-h>" = "fake-key <Backspace>";
      "<Ctrl-w>" = "fake-key <Ctrl-Backspace>";
      "<Ctrl-u>" = "fake-key <Shift-Home><Delete>";
      "<Ctrl-k>" = "fake-key <Shift-End><Delete>";
      "<Ctrl-[>" = "mode-leave";
    };

    command = {
      "<Ctrl-a>" = "rl-beginning-of-line";
      "<Ctrl-e>" = "rl-end-of-line";
      "<Ctrl-[>" = "mode-leave";
    };

    prompt = {
      "<Ctrl-a>" = "rl-beginning-of-line";
      "<Ctrl-e>" = "rl-end-of-line";
      "<Ctrl-[>" = "mode-leave";
    };
  };

  # Key mappings (global remaps)
  programs.qutebrowser.keyMappings = {
    "<Ctrl-[>" = "<Escape>";
  };
}
```

### 4.4 Module: search-engines.nix

```nix
# modules/home-manager/qutebrowser/search-engines.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.searchEngines = {
    DEFAULT = "https://duckduckgo.com/?q={}";
    g = "https://www.google.com/search?q={}";
    gh = "https://github.com/search?q={}";
    nix = "https://search.nixos.org/packages?query={}";
    nixopt = "https://search.nixos.org/options?query={}";
    hm = "https://home-manager-options.extranix.com/?query={}";
    w = "https://en.wikipedia.org/wiki/{}";
    yt = "https://www.youtube.com/results?search_query={}";
    r = "https://www.reddit.com/search?q={}";
    so = "https://stackoverflow.com/search?q={}";
    mdn = "https://developer.mozilla.org/en-US/search?q={}";
    rs = "https://doc.rust-lang.org/std/?search={}";
    py = "https://docs.python.org/3/search.html?q={}";
    npm = "https://www.npmjs.com/search?q={}";
    crates = "https://crates.io/search?q={}";
  };
}
```

### 4.5 Module: quickmarks.nix

```nix
# modules/home-manager/qutebrowser/quickmarks.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.quickmarks = {
    gh = "https://github.com";
    ghme = "https://github.com/ldangelo";
    nixpkgs = "https://github.com/NixOS/nixpkgs";
    hm = "https://github.com/nix-community/home-manager";
    hmopt = "https://home-manager-options.extranix.com/";
    nixsearch = "https://search.nixos.org/packages";
    claude = "https://claude.ai";
    hn = "https://news.ycombinator.com";
    reddit = "https://www.reddit.com";
    yt = "https://www.youtube.com";
    mail = "https://mail.google.com";
    cal = "https://calendar.google.com";
  };
}
```

### 4.6 Module: userscripts.nix

```nix
# modules/home-manager/qutebrowser/userscripts.nix
{ config, pkgs, lib, ... }:

{
  # Install userscripts to XDG data directory
  xdg.dataFile = {
    # 1Password integration (fetched from upstream)
    "qutebrowser/userscripts/qute-1pass" = {
      source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/qutebrowser/qutebrowser/main/misc/userscripts/qute-1pass";
        sha256 = "sha256-PLACEHOLDER";  # Update with actual hash on first build
      };
      executable = true;
    };

    # Custom org-capture script
    "qutebrowser/userscripts/org-capture" = {
      source = ./scripts/org-capture;
      executable = true;
    };
  };

  # Ensure required dependencies are installed
  home.packages = with pkgs; [
    choose-gui  # Native macOS fuzzy finder for 1Password selection
    jq          # JSON parsing for 1Password
  ];
}
```

**Note**: The qute-1pass script may need modification to use `choose` instead of `rofi`. Environment variable `QUTE_1PASS_DMENU_CMD` can be set to override the default selector.

### 4.7 Script: org-capture

```bash
#!/usr/bin/env bash
# modules/home-manager/qutebrowser/scripts/org-capture
# Capture current page to Emacs org-mode

TITLE="$QUTE_TITLE"
URL="$QUTE_URL"

# Use emacsclient to capture
emacsclient -e "(org-capture-string \"* [[${URL}][${TITLE}]]\" \"w\")" \
    || echo "message-error 'Failed to capture to org-mode'" >> "$QUTE_FIFO"

echo "message-info 'Captured: ${TITLE}'" >> "$QUTE_FIFO"
```

---

## 5. Sprint Planning (Consolidated)

### 5.1 Sprint 1: Core Implementation (Tasks T01-T14)

**Goal**: Create complete module structure with all configuration

**Duration**: 1 session

**Tasks**:

**Phase A: Module Setup (T01-T07)**
- [ ] T01: Create `modules/home-manager/qutebrowser/` directory
- [ ] T02: Create `default.nix` with module imports
- [ ] T03: Add import to `modules/home-manager/default.nix`
- [ ] T04: Create `settings.nix` with all UI/privacy settings
- [ ] T05: Migrate existing config from `programs.nix`
- [ ] T06: Remove qutebrowser section from `programs.nix`
- [ ] T07: Test `darwin-rebuild switch` and verify launch

**Phase B: Keybindings (T08-T10)**
- [ ] T08: Create `keybindings.nix` with normal mode vim bindings
- [ ] T09: Add Space leader key bindings
- [ ] T10: Add insert mode emacs-style bindings

**Phase C: Search & Navigation (T11-T14)**
- [ ] T11: Create `search-engines.nix` with 15 engines
- [ ] T12: Create `quickmarks.nix` with 12 bookmarks
- [ ] T13: Add per-domain cookie/JS settings
- [ ] T14: Test search engine functionality

**Validation Checkpoint**:
```bash
darwin-rebuild switch
# Verify: qutebrowser launches, j/k scrolls, SPC h opens help
# Verify: o gh test → GitHub search works
```

### 5.2 Sprint 2: Integration & Testing (Tasks T15-T21)

**Goal**: Complete userscript integration and full validation

**Duration**: 1 session

**Tasks**:

**Phase A: Userscripts (T15-T17)**
- [ ] T15: Create `userscripts.nix` with xdg.dataFile
- [ ] T16: Install `qute-1pass` userscript (with choose-gui)
- [ ] T17: Create custom `org-capture` script

**Phase B: Final Testing (T18-T21)**
- [ ] T18: Run full `darwin-rebuild switch`
- [ ] T19: Test all navigation keybindings systematically
- [ ] T20: Test 1Password login flow end-to-end
- [ ] T21: Document any issues or workarounds

**Final Validation**:
```
Open qutebrowser
- SPC p → choose-gui shows 1Password items
- SPC c → Emacs org-capture opens
- SPC m on YouTube → mpv plays video
- All PRD acceptance criteria pass
```

---

## 6. Quality Requirements

### 6.1 Code Quality

| Requirement | Standard | Validation |
|-------------|----------|------------|
| Nix formatting | Follow nixpkgs style guide | `nixfmt` passes |
| Module separation | Single responsibility per file | Code review |
| Comments | Non-obvious settings documented | Code review |
| No hardcoded paths | Use `~` or `$HOME` | Code review |

### 6.2 Testing Requirements

| Test Type | Scope | Method |
|-----------|-------|--------|
| Build validation | All modules compile | `darwin-rebuild switch` |
| Functional | All keybindings work | Manual testing |
| Integration | 1Password fills credentials | Manual testing |
| Regression | Catppuccin theme still applies | Visual inspection |

### 6.3 Security Requirements

| Requirement | Implementation | Validation |
|-------------|----------------|------------|
| No plaintext secrets | 1Password via CLI | Code review |
| Third-party cookie blocking | `content.cookies.accept = "no-3rdparty"` | Browser settings |
| Userscript permissions | Scripts use `$QUTE_FIFO` only | Script audit |

---

## 7. Risk Assessment

### 7.1 Technical Risks

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| qute-1pass script not in nixpkgs package | Medium | High | Fetch from upstream GitHub |
| Space key conflicts with default bindings | Low | Medium | Use `enableDefaultBindings = true` |
| Catppuccin overrides custom colors | Low | Low | Don't define colors in settings.nix |
| rofi not available on macOS | Medium | High | Use `dmenu` or `choose` as alternative |

### 7.2 Mitigation Strategies

**qute-1pass script location**:
```nix
# Fetch directly from upstream (already in userscripts.nix spec)
xdg.dataFile."qutebrowser/userscripts/qute-1pass".source = pkgs.fetchurl {
  url = "https://raw.githubusercontent.com/qutebrowser/qutebrowser/main/misc/userscripts/qute-1pass";
  sha256 = "sha256-PLACEHOLDER";
};
```

**choose-gui for macOS selection UI**:
```nix
# Use native macOS choose-gui
home.packages = with pkgs; [ choose-gui jq ];

# Set environment variable for qute-1pass to use choose
home.sessionVariables = {
  QUTE_1PASS_DMENU_CMD = "choose";
};
```

---

## 8. Dependencies

### 8.1 Build Dependencies

| Dependency | Purpose | Source |
|------------|---------|--------|
| home-manager | Module system | flake input |
| pkgs.qutebrowser | Browser package | nixpkgs |
| pkgs.choose-gui | Selection UI (macOS native) | nixpkgs |
| pkgs.jq | JSON parsing | nixpkgs |

### 8.2 Runtime Dependencies

| Dependency | Purpose | Installation |
|------------|---------|--------------|
| 1Password CLI (`op`) | Password management | homebrew cask |
| mpv | Video playback | nixpkgs or homebrew |
| Emacs | Org-capture | Already installed |

### 8.3 Pre-requisites

- [ ] 1Password CLI installed: `brew install --cask 1password-cli`
- [ ] 1Password CLI signed in: `op signin`
- [ ] Emacs running as server: `emacs --daemon`

---

## 9. Acceptance Criteria Checklist

### From PRD Section 6

#### 6.1 Module Structure
- [ ] `modules/home-manager/qutebrowser/default.nix` exists and imports sub-modules
- [ ] All 6 module files created (default, settings, keybindings, search-engines, quickmarks, userscripts)
- [ ] Module is imported in `modules/home-manager/default.nix`
- [ ] `darwin-rebuild switch` succeeds without errors
- [ ] Qutebrowser launches with applied configuration

#### 6.2 Keybindings
- [ ] All normal mode bindings work as documented
- [ ] Space leader key triggers custom commands
- [ ] `SPC p` opens 1Password credential selection
- [ ] Insert mode emacs-style bindings work
- [ ] `Ctrl-[` exits insert mode

#### 6.3 Userscripts
- [ ] Userscripts are installed to `~/.local/share/qutebrowser/userscripts/`
- [ ] Scripts have executable bit set
- [ ] `SPC p` opens 1Password via qute-1pass
- [ ] `SPC m` opens current URL in mpv
- [ ] `SPC c` triggers org-capture

#### 6.4 Search Engines
- [ ] `o nix home-manager` searches NixOS packages
- [ ] `o gh nix-community/home-manager` searches GitHub
- [ ] `o mdn flexbox` searches MDN
- [ ] DuckDuckGo is default search engine

#### 6.5 Quickmarks
- [ ] All 12 pre-populated quickmarks accessible
- [ ] `SPC Q gh` opens GitHub
- [ ] `b gh<Enter>` opens GitHub

#### 6.6 Integration
- [ ] Catppuccin theme applied (via existing catppuccin module)
- [ ] Config file generated at `~/.config/qutebrowser/config.py`
- [ ] No conflicts with chezmoi-managed files
- [ ] 1Password CLI (`op`) integration working

---

## 10. References

### Documentation
- [Home Manager Qutebrowser Module](https://github.com/nix-community/home-manager/blob/master/modules/programs/qutebrowser.nix)
- [Qutebrowser Documentation](https://qutebrowser.org/doc/)
- [qute-1pass Script](https://github.com/qutebrowser/qutebrowser/blob/main/misc/userscripts/qute-1pass)

### Related Files in Repository
- `modules/home-manager/default.nix` - Parent module imports
- `modules/home-manager/programs.nix` - Current qutebrowser config (to be migrated)
- `modules/home-manager/nvim/default.nix` - Example modular structure

### PRD Reference
- [docs/PRD/qutebrowser-configuration.md](../PRD/qutebrowser-configuration.md)
