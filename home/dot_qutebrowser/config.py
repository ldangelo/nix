config.load_autoconfig(False)
config.set("colors.webpage.darkmode.enabled", True)
config.set("colors.webpage.preferred_color_scheme", "dark")
config.set("completion.height", "30%")
config.set("completion.scrollbar.width", 0)
config.set("content.cookies.accept", "no-3rdparty")
config.set("content.javascript.enabled", True)
config.set("content.pdfjs", True)
config.set("downloads.location.directory", "~/Downloads")
config.set("downloads.location.prompt", False)
config.set("downloads.position", "bottom")
config.set("fonts.default_family", "Source Code Pro")
config.set("fonts.hints", "bold 18pt Source Code Pro")
config.set("hints.border", "none")
config.set("hints.padding.bottom", 2)
config.set("hints.padding.left", 4)
config.set("hints.padding.right", 4)
config.set("hints.padding.top", 2)
config.set("hints.radius", 3)
config.set("hints.uppercase", True)
config.set("scrolling.smooth", True)
config.set("statusbar.show", "in-mode")
config.set("tabs.favicons.scale", 1.200000)
config.set("tabs.indicator.width", 0)
config.set("tabs.padding.bottom", 4)
config.set("tabs.padding.left", 8)
config.set("tabs.padding.right", 8)
config.set("tabs.padding.top", 4)
config.set("tabs.position", "left")
config.set("tabs.show", "multiple")
config.set("tabs.title.format", "{audio}{current_title}")
config.set("tabs.width", "15%")
config.set("window.title_format", "{perc}{current_title}")
c.url.searchengines['DEFAULT'] = "https://duckduckgo.com/?q={}"
c.url.searchengines['crates'] = "https://crates.io/search?q={}"
c.url.searchengines['g'] = "https://www.google.com/search?q={}"
c.url.searchengines['gh'] = "https://github.com/search?q={}"
c.url.searchengines['hm'] = "https://home-manager-options.extranix.com/?query={}"
c.url.searchengines['mdn'] = "https://developer.mozilla.org/en-US/search?q={}"
c.url.searchengines['nix'] = "https://search.nixos.org/packages?query={}"
c.url.searchengines['nixopt'] = "https://search.nixos.org/options?query={}"
c.url.searchengines['npm'] = "https://www.npmjs.com/search?q={}"
c.url.searchengines['py'] = "https://docs.python.org/3/search.html?q={}"
c.url.searchengines['r'] = "https://www.reddit.com/search?q={}"
c.url.searchengines['rs'] = "https://doc.rust-lang.org/std/?search={}"
c.url.searchengines['so'] = "https://stackoverflow.com/search?q={}"
c.url.searchengines['w'] = "https://en.wikipedia.org/wiki/{}"
c.url.searchengines['yt'] = "https://www.youtube.com/results?search_query={}"
c.bindings.key_mappings['<Ctrl-[>'] = "<Escape>"
config.bind("<Ctrl-[>", "mode-leave", mode="command")
config.bind("<Ctrl-a>", "rl-beginning-of-line", mode="command")
config.bind("<Ctrl-e>", "rl-end-of-line", mode="command")
config.bind("<Ctrl-[>", "mode-leave", mode="insert")
config.bind("<Ctrl-a>", "fake-key <Home>", mode="insert")
config.bind("<Ctrl-e>", "fake-key <End>", mode="insert")
config.bind("<Ctrl-h>", "fake-key <Backspace>", mode="insert")
config.bind("<Ctrl-k>", "fake-key <Shift-End><Delete>", mode="insert")
config.bind("<Ctrl-u>", "fake-key <Shift-Home><Delete>", mode="insert")
config.bind("<Ctrl-w>", "fake-key <Ctrl-Backspace>", mode="insert")
config.bind("<Space>/", "set-cmd-text /", mode="normal")
config.bind("<Space>D", "hint links download", mode="normal")
config.bind("<Space>M", "hint links spawn mpv {hint-url}", mode="normal")
config.bind("<Space>P", "spawn --userscript qute-1pass --totp", mode="normal")
config.bind("<Space>Q", "set-cmd-text -s :quickmark-load", mode="normal")
config.bind("<Space>b", "bookmark-add", mode="normal")
config.bind("<Space>c", "spawn --userscript org-capture", mode="normal")
config.bind("<Space>d", "download", mode="normal")
config.bind("<Space>h", "help", mode="normal")
config.bind("<Space>l", "set-cmd-text -s :session-load", mode="normal")
config.bind("<Space>m", "spawn mpv {url}", mode="normal")
config.bind("<Space>p", "spawn --userscript qute-1pass", mode="normal")
config.bind("<Space>q", "quickmark-save", mode="normal")
config.bind("<Space>r", "spawn --userscript readability", mode="normal")
config.bind("<Space>s", "set-cmd-text -s :session-save", mode="normal")
config.bind("<Space>v", "spawn --userscript view-source", mode="normal")
config.bind("H", "back", mode="normal")
config.bind("J", "tab-prev", mode="normal")
config.bind("K", "tab-next", mode="normal")
config.bind("L", "forward", mode="normal")
config.bind("X", "undo", mode="normal")
config.bind("x", "tab-close", mode="normal")
config.bind("<Ctrl-[>", "mode-leave", mode="prompt")
config.bind("<Ctrl-a>", "rl-beginning-of-line", mode="prompt")
config.bind("<Ctrl-e>", "rl-end-of-line", mode="prompt")
import catppuccin
# Per-domain cookie settings
config.set('content.cookies.accept', 'all', '*.github.com')
config.set('content.cookies.accept', 'no-3rdparty', '*.google.com')
config.set('content.cookies.accept', 'no-3rdparty', '*.duckduckgo.com')

catppuccin.setup(c, 'mocha', False)