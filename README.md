# Emacs Configuration

Personal Emacs configuration. Cross-platform (Linux/macOS/Windows) with optional EXWM window manager support on Linux.

## Requirements

- Emacs 31+ (emacs-next on Guix)
- Git, ripgrep (`rg`), fd, GnuPG (`gpg`)

Run `M-x kdb-doctor` to check for all optional dependencies.

## Setup

```bash
git clone https://github.com/kborling/emacs.d ~/.emacs.d
cp ~/.emacs.d/personal.el.template ~/.emacs.d/personal.el
# Edit personal.el with your API keys, SQL connections, etc.
```

Packages install automatically on first launch.

## Structure

```
init.el                  Main config — everything loads from here
personal.el              Private settings (API keys, connections — not tracked)
personal.el.template     Template for personal.el
custom.el                Customize output
lisp/
  init-org.el            Org mode, capture templates, org-modern
  init-vc.el             VC/Git transient menu (Magit-like, built-in VC)
  init-exwm.el           EXWM window manager (X11 only)
  init-terminal.el       Terminal/tmux optimizations
  init-claude.el         Claude session archive and browsing
```

## Features

### Completion
- **Minibuffer:** fido-mode + orderless + marginalia
- **In-buffer:** corfu + cape + completion-preview (ghost text)

### Development
- **LSP:** eglot with eglot-booster
- **Tree-sitter:** built-in Emacs 31 `treesit-enabled-modes`
- **Languages:** C/C++, Rust, Zig, Go, TypeScript, Python, C#/.NET, Angular, HTML/CSS
- **Diagnostics:** flymake with eldoc integration
- **VC:** built-in VC with custom transient menu (`C-c g`)

### AI / Claude
- **gptel:** Claude chat in org-mode buffers (`C-c l l`)
- **Capture-to-Claude:** capture notes (`C-c o c a`), send to Claude (`C-c l q`)
- **Session archive:** browse/search Claude Code sessions (`C-c l b`, `C-c l /`)

### Org Mode
- org-modern styling, org-appear
- Capture templates: todo, notes, journal, meetings, 1:1s, projects, decisions, specs
- Refile targets across org files

### Terminal
- Eshell with custom prompt and toggle (`C-backtick`)
- Eat terminal emulator with eshell integration
- Clipboard support: macOS (pbcopy), Linux (xclip/xsel/wl-copy), Windows (clip.exe)

### Other
- Evil mode (opt-in per project/buffer, `C-c t v`)
- EPA symmetric encryption (`C-c s e` / `C-c s d`)
- Smooth pixel scrolling with momentum
- Custom minimal mode line
- Theme toggle with system-wide dark/light switching (EXWM)

## Key Bindings

### General

| Key | Action |
|-----|--------|
| `C-;` | Comment line |
| `C-a` | Smart beginning of line |
| `C-k` | Smart kill line |
| `C-j` | Delete indentation (join lines) |
| `C-o` | Occur |
| `C-z` | Undo |
| `C-\` | Hippie expand |
| `C-g` | Smart keyboard quit (deactivate region first) |
| `C-M-s` | Isearch symbol at point |
| `M-z` | Zap up to char |
| `C-=` / `C--` | Treesit expand/contract region |
| `C->` / `C-<` | Multiple cursors next/prev |
| `C-'` | Multiple cursors mark all |
| `C-c b` | Copy whole buffer |
| `C-c C-d` | Duplicate dwim |
| `C-c C-r` | Replace regexp |
| `C-c g` | VC transient menu |

### Files & Buffers

| Key | Action |
|-----|--------|
| `C-x f` | Project find file (fd) |
| `C-x g` | Deadgrep (project search) |
| `C-x C-r` | Recent files |
| `C-x b` | Ibuffer |
| `C-x C-b` | Switch to buffer |
| `C-x k` | Kill current buffer |
| `C-x M-k` | Kill buffer other window |
| `C-c C-p` / `C-c C-n` | Previous/next buffer |
| `C-c C-o` | Other window |
| `C-x w s` | Swap window states |
| `Backtab` | Format current buffer |

### Terminal (`C-c t` / `C-backtick`)

| Key | Action |
|-----|--------|
| `` C-` `` | Toggle eshell |
| `C-~` | New eat terminal |
| `C-c t e` | New eshell |
| `C-c t t` | New eat terminal |
| `C-c t s` | Shell |
| `C-c t d` | Dired jump other window |
| `C-c t =` | Toggle fido vertical mode |

### Claude / AI (`C-c l`)

| Key | Action |
|-----|--------|
| `C-c l l` | Open Claude chat |
| `C-c l s` | Send prompt at point |
| `C-c l r` | Rewrite region |
| `C-c l q` | Send captured entry to Claude |
| `C-c l a` | Add region to context |
| `C-c l f` | Add file to context |
| `C-c l k` | Code session (current buffer as context) |
| `C-c l p` | Add project to context |
| `C-c l m` | Model/params menu |
| `C-c l c` | Clear context |
| `C-c l b` | Browse Claude sessions |
| `C-c l /` | Search Claude sessions |
| `C-c l i` | Import Claude Desktop export |
| `C-c l S` | Sync Claude sessions |

### Eglot / LSP (`C-c c`)

| Key | Action |
|-----|--------|
| `C-.` | Code actions |
| `C-c c r` | Rename |
| `C-c c f` | Format buffer |
| `C-c c o` | Organize imports |
| `C-c c a` | All code actions |
| `C-c c q` | Quickfix |
| `C-c c e` | Extract |
| `C-c c j` | Inline |
| `C-c c k` | Rewrite |
| `C-c c i` | Find implementation |
| `C-c c d` | Find declaration |
| `C-c c t` | Find type definition |
| `C-c c h` | Eldoc |

### Flymake (`C-c f`)

| Key | Action |
|-----|--------|
| `M-n` / `M-p` | Next/prev diagnostic |
| `C-c f n` / `C-c f p` | Next/prev error (skip warnings) |
| `C-c f d` | Buffer diagnostics |
| `C-c f D` | Project diagnostics |
| `C-c f s` | Start flymake |
| `C-c f q` | Quickfix |

### Org Mode (`C-c o`)

| Key | Action |
|-----|--------|
| `C-c o a` | Org agenda |
| `C-c o c` | Org capture |
| `C-c o c a` | Capture → Ask Claude |
| `C-c o c m` | Capture → Meeting |
| `C-c o c t` | Capture → Todo |
| `C-c o c n` | Capture → Note |

### .NET (`C-c n`)

| Key | Action |
|-----|--------|
| `C-c n r` | Dotnet run |
| `C-c n b` | Dotnet build |
| `C-c n t` | Dotnet test |
| `C-c n !` | Dotnet command |

### Tab Bar (`C-c TAB`)

| Key | Action |
|-----|--------|
| `C-c TAB n` | New tab |
| `C-c TAB r` | Rename tab |
| `C-c TAB k` | Close tab |
| `C-c TAB l` | Switch to tab |
| `C-c TAB f` / `C-c TAB b` | Next/prev tab |

### Config & Toggles

| Key | Action |
|-----|--------|
| `C-c e v` | Visit init.el |
| `C-c e r` | Reload init.el |
| `C-c e d` | Doctor (check dependencies) |
| `C-c t h` | Toggle theme |
| `C-c t f` | Toggle fullscreen |
| `C-c t v` | Toggle evil mode |
| `C-c t V` | Toggle evil in buffer |
| `C-c s e` | Encrypt file |
| `C-c s d` | Decrypt file |
| `C-h C-r` | Restart Emacs |

## Themes

- [uwu-theme](https://github.com/kborling/uwu-theme) — dark, colorful
- [fleury-theme](https://github.com/kborling/fleury-theme.el) — dark, warm bronze/amber
- [acme-theme](https://github.com/ianyepan/acme-emacs-theme) — light

## EXWM (Linux X11 Only)

EXWM turns Emacs into a tiling window manager. It activates automatically when `DESKTOP_SESSION=exwm`.

### Setup on Guix

1. Add `emacs-exwm` to your system packages in `config.scm`
2. Select "Custom Session" in your display manager — this runs `~/.xsession`
3. Set `kdb-location-coordinates` in `personal.el` for redshift night light

### EXWM Key Bindings (Super key)

| Key | Action |
|-----|--------|
| `s-0` to `s-9` | Switch workspace |
| `s-!` to `s-)` | Move window to workspace |
| `s-h/j/k/l` | Focus left/down/up/right |
| `s-H/J/K/L` | Resize windows |
| `s-v` / `s-s` | Split right / below |
| `s-d` / `s-D` | Delete window / other windows |
| `s-q` | Close buffer |
| `s-f` / `s-F` | Fullscreen / floating toggle |
| `s-r` | Reset to line-mode |
| `s-i` | Toggle char/line mode |
| `s-SPC` | Application launcher |
| `s-RET` | Terminal |
| `s-c` | Browser |
| `s-e` | File manager |
| `s-t` | Toggle dark/light theme |
| `s-p` | Power menu |
| `s-x` | Lock screen |
| `s-S` | Screenshot |
| `s-TAB` | Switch EXWM buffer |
| `s-[` / `s-]` | Prev/next workspace |

Terminal emulators (kitty, alacritty, xterm) auto-enter char-mode so keys go directly to the terminal. Use `s-r` to return to line-mode.

### Simulation Keys (Line-mode)

In line-mode, Emacs keys are translated for X applications:

| Emacs | Application |
|-------|-------------|
| `C-b/f` | Left/Right |
| `C-p/n` | Up/Down |
| `C-a/e` | Home/End |
| `C-w` | Cut |
| `M-w` | Copy |
| `C-y` | Paste |
| `C-s` | Find |

## personal.el

Copy `personal.el.template` to `personal.el` and configure:

- `gptel-api-key` — Claude API key
- `kdb-sql-connections` — Azure SQL connection list
- `kdb-evil-project-list` — projects using vim bindings
- `kdb-location-coordinates` — lat:long for redshift
- `kdb-claude-export-directory` — Claude Desktop export path
