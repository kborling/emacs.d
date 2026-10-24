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
init.el                       Main config — everything loads from here
early-init.el                 Pre-frame performance and UI chrome
personal.el                   Private settings (API keys — not tracked)
personal.el.template          Template for personal.el
custom.el                     Customize output
lisp/
  init-claude-workflow.el     Claude transient over agent-shell: start, send, sessions
  init-claude.el              Claude session archive, history, search, Desktop import
  init-claude-print.el        Fallback Claude backend over pipes (M-x kdb-claude-print)
  init-org.el                 Org mode, capture templates, org-modern
  init-vc.el                  VC/Git transient menu (Magit-like, built-in VC)
  init-exwm.el                EXWM window manager (X11 only)
  init-terminal.el            Terminal/tmux optimizations
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

### Claude AI (`C-c l`)

Sessions run in [agent-shell](https://github.com/xenodium/agent-shell) over ACP, using the `claude` CLI login (works with a Max subscription, Linux and native Windows). Requires node and `npm install -g @agentclientprotocol/claude-agent-acp`.

- **Start** — new session at the project root, pre-filled with the region, Dired files or org entry at point; or pick any project (recent Claude directories first) and branch, which opens in its own git worktree
- **Send** — region, note, files or a file to a session you pick
- **Sessions** — [agent-shell-attention](https://github.com/ultronozm/agent-shell-attention.el) tracks sessions waiting on you; jump, dashboard, or switch grouped by project
- **History** — every past session (agent-shell, CLI, Desktop's Code tab, imported Desktop chats) in one list: filter, full-text search, ask Claude about them, resume

**New machine** (see [Claude setup checklist](#claude-setup-checklist)): install node and the Claude Code CLI, log in, then `C-c l !`.

Conversation data stays out of this repo and out of your projects: transcripts, agent-shell screenshots and prompt history all live in the archive (set `kdb-claude-archive-dir` in `personal.el` to move it).

Every session can read the archive (`~/.claude-archive`, via `permissions.additionalDirectories` and `~/.claude/CLAUDE.md`), so Claude can look up earlier work itself.

### Org Mode
- org-modern styling, org-appear
- Capture templates: todo, notes, journal, meetings, 1:1s, projects, decisions, specs, Ask Claude
- Refile targets across org files
- Deft for fast note search (`C-c o d`)

### Terminal
- Eshell with custom prompt and toggle (`` C-` ``)
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
| `C-j` | Join lines |
| `C-o` | Open line |
| `C-z` | Undo |
| `C-\` | Hippie expand |
| `C-g` | Smart quit (deactivate region first) |
| `C-M-s` | Isearch symbol at point |
| `M-z` | Zap up to char |
| `C-=` / `C--` | Treesit expand/contract |
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

### Terminal (`C-c x`)

| Key | Action |
|-----|--------|
| `` C-` `` | Toggle eshell |
| `C-~` | New eat terminal |
| `C-c x e` | New eshell |
| `C-c x t` | New eat terminal |
| `C-c x s` | Shell |
| `C-c x d` | Dired jump other window |

### Claude (`C-c l`)

`C-c l` opens the Claude transient. All keys below follow `C-c l`.

| | **Start** | | **Send** | | **Sessions** | | **History** |
|---|---|---|---|---|---|---|---|
| `n` | New here (`C-u` pick project) | `s` | Region/note/files → session | `'` | Next waiting | `h` | All sessions |
| `N` | New on project + branch | `f` | File → session | `d` | Dashboard | `/` | Search text |
| | | | | `l` | Switch (by project) | `A` | Ask past sessions |
| | | | | | | `S` | Sync & commit |
| | | | | | | `i` | Import Desktop export |

In the history list (`C-c l h`): `RET` open, `u` resume (imported chats continue in a new session that reads the old one), `/` filter, `s` search, `A` ask, `g` refresh.

Session shortcuts (no transient needed):

| Key | Action |
|-----|--------|
| `C-c ;` | Switch session, grouped by project |
| `C-c '` | Jump to the next session waiting on you |

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
| `C-c o d` | Deft (note search) |
| `C-c o M` | Markdown → Org |
| `C-c o P` | Pandoc convert |

Capture templates: `t` todo, `n` note, `j` journal, `w` work, `l` link, `c` artifact, `m` meeting, `1` 1:1, `p` project, `d` decision, `s` spec, `a` Ask Claude.

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
| `C-c t t` | Toggle theme |
| `C-c t f` | Toggle fullscreen |
| `C-c t v` | Toggle evil mode |
| `C-c t V` | Toggle evil in buffer |
| `C-c h n` / `C-c h p` | Next/prev TODO |
| `C-c h o` | TODO occur |
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

## Claude setup checklist

Do this once per machine (e.g. the Windows work machine).

1. Install [node](https://nodejs.org) and the [Claude Code CLI](https://claude.com/claude-code); run `claude` once and log in with the Max account.
2. Pull this config and restart Emacs (`agent-shell` and `agent-shell-attention` install themselves).
3. Create an empty **private** repo for this machine's archive (keep work conversations in a separate repo from personal ones), then run `C-c l !` (`M-x kdb-claude-setup`). Answer yes to the ACP adapter and emacs-skills. It:
   - creates the archive (`~/.claude-archive`, its own git repo, never inside this config), or clones an existing one if you give its URL
   - asks for the private remote; `C-c l S` then commits and pushes it in the background
   - sets `cleanupPeriodDays` to 36500 in Claude's `settings.json` so transcripts aren't deleted after 30 days
   - adds the archive to `permissions.additionalDirectories` and a note to `CLAUDE.md`, so sessions can look up past work
   - runs `npm install -g @agentclientprotocol/claude-agent-acp` and installs the emacs-skills plugin

   On Windows these live in `%USERPROFILE%\.claude`, not Emacs's AppData home.  Safe to re-run.
4. Check it works: `C-c l n` in a project should open a Claude shell.  If agent-shell misbehaves on Windows, `M-x kdb-claude-print` is the pipe-based fallback.
5. Optional — bring in claude.ai / Desktop chat history:
   1. claude.ai → Settings → Privacy → Export data; save the emailed `manifest-*.json` to Downloads.
   2. `C-c l i` on the manifest opens its 5 zip links in the browser (they need your login and expire in 24h); let them save next to the manifest.
   3. `C-c l i` on the same manifest again unpacks and imports them; they appear in `C-c l h` as project `desktop`.

   On Windows the zips are unpacked with the built-in `tar` (no `unzip` needed).

## personal.el

Copy `personal.el.template` to `personal.el` and configure:

- `kdb-sql-connections` — Azure SQL connection list
- `kdb-evil-project-list` — projects using vim bindings
- `kdb-location-coordinates` — lat:long for redshift
- `kdb-claude-export-directory` — Claude Desktop export path
