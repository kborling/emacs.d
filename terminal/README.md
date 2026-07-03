# Terminal Themes

Matching color schemes for terminal emulators. Keep your terminal and Emacs visually consistent.

## Kitty (Linux, macOS)

Copy or symlink to `~/.config/kitty/`:

```sh
ln -sf ~/.emacs.d/terminal/kitty/kitty.conf ~/.config/kitty/kitty.conf
ln -sf ~/.emacs.d/terminal/kitty/themes ~/.config/kitty/themes
```

### Keybindings

| Key | Action |
|-----|--------|
| `Ctrl+Shift+T` | New tab (same directory) |
| `Ctrl+Shift+W` | Close tab |
| `Ctrl+Shift+\` | Vertical split |
| `Ctrl+Shift+-` | Horizontal split |
| `Ctrl+Shift+H/J/K/L` | Focus left/down/up/right |
| `Ctrl+Shift+Z` | Toggle fullscreen pane (stack layout) |
| `Ctrl+Shift+R` | Resize pane |
| `Ctrl+Shift+1-5` | Jump to tab |
| `Ctrl+Shift+F` | Fuzzy search scrollback (fzf) |
| `Ctrl+Shift+F5` | Reload config |

macOS uses `Cmd` instead of `Ctrl+Shift` for common operations.

### Themes

- `themes/fleury.conf` — warm bronze/amber (default)
- `themes/dark.conf` — uwu dark
- `themes/light.conf` — acme light

Switch: edit `include themes/X.conf` in kitty.conf, then `Ctrl+Shift+F5`.

## Windows Terminal

Import themes into Windows Terminal settings:

1. Open Settings (Ctrl+,) → Open JSON file
2. Copy contents of `profile.json` → `schemes` into your `schemes` array
3. Copy the profiles you want into `profiles.list`
4. Set your default profile's `colorScheme` to `"Fleury"`, `"UwU"`, or `"Acme Light"`

Individual theme files (`fleury.json`, `uwu.json`, `acme-light.json`) are also provided for selective import.
