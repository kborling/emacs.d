# EXWM Configuration Guide

This branch contains a comprehensive EXWM (Emacs X Window Manager) configuration.

## Installation

### 1. Install Required Dependencies

```bash
# On Ubuntu/Debian/Linux Mint
sudo apt install rofi picom feh brightnessctl scrot nitrogen dunst

# Optional but recommended
sudo apt install i3lock network-manager network-manager-gnome blueman
```

### 2. Install EXWM Package

EXWM will be automatically installed when you first load Emacs with this configuration. The package is defined in `init.el` and will be fetched from MELPA.

### 3. Set Up EXWM as Your Window Manager

#### Option A: Using a Display Manager (Recommended)

1. Copy the desktop entry to your system:
```bash
sudo cp ~/.emacs.d/exwm.desktop /usr/share/xsessions/
```

2. Log out and select "EXWM" from your display manager's session menu

#### Option B: Using .xinitrc (For startx)

Add to your `~/.xinitrc`:
```bash
#!/bin/sh
exec emacs --eval "(progn (require 'init-exwm) (kdb-exwm-init))"
```

Then run:
```bash
chmod +x ~/.xinitrc
startx
```

## Key Bindings

EXWM uses the Super key (Windows key) as its primary modifier.

### Workspace Management

- `Super + 0-9` - Switch to workspace
- `Super + Shift + 0-9` - Move window to workspace
- `Super + [` - Previous workspace
- `Super + ]` - Next workspace
- `Super + w` - Switch workspace interactively

### Window Management

- `Super + h/j/k/l` - Move focus (left/down/up/right)
- `Super + H/J/K/L` - Resize windows
- `Super + v` - Split window vertically
- `Super + s` - Split window horizontally
- `Super + d` - Delete current window
- `Super + D` - Delete other windows
- `Super + q` - Close window
- `Super + f` - Toggle fullscreen
- `Super + F` - Toggle floating mode
- `Super + r` - Reset window to line-mode
- `Super + i` - Toggle between char-mode and line-mode
- `Super + TAB` - Switch between EXWM buffers

### Application Launchers

- `Super + Space` - Application launcher (rofi/dmenu)
- `Super + Return` - Launch terminal
- `Super + c` - Launch browser
- `Super + e` - Launch file manager
- `Super + &` - Run shell command

### System Controls

- `Super + p` - Power menu (Lock/Logout/Suspend/Reboot/Shutdown)
- `Super + x` - Lock screen
- `Super + S` - Take screenshot
- `Super + Ctrl + s` - Take screenshot of region

### Media Keys

- `XF86AudioRaiseVolume` - Increase volume
- `XF86AudioLowerVolume` - Decrease volume
- `XF86AudioMute` - Toggle mute
- `XF86MonBrightnessUp` - Increase brightness
- `XF86MonBrightnessDown` - Decrease brightness

### Special Keys

- `Ctrl + q` - Send next key directly to application (in char-mode)
- `Super + b` - Open ibuffer

## Configuration Details

### Workspace Configuration

The default setup includes 5 workspaces:
1. Main - General purpose
2. Web - Browsers
3. Code - Development
4. Chat - Communication apps
5. Media - Media players

### Window Rules

Certain applications are automatically assigned to specific workspaces:
- Firefox/Chrome → Workspace 1 (Web)
- Slack/Discord → Workspace 3 (Chat)
- Spotify → Workspace 4 (Media)

### Simulation Keys

When in char-mode (application has keyboard focus), these Emacs keys are translated to common application keys:

- `Ctrl + b/f` → Left/Right arrow
- `Ctrl + p/n` → Up/Down arrow
- `Ctrl + a/e` → Home/End
- `Ctrl + w` → Cut (Ctrl+x)
- `Meta + w` → Copy (Ctrl+c)
- `Ctrl + y` → Paste (Ctrl+v)
- `Ctrl + s` → Find (Ctrl+f)

## Autostart Programs

The following programs start automatically with EXWM (if installed):

- **Picom** - Compositor for transparency and effects
- **Feh** - Wallpaper setter (looks for ~/Pictures/wallpaper.jpg)
- **nm-applet** - Network Manager applet
- **blueman-applet** - Bluetooth manager
- **xscreensaver** - Screen saver
- **dunst** - Notification daemon

## Customization

### Change Wallpaper

Place your wallpaper at `~/Pictures/wallpaper.jpg` or edit the path in `init-exwm.el` in the `kdb-exwm-run-autostart` function.

### Modify Workspaces

Edit the `kdb-exwm-workspace-names` function in `init-exwm.el` to change workspace names and count.

### Add Custom Key Bindings

Add new bindings to the `exwm-input-global-keys` list in `init-exwm.el`:

```elisp
([?\\s-KEY] . function-name)
```

### Configure Application Rules

Add entries to `exwm-manage-configurations` to control where windows open and their behavior.

## Multi-Monitor Support

EXWM includes basic multi-monitor support via EXWM-RANDR. Edit the `kdb-exwm-update-displays` function to configure your monitor layout:

```elisp
(defun kdb-exwm-update-displays ()
  "Update display configuration."
  (interactive)
  (start-process-shell-command
   "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x0"))
```

## Troubleshooting

### EXWM doesn't start

1. Check that Emacs is version 27 or higher: `emacs --version`
2. Ensure X11 is available (EXWM doesn't work on Wayland)
3. Check Emacs messages buffer for errors: `*Messages*`

### Applications don't launch

1. Verify the application is installed: `which application-name`
2. Try launching from Emacs: `M-x shell-command RET application-name`

### Keyboard shortcuts don't work

1. Make sure you're using the Super key (Windows key)
2. Check if the key is in `exwm-input-prefix-keys` (these pass through to Emacs)
3. Use `Super + r` to reset the window if it's stuck

### Windows appear but can't be controlled

1. Press `Super + r` to reset the window
2. Try toggling between char-mode and line-mode with `Super + i`

## Switching Back to Linux Mint Cinnamon

If you want to switch back to your regular desktop environment:

1. Log out
2. At the login screen, select your previous session (Cinnamon)
3. Log back in

Your EXWM configuration remains on this branch and your main configuration is on the `main` branch.

## Advanced Features

### Buffer Management

EXWM buffers are regular Emacs buffers, so you can use all normal Emacs buffer commands. The `ibuffer` interface groups EXWM windows for easy management.

### Integration with Emacs

Since EXWM is built into Emacs, you have full access to:
- All your Emacs packages and configurations
- Org-mode for notes and task management
- Magit for git operations
- Your coding environments
- All without leaving your window manager

### Custom Modeline

A custom modeline segment `kdb-exwm-modeline-segment` is provided. Uncomment the code in `init-exwm.el` to display the current workspace in your modeline.

## Resources

- [EXWM Official Wiki](https://github.com/ch11ng/exwm/wiki)
- [EXWM Manual](https://github.com/ch11ng/exwm)
- [Arch Wiki EXWM](https://wiki.archlinux.org/title/EXWM)

## Quick Reference Card

```
Workspace:    Super + [0-9]       Application:  Super + Space
Move Window:  Super + Shift + [0-9]  Terminal:     Super + Return
Focus:        Super + h/j/k/l    Browser:      Super + c
Resize:       Super + H/J/K/L    Files:        Super + e
Split V:      Super + v
Split H:      Super + s          System:       Super + p
Close:        Super + q          Lock:         Super + x
Fullscreen:   Super + f          Screenshot:   Super + S
```

Enjoy your new EXWM setup!
