# Alex's Emacs Configuration

This repository contains my personal Emacs configuration files, organized for maintainability and version control.

## Structure

- `init.el` - Main configuration file
- `lisp/` - Custom elisp modules
  - `alex-org-core.el` - Core org-mode setup
  - `alex-org-projects.el` - Project management functionality
  - `alex-org-tasks.el` - Task handling and state management
  - `alex-org-agenda.el` - Custom agenda views
  - `alex-org-capture.el` - Capture templates and journal functionality

## Features

- Modular org-mode configuration for task and project management
- org-super-agenda with three specialized views:
  1. Project-Focused View (C-c a 1)
  2. Context-Based View (C-c a 2)
  3. Time-Oriented View (C-c a 3)
- Helm integration for efficient navigation
- Projectile for project management
- Custom keybindings for improved workflow

## Keyboard Shortcuts

### Org-mode and Agenda
- `C-c a 1` - Project-Focused agenda view
- `C-c a 2` - Context-Based agenda view
- `C-c a 3` - Time-Oriented agenda view
- `C-c a v` - Toggle between agenda views
- `C-c c` - Org capture
- `C-c p a` - Assign task to project

### Navigation
- `C-c n` - Search notes
- `C-c h` - Helm prefix
- `C-c p` - Projectile prefix

## Installation

To use this configuration on a new system:

1. Clone this repository to `~/.emacs.d`:
   ```
   git clone https://github.com/yourusername/emacs-config.git ~/.emacs.d
   ```
2. Start Emacs - the configuration will automatically install required packages

## Maintenance

To update packages:
```
M-x package-refresh-contents
M-x package-upgrade-all
```

To add the repository to Git:
```
cd ~/.emacs.d
git add .
git commit -m "Initial commit"
git remote add origin <your-repository-url>
git push -u origin master
```
