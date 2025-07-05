# Tech Context: Claude Code Emacs Integration

## Technology Stack

### Core Technologies
- **Emacs**: 30.0+ (primary platform)
- **Claude Code CLI**: AI assistant command-line interface
- **Emacs Lisp**: Configuration and extension language
- **Git**: Version control for configuration management

### Existing Dependencies
- **claude-code.el**: Core integration package (already configured)
- **transient**: Command menu system (>= 0.7.5)
- **eat**: Terminal emulator backend (primary)
- **vterm**: Alternative terminal backend (optional)
- **org-super-agenda**: Task management (already configured)
- **helm**: Navigation and completion (already configured)
- **projectile**: Project management (already configured)

### Development Environment
- **Platform**: Linux ARM64 (6.1.0-34-arm64)
- **Working Directory**: `/home/alex/.emacs.d`
- **Repository**: Git initialized with recent commits
- **Terminal**: EAT backend configured for claude-code.el

## File Structure
```
.emacs.d/
├── init.el                    # Main configuration
├── memory-bank/               # Project documentation
├── site-lisp/                 # External packages
│   └── claude-code/          # Claude integration
├── lisp/                     # Modular org configuration
│   ├── alex-org-core.el
│   ├── alex-org-projects.el
│   ├── alex-org-tasks.el
│   ├── alex-org-agenda.el
│   └── alex-org-capture.el
├── icons/                    # UI assets
└── [standard emacs dirs]     # elpa, auto-save-list, etc.
```

## Configuration Architecture

### Modular Design
- **init.el**: Main configuration entry point
- **lisp/alex-org-*.el**: Org-mode functionality modules
- **claude-code.el**: AI integration (site-lisp/claude-code/)
- **memory-bank/**: Project documentation and memory

### Key Configuration Sections
1. **Package Management**: use-package with MELPA
2. **UI Configuration**: Helm, projectile, themes
3. **Org Setup**: Three-view org-super-agenda system
4. **Claude Integration**: Already configured with EAT backend
5. **Development Tools**: Astro mode, LSP, flycheck

## Development Constraints
- **Emacs Version**: Must maintain 30.0+ compatibility
- **Backward Compatibility**: Don't break existing workflows
- **Performance**: Efficient frame/session tracking
- **Memory Usage**: Reasonable storage for layouts and associations
- **Integration**: Work with existing claude-code.el architecture

## Technical Requirements
- **Frame Management**: Emacs frame parameter manipulation
- **Session Tracking**: Hash table for efficient lookups
- **Layout Persistence**: Window configuration serialization
- **Hook Integration**: Leverage existing Emacs hook system
- **Error Handling**: Graceful degradation if claude-code unavailable

## Development Workflow
- **Testing**: Interactive development in running Emacs instance
- **Version Control**: Git tracking of all configuration changes
- **Documentation**: Memory bank maintenance for understanding
- **Modularity**: Keep frame management separate from core claude-code.el