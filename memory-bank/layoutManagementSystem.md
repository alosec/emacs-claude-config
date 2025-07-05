# Layout Management System

## Overview
Intelligent window configuration persistence and restoration system that maintains development contexts across frame-session switches and emacs restarts.

## Core Concepts

### 1. Layout Hierarchy
```
Session Layout Management
├── Frame-Level Layouts (per frame-session combination)
├── Session Templates (default layouts for workflow types)
├── Project Layouts (per-project defaults)
└── Global Layouts (fallback configurations)
```

### 2. Layout Types
- **Development Layout**: Code + Claude side-by-side
- **Review Layout**: Multiple code buffers + Claude
- **Research Layout**: Org-mode + Claude + reference materials
- **Custom Layout**: User-defined configurations

## Data Structures

### Layout Storage
```elisp
;; Primary layout storage
(defvar claude-code-layouts (make-hash-table :test 'equal)
  "Hash table storing window configurations by key.")

;; Key format examples:
;; "frame:session" → "main-frame:project-ai"
;; "template:type" → "template:development"
;; "project:default" → "project:/path/to/proj"

;; Layout metadata
(defstruct claude-code-layout
  configuration    ; window-configuration object
  created-at      ; timestamp
  last-used       ; timestamp
  description     ; human-readable description
  layout-type     ; :development, :review, :research, :custom
  buffer-list     ; list of buffers in configuration
  claude-session  ; associated claude session
  project-root)   ; project directory

;; Session-specific layout history
(defvar claude-code-layout-history (make-hash-table :test 'equal)
  "History of layouts per session for undo/redo functionality.")
```

### Persistence Storage
```elisp
;; File-based persistence
(defcustom claude-code-layouts-file 
  (expand-file-name "claude-code-layouts.el" user-emacs-directory)
  "File to save layout configurations."
  :type 'file
  :group 'claude-code)

;; Serializable layout data
(defstruct claude-code-saved-layout
  frame-name      ; frame identifier
  session-id      ; claude session
  window-tree     ; serialized window structure
  buffer-files    ; file paths for restoration
  claude-buffer   ; claude session buffer name
  project-root    ; project directory
  timestamp)      ; save time
```

## Layout Operations

### Core Functions
```elisp
;; Layout capture and restoration
(defun claude-code-capture-layout (frame session-id &optional description)
  "Capture current window configuration for FRAME and SESSION-ID.")

(defun claude-code-restore-layout (frame session-id)
  "Restore saved layout for FRAME and SESSION-ID.")

(defun claude-code-save-layout-to-disk (layout-key &optional file)
  "Save layout to persistent storage.")

(defun claude-code-load-layouts-from-disk (&optional file)
  "Load layouts from persistent storage.")

;; Layout management
(defun claude-code-create-layout-template (name layout-type)
  "Create a reusable layout template.")

(defun claude-code-apply-layout-template (template-name frame)
  "Apply layout template to FRAME.")

(defun claude-code-list-layouts (&optional session-id)
  "List available layouts, optionally filtered by SESSION-ID.")

;; Smart layout operations
(defun claude-code-auto-layout-for-project (project-root)
  "Automatically select appropriate layout for PROJECT-ROOT.")

(defun claude-code-suggest-layout (session-id)
  "Suggest optimal layout based on session context.")
```

### Layout Templates

#### Development Template
```elisp
(defun claude-code-create-development-layout (frame claude-buffer)
  "Create side-by-side development layout."
  ;; Split frame: 60% code, 40% claude
  ;; Top: file buffer | claude buffer
  ;; Bottom: optional compilation/terminal
  )
```

#### Review Template  
```elisp
(defun claude-code-create-review-layout (frame claude-buffer)
  "Create code review layout."
  ;; Split frame: multiple code windows + claude
  ;; Layout for comparing files with AI assistance
  )
```

#### Research Template
```elisp
(defun claude-code-create-research-layout (frame claude-buffer)
  "Create research and documentation layout."
  ;; Org-mode + claude + reference materials
  ;; Optimized for note-taking and information gathering
  )
```

## Smart Layout Features

### 1. Context-Aware Restoration
```elisp
(defun claude-code-smart-restore (frame session-id)
  "Intelligently restore layout with context awareness."
  ;; 1. Check for exact layout match
  ;; 2. Fall back to project template
  ;; 3. Use session type template
  ;; 4. Apply global default
  )
```

### 2. Buffer Availability Handling
```elisp
(defun claude-code-handle-missing-buffers (layout)
  "Handle missing buffers during layout restoration."
  ;; 1. Try to reopen files from paths
  ;; 2. Substitute with similar buffers
  ;; 3. Create placeholder buffers
  ;; 4. Adjust layout for missing windows
  )
```

### 3. Dynamic Layout Adjustment
```elisp
(defun claude-code-adjust-layout-for-frame (layout frame)
  "Adjust saved layout to fit current frame size."
  ;; Scale window proportions to frame dimensions
  ;; Maintain relative sizes while adapting to different screens
  )
```

## Integration with Existing Systems

### Window Configuration Hooks
```elisp
;; Automatic layout saving
(add-hook 'window-configuration-change-hook 
          #'claude-code-maybe-save-layout)

;; Frame-specific hooks
(add-hook 'after-make-frame-functions 
          #'claude-code-apply-default-layout)

;; Session change hooks  
(add-hook 'claude-code-session-switch-hook
          #'claude-code-save-and-restore-layout)
```

### Claude-Code.el Integration
```elisp
;; Extend session switching to include layout management
(advice-add 'claude-code-switch-to-buffer :around 
            #'claude-code-layout-aware-switch)

;; Add layout commands to transient menu
(transient-append-suffix 'claude-code-transient "Manage Claude"
  '("l" "Layout Management" claude-code-layout-transient))
```

## User Interface

### Interactive Commands
```elisp
;; Primary layout commands
(defun claude-code-save-current-layout (&optional name)
  "Save current layout with optional NAME.")

(defun claude-code-load-layout (name)
  "Load saved layout by NAME.")

(defun claude-code-layout-manager ()
  "Open layout management interface.")

;; Quick layout switching
(defun claude-code-next-layout ()
  "Switch to next layout in history.")

(defun claude-code-previous-layout ()
  "Switch to previous layout in history.")

;; Template management
(defun claude-code-create-template-from-current ()
  "Create template from current layout.")

(defun claude-code-edit-layout-template (name)
  "Edit existing layout template.")
```

### Visual Indicators
```elisp
;; Mode-line layout indicator
(defun claude-code-layout-mode-line-indicator ()
  "Show current layout in mode-line.")

;; Header-line session info
(defun claude-code-session-header-line ()
  "Display session and layout info in header-line.")
```

## Persistence Strategy

### Session-Level Persistence
- Save layouts automatically on session switch
- Maintain layout history per session
- Quick undo/redo for layout changes

### Cross-Session Persistence
- Optional save to disk for important layouts
- Project-specific layout defaults
- Template library for reusable layouts

### Performance Considerations
- Lazy loading of saved layouts
- Cleanup of old/unused layouts
- Efficient window configuration serialization

## Configuration Options

### User Customization
```elisp
(defcustom claude-code-auto-save-layouts t
  "Automatically save layouts on configuration changes."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-layout-history-size 10
  "Number of layouts to keep in history per session."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-default-layout-type 'development
  "Default layout type for new sessions."
  :type '(choice (const :tag "Development" development)
                 (const :tag "Review" review)
                 (const :tag "Research" research)
                 (const :tag "Custom" custom))
  :group 'claude-code)

(defcustom claude-code-persist-layouts-on-exit t
  "Save important layouts to disk on Emacs exit."
  :type 'boolean
  :group 'claude-code)
```

## Error Handling and Recovery

### Graceful Degradation
- Continue without layout management if features fail
- Fall back to simple window splitting on restoration errors
- Maintain basic functionality when templates are missing

### Recovery Mechanisms
- Automatic cleanup of corrupted layout data
- Fallback templates when user templates fail
- Self-healing layout restoration with error logging