# Frame-to-Session Relationship Architecture

## Core Concept
Associate Emacs frames with specific claude-code sessions, enabling seamless context switching and persistent layouts across development workflows.

## Data Structures

### 1. Frame-Session Association
```elisp
;; Frame parameter for session binding
;; Stored in frame parameters: 'claude-code-session
(defvar claude-code-session-data
  '((session-id . "project-main")           ; Unique session identifier
    (buffer-name . "*claude:/path/to/proj*") ; Associated claude buffer
    (directory . "/path/to/project/")        ; Working directory
    (instance-name . "main")                 ; Instance name within directory
    (created-at . "2024-07-05T10:30:00")   ; Timestamp
    (last-active . "2024-07-05T15:45:00"))) ; Last interaction

;; Global registry for reverse lookup
(defvar claude-code-frame-registry (make-hash-table :test 'equal)
  "Maps session-id to frame for efficient reverse lookup.")
```

### 2. Layout Persistence
```elisp
;; Per-frame layout storage
(defvar claude-code-layout-storage (make-hash-table :test 'equal)
  "Stores window configurations per frame-session combination.")

;; Key format: "frame-name:session-id"
;; Value: window-configuration object
;; Example: "emacs-main:project-ai" → #<window-configuration>
```

## API Design

### Core Functions
```elisp
;; Primary association functions
(defun claude-code-bind-frame-to-session (frame session-buffer)
  "Associate FRAME with SESSION-BUFFER.")

(defun claude-code-get-frame-session (frame)
  "Get session data for FRAME.")

(defun claude-code-switch-frame-session (frame new-session)
  "Switch FRAME to NEW-SESSION, saving current layout.")

;; Layout management
(defun claude-code-save-frame-layout (frame)
  "Save current window configuration for FRAME.")

(defun claude-code-restore-frame-layout (frame)
  "Restore saved window configuration for FRAME.")

;; Session discovery
(defun claude-code-find-session-frame (session-id)
  "Find frame associated with SESSION-ID.")

(defun claude-code-list-frame-sessions ()
  "List all frame-session associations.")
```

### Extended Commands
```elisp
;; New interactive commands
(defun claude-code-bind-current-frame ()
  "Interactively bind current frame to a claude-code session.")

(defun claude-code-switch-to-session (session)
  "Switch current frame to SESSION, creating new frame if needed.")

(defun claude-code-new-frame-for-session (session)
  "Create new frame dedicated to SESSION.")

(defun claude-code-clone-frame-session ()
  "Create new frame with same session as current frame.")
```

## Integration Points

### 1. Hook Integration
```elisp
;; Frame lifecycle hooks
(add-hook 'after-make-frame-functions #'claude-code-setup-new-frame)
(add-hook 'delete-frame-functions #'claude-code-cleanup-frame-session)

;; Window configuration hooks
(add-hook 'window-configuration-change-hook #'claude-code-auto-save-layout)

;; Frame focus hooks
(add-hook 'focus-in-hook #'claude-code-activate-frame-session)
```

### 2. Claude-Code.el Extensions
```elisp
;; Extend existing commands with frame awareness
(advice-add 'claude-code :around #'claude-code-frame-aware-start)
(advice-add 'claude-code-switch-to-buffer :around #'claude-code-frame-aware-switch)

;; New transient menu sections
(transient-append-suffix 'claude-code-transient "Manage Claude"
  '("F" "Frame Binding" claude-code-frame-commands))
```

## Implementation Strategy

### Phase 1: Core Infrastructure
1. **Frame Parameter System**: Store session data in frame parameters
2. **Registry Management**: Maintain global session-to-frame mapping
3. **Basic Association**: Bind frames to existing claude sessions

### Phase 2: Layout Management
1. **Configuration Capture**: Save window configurations automatically
2. **Restoration Logic**: Restore layouts on session switches
3. **Persistence**: Optional save to disk for cross-session continuity

### Phase 3: User Interface
1. **Interactive Commands**: User-friendly session binding commands
2. **Visual Indicators**: Mode-line or header-line session display
3. **Transient Menu**: Extend existing claude-code menu system

### Phase 4: Advanced Features
1. **Automatic Association**: Smart session detection based on project context
2. **Session Templates**: Predefined layouts for common workflows
3. **Multi-Session Frames**: Support multiple claude sessions per frame

## Event Flow

### Frame Creation
1. **New Frame Created** → `after-make-frame-functions`
2. **Check Directory** → Look for existing claude sessions
3. **Auto-Associate** → Bind to default session if available
4. **Initialize Layout** → Set up default window configuration

### Session Switching
1. **User Command** → `claude-code-switch-to-session`
2. **Save Current Layout** → Store window configuration
3. **Update Frame Parameters** → Change session association
4. **Restore New Layout** → Apply session's saved configuration
5. **Activate Session** → Ensure claude buffer is active

### Frame Focus
1. **Frame Gains Focus** → `focus-in-hook`
2. **Read Session Data** → Get associated session from frame parameters
3. **Activate Session** → Ensure claude session is ready
4. **Update Registry** → Mark as last active frame for session

## Error Handling

### Graceful Degradation
- **Missing Claude Session**: Frame works normally without association
- **Invalid Session Data**: Clean up corrupted frame parameters
- **Layout Restoration Failure**: Fall back to default configuration
- **Registry Inconsistency**: Self-healing registry updates

### Recovery Mechanisms
- **Orphaned Sessions**: Re-associate with available frames
- **Corrupted Layouts**: Regenerate default configurations
- **Missing Dependencies**: Disable frame management gracefully

## Configuration Options

### User Customization
```elisp
(defcustom claude-code-auto-bind-frames t
  "Automatically bind new frames to claude sessions."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-save-layouts-automatically t
  "Automatically save window layouts on changes."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-frame-session-indicator t
  "Show session name in mode-line or header."
  :type 'boolean
  :group 'claude-code)
```