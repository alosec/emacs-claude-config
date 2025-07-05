# System Patterns: Claude Code Emacs Integration

## Architecture Overview
The system builds on existing claude-code.el session management to create frame-to-session relationships and persistent layouts.

## Key Technical Decisions

### 1. Frame-to-Session Relationship Model
```
Frame → Session Mapping
├── Frame Parameters (frame-specific data)
├── Session Identifier (claude buffer + instance)
├── Layout Configuration (window splits, sizes)
└── Context Metadata (project, working directory)
```

### 2. Session Management Integration
**Existing (claude-code.el)**:
- `claude-code--directory-buffer-map`: Maps directories to buffers
- Buffer naming: `*claude:/path/to/project/:instance*`
- Instance selection and switching

**New Extension**:
- Frame parameter storage for session association
- Layout persistence per frame/session combination
- Context restoration on frame focus

### 3. Layout Persistence Pattern
```elisp
;; Frame-specific layout storage
(defvar claude-code-frame-layouts (make-hash-table :test 'equal))

;; Key structure: "frame-name:session-id" → window-configuration
;; Example: "main-dev:project-ai" → #<window-configuration>
```

### 4. Integration Points
- **Frame Creation**: Hook into `after-make-frame-functions`
- **Session Binding**: Extend claude-code commands to support frame association
- **Layout Saving**: Hook into window configuration changes
- **Context Restoration**: Integrate with existing buffer selection logic

## Design Patterns

### 1. Observer Pattern
- Frame focus events trigger session activation
- Window configuration changes trigger layout saves
- Session state changes notify frame updates

### 2. State Management
- Frame parameters store session associations
- Hash tables provide efficient layout lookup
- Persistent storage for cross-session continuity

### 3. Command Extension
- Extend existing claude-code commands with frame awareness
- New commands for explicit frame-session binding
- Integration with existing transient menu system

## Data Flow
1. **Frame Creation** → Check for existing session associations
2. **Session Selection** → Update frame parameters, save layout
3. **Layout Changes** → Persist configuration for current frame-session
4. **Frame Focus** → Restore associated session and layout
5. **Session Switching** → Update frame association, restore new layout

## Integration Strategy
- **Minimal Disruption**: Build on existing claude-code.el without major changes
- **Backward Compatible**: Existing workflows continue to work
- **Opt-in Enhancement**: New features available but not required
- **Modular Design**: Frame management can be enabled/disabled independently