;;; project-tab-bar.el --- Project tab bar with Claude Code integration -*- lexical-binding: t; -*-

;; Author: Alex
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0") (projectile "2.0"))
;; Keywords: tools, projects, ai

;;; Commentary:
;; A persistent project tab bar that displays active projects and provides
;; one-click access to deploy Claude Code sessions associated with each project.
;; Uses header-line-format for persistent display across all buffers.

;;; Code:

(require 'projectile)
(require 'cl-lib)

;;;; Customization

(defgroup project-tab-bar nil
  "Project tab bar with Claude Code integration."
  :group 'tools
  :prefix "project-tab-bar-")

(defcustom project-tab-bar-max-tabs 8
  "Maximum number of project tabs to display."
  :type 'integer
  :group 'project-tab-bar)

(defcustom project-tab-bar-tab-width 20
  "Maximum width of each project tab."
  :type 'integer
  :group 'project-tab-bar)

(defcustom project-tab-bar-min-tab-width 8
  "Minimum width of each project tab when space is constrained."
  :type 'integer
  :group 'project-tab-bar)

(defcustom project-tab-bar-enable-scrolling t
  "Enable horizontal scrolling when tabs overflow the frame width."
  :type 'boolean
  :group 'project-tab-bar)

(defcustom project-tab-bar-show-session-indicator t
  "Show Claude session indicator on project tabs."
  :type 'boolean
  :group 'project-tab-bar)

;;;; Faces

(defface project-tab-bar-active-face
  '((t :background "#4CAF50" :foreground "white" :weight bold))
  "Face for the active project tab."
  :group 'project-tab-bar)

(defface project-tab-bar-inactive-face
  '((t :background "#757575" :foreground "white"))
  "Face for inactive project tabs."
  :group 'project-tab-bar)

(defface project-tab-bar-session-indicator-face
  '((t :foreground "#FFC107" :weight bold))
  "Face for Claude session indicator."
  :group 'project-tab-bar)

(defface project-tab-bar-scroll-face
  '((t :background "#2196F3" :foreground "white" :weight bold))
  "Face for scroll indicators when hovered."
  :group 'project-tab-bar)

;;;; Internal Variables

(defvar project-tab-bar--current-project nil
  "Currently active project directory.")

(defvar project-tab-bar--project-list nil
  "List of recent/active projects for tab display.")

(defvar project-tab-bar--project-sessions (make-hash-table :test 'equal)
  "Hash table mapping project directories to Claude session data.")

(defvar project-tab-bar--scroll-offset 0
  "Current horizontal scroll offset for project tabs.")

(defvar project-tab-bar-mode nil
  "Non-nil if project tab bar mode is enabled.")

;;;; Core Functions

(defun project-tab-bar--get-project-name (project-root)
  "Get display name for PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun project-tab-bar--truncate-name (name max-width)
  "Truncate NAME to MAX-WIDTH characters with ellipsis if needed."
  (if (<= (length name) max-width)
      name
    (concat (substring name 0 (- max-width 3)) "...")))

(defun project-tab-bar--calculate-available-width ()
  "Calculate available width for project tabs in current frame."
  (let* ((frame-width (frame-width))
         (prefix-width (length " Projects: "))
         (visible-count (min (length project-tab-bar--project-list)
                            (/ (- frame-width prefix-width 4) project-tab-bar-tab-width))) ; Reserve space for scroll indicators
         (separator-width (* (1- visible-count) 1))  ; For each "│" separator between visible tabs
         (scroll-indicator-width (if (and project-tab-bar-enable-scrolling 
                                         (> (length project-tab-bar--project-list) visible-count))
                                    2  ; Space for ◀ ▶ 
                                  0)))
    (- frame-width prefix-width separator-width scroll-indicator-width)))


(defun project-tab-bar--has-claude-session-p (project-root)
  "Check if PROJECT-ROOT has an associated Claude session."
  (when (featurep 'claude-code)
    (let ((claude-buffers (claude-code--find-claude-buffers-for-directory project-root)))
      (length claude-buffers))))

(defun project-tab-bar--update-project-list ()
  "Update the list of projects for tab display."
  (let ((known-projects (projectile-relevant-known-projects))
        (current-project (or project-tab-bar--current-project  ; Respect manually set project
                            (projectile-project-root))))       ; Fall back to projectile detection
    
    ;; Add current project to front if not already there
    (when current-project
      (setq known-projects (cl-remove current-project known-projects :test #'string=))
      (push current-project known-projects))
    
    ;; Limit to max tabs
    (setq project-tab-bar--project-list 
          (cl-subseq known-projects 0 (min (length known-projects) 
                                          project-tab-bar-max-tabs)))
    
    ;; Only update current project if we don't have a manual selection
    (unless project-tab-bar--current-project
      (setq project-tab-bar--current-project current-project))))

(defun project-tab-bar--create-tab (project-root)
  "Create a tab string for PROJECT-ROOT."
  (let* ((project-name (project-tab-bar--get-project-name project-root))
         (truncated-name (project-tab-bar--truncate-name project-name 
                                                        (- project-tab-bar-tab-width 4))) ; Account for indicators and spacing
         (is-active (string= project-root project-tab-bar--current-project))
         (has-session (project-tab-bar--has-claude-session-p project-root))
         (face (if is-active 'project-tab-bar-active-face 'project-tab-bar-inactive-face))
         (session-indicator (if (and has-session project-tab-bar-show-session-indicator)
                               (cond
                                ((> has-session 1)
                                 (propertize (format "%d●" has-session) 
                                           'face 'project-tab-bar-session-indicator-face))
                                (t
                                 (propertize "●" 'face 'project-tab-bar-session-indicator-face)))
                             " "))
         (tab-text (format " %s%s " session-indicator truncated-name)))
    
    (propertize tab-text
                'face face
                'mouse-face 'highlight
                'help-echo (format "Switch to project: %s%s" 
                                  project-name
                                  (cond
                                   ((> has-session 1) (format " (%d Claude sessions)" has-session))
                                   (has-session " (has Claude session)")
                                   (t "")))
                'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [header-line mouse-1] 
                              `(lambda () (interactive) 
                                 (project-tab-bar-switch-to-project ,project-root)))
                            map)
                'project-root project-root)))

(defun project-tab-bar--generate-header-line ()
  "Generate the header line with project tabs."
  (project-tab-bar--update-project-list)
  (if project-tab-bar--project-list
      (let* ((visible-count (project-tab-bar--calculate-visible-tab-count))
             (visible-projects (if project-tab-bar-enable-scrolling
                                 (cl-subseq project-tab-bar--project-list 
                                           project-tab-bar--scroll-offset
                                           (min (length project-tab-bar--project-list)
                                                (+ project-tab-bar--scroll-offset visible-count)))
                               project-tab-bar--project-list))
             (tabs (mapcar #'project-tab-bar--create-tab visible-projects))
             (prefix (propertize " Projects: " 'face 'bold))
             (scroll-left-indicator (if (and project-tab-bar-enable-scrolling 
                                           (> project-tab-bar--scroll-offset 0))
                                      (propertize "◀" 'face 'bold 'help-echo "Scroll left"
                                                 'mouse-face 'project-tab-bar-scroll-face
                                                 'local-map (let ((map (make-sparse-keymap)))
                                                              (define-key map [header-line mouse-1] 
                                                                #'project-tab-bar-scroll-left)
                                                              map))
                                    ""))
             (scroll-right-indicator (if (and project-tab-bar-enable-scrolling
                                            (< (+ project-tab-bar--scroll-offset visible-count)
                                               (length project-tab-bar--project-list)))
                                       (propertize "▶" 'face 'bold 'help-echo "Scroll right"
                                                  'mouse-face 'project-tab-bar-scroll-face
                                                  'local-map (let ((map (make-sparse-keymap)))
                                                               (define-key map [header-line mouse-1] 
                                                                 #'project-tab-bar-scroll-right)
                                                               map))
                                     ""))
             (tab-string (mapconcat #'identity tabs "│"))
             (tab-area (propertize tab-string 
                                  'local-map (let ((map (make-sparse-keymap)))
                                               (define-key map [header-line wheel-up] 
                                                 #'project-tab-bar-scroll-left)
                                               (define-key map [header-line wheel-down] 
                                                 #'project-tab-bar-scroll-right)
                                               map))))
        (concat prefix scroll-left-indicator tab-area scroll-right-indicator))
    (propertize " No projects " 'face 'italic)))

;;;; Public API

(defun project-tab-bar-switch-to-project (project-root)
  "Switch to PROJECT-ROOT and activate associated Claude sessions."
  (interactive)
  (when (file-directory-p project-root)
    ;; Switch to project directory directly (avoid triggering helm-projectile)
    (setq default-directory project-root)
    
    ;; Ensure projectile knows about this project without triggering switch action
    (when (featurep 'projectile)
      (projectile-add-known-project project-root))
    
    ;; Update current project
    (setq project-tab-bar--current-project project-root)
    
    ;; Deploy Claude sessions if available
    (project-tab-bar--deploy-claude-sessions project-root)
    
    ;; Refresh tab bar
    (project-tab-bar--refresh)
    
    (message "Switched to project: %s" (project-tab-bar--get-project-name project-root))))

(defun project-tab-bar-associate-claude-session (project-root session-data)
  "Associate PROJECT-ROOT with Claude SESSION-DATA."
  (puthash project-root session-data project-tab-bar--project-sessions)
  (project-tab-bar--refresh))

(defun project-tab-bar-remove-claude-session (project-root)
  "Remove Claude session association for PROJECT-ROOT."
  (remhash project-root project-tab-bar--project-sessions)
  (project-tab-bar--refresh))

(defun project-tab-bar--deploy-claude-sessions (project-root)
  "Deploy Claude Code sessions for PROJECT-ROOT."
  (when (featurep 'claude-code)
    (let ((claude-buffers (claude-code--find-claude-buffers-for-directory project-root)))
      (cond
       ;; Multiple sessions - switch to default session
       ((> (length claude-buffers) 1)
        (message "Found %d Claude sessions for %s" 
                 (length claude-buffers)
                 (project-tab-bar--get-project-name project-root))
        ;; Clear windows and switch to the first (default) session
        (let ((default-session (car claude-buffers)))
          (delete-other-windows)
          (switch-to-buffer default-session)
          (message "Showing default session. Use C-c c B to switch between %d sessions"
                   (length claude-buffers))))
       
       ;; Single session - display it and switch to it
       ((= (length claude-buffers) 1)
        (let ((session-buffer (car claude-buffers)))
          ;; Clear current window configuration and switch to Claude session
          (delete-other-windows)
          (switch-to-buffer session-buffer)
          (message "Activated Claude session for %s" 
                   (project-tab-bar--get-project-name project-root))))
       
       ;; No existing sessions - offer to create one
       (t
        (let ((use-dialog-box nil))  ; Force minibuffer instead of GUI dialog
          (when (yes-or-no-p (format "No Claude sessions found for %s. Start new session? "
                                     (project-tab-bar--get-project-name project-root)))
            (let ((default-directory project-root))
              ;; Start new claude session in this project
              (claude-code)
              (message "Started new Claude session for %s" 
                       (project-tab-bar--get-project-name project-root)))))))
      
      ;; Always refresh tab bar to update session indicators
      (project-tab-bar--refresh))))

(defun project-tab-bar--refresh ()
  "Refresh the project tab bar display."
  (when project-tab-bar-mode
    (force-mode-line-update t)))

;;;; Mode Definition

(defun project-tab-bar--setup ()
  "Set up project tab bar."
  (setq-default header-line-format '(:eval (project-tab-bar--generate-header-line)))
  (project-tab-bar--update-project-list)
  
  ;; Hook into projectile for automatic updates
  (add-hook 'projectile-after-switch-project-hook #'project-tab-bar--refresh)
  (add-hook 'projectile-find-file-hook #'project-tab-bar--refresh)
  
  ;; Hook into claude-code for session tracking
  (when (featurep 'claude-code)
    (add-hook 'claude-code-start-hook #'project-tab-bar--refresh)))

(defun project-tab-bar--teardown ()
  "Tear down project tab bar."
  (setq-default header-line-format nil)
  
  ;; Remove hooks
  (remove-hook 'projectile-after-switch-project-hook #'project-tab-bar--refresh)
  (remove-hook 'projectile-find-file-hook #'project-tab-bar--refresh)
  (when (featurep 'claude-code)
    (remove-hook 'claude-code-start-hook #'project-tab-bar--refresh)))

;;;###autoload
(define-minor-mode project-tab-bar-mode
  "Toggle project tab bar mode.
When enabled, displays a persistent tab bar with project tabs that can
be clicked to switch projects and deploy associated Claude Code sessions."
  :init-value nil
  :global t
  :group 'project-tab-bar
  :lighter " ProjTabs"
  (if project-tab-bar-mode
      (project-tab-bar--setup)
    (project-tab-bar--teardown)))

;;;; Interactive Commands

;;;###autoload
(defun project-tab-bar-toggle ()
  "Toggle project tab bar on/off."
  (interactive)
  (project-tab-bar-mode (if project-tab-bar-mode -1 1)))

;;;###autoload
(defun project-tab-bar-refresh ()
  "Manually refresh project tab bar."
  (interactive)
  (project-tab-bar--refresh))

;;;###autoload
(defun project-tab-bar-scroll-left ()
  "Scroll project tabs to the left by one tab."
  (interactive)
  (when (and project-tab-bar-enable-scrolling (> project-tab-bar--scroll-offset 0))
    (setq project-tab-bar--scroll-offset (max 0 (1- project-tab-bar--scroll-offset)))
    (project-tab-bar--refresh)))

;;;###autoload
(defun project-tab-bar-scroll-right ()
  "Scroll project tabs to the right by one tab."
  (interactive)
  (when (and project-tab-bar-enable-scrolling project-tab-bar--project-list)
    (let* ((visible-count (project-tab-bar--calculate-visible-tab-count))
           (total-count (length project-tab-bar--project-list))
           (max-offset (max 0 (- total-count visible-count))))
      (when (< project-tab-bar--scroll-offset max-offset)
        (setq project-tab-bar--scroll-offset (min max-offset (1+ project-tab-bar--scroll-offset)))
        (project-tab-bar--refresh)))))

(defun project-tab-bar--calculate-visible-tab-count ()
  "Calculate how many tabs can fit in the current frame width."
  (let* ((frame-width (frame-width))
         (prefix-width (length " Projects: "))
         (scroll-space 4) ; Reserve space for potential ◀ ▶ indicators
         (usable-width (- frame-width prefix-width scroll-space))
         (tab-and-separator-width (1+ project-tab-bar-tab-width))) ; tab + separator
    (max 1 (/ usable-width tab-and-separator-width))))

;;;###autoload
(defun project-tab-bar-next-project ()
  "Switch to next project in tab bar."
  (interactive)
  (when project-tab-bar--project-list
    (let* ((current-idx (cl-position project-tab-bar--current-project 
                                    project-tab-bar--project-list 
                                    :test #'string=))
           (next-idx (if current-idx
                        (mod (1+ current-idx) (length project-tab-bar--project-list))
                      0))
           (next-project (nth next-idx project-tab-bar--project-list)))
      (when next-project
        (project-tab-bar-switch-to-project next-project)))))

;;;###autoload
(defun project-tab-bar-previous-project ()
  "Switch to previous project in tab bar."
  (interactive)
  (when project-tab-bar--project-list
    (let* ((current-idx (cl-position project-tab-bar--current-project 
                                    project-tab-bar--project-list 
                                    :test #'string=))
           (prev-idx (if current-idx
                        (mod (1- current-idx) (length project-tab-bar--project-list))
                      (1- (length project-tab-bar--project-list))))
           (prev-project (nth prev-idx project-tab-bar--project-list)))
      (when prev-project
        (project-tab-bar-switch-to-project prev-project)))))

;;;; Key bindings

(defvar project-tab-bar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t n") #'project-tab-bar-next-project)
    (define-key map (kbd "C-c t p") #'project-tab-bar-previous-project)
    (define-key map (kbd "C-c t r") #'project-tab-bar-refresh)
    (define-key map (kbd "C-c t t") #'project-tab-bar-toggle)
    (define-key map (kbd "C-c t <") #'project-tab-bar-scroll-left)
    (define-key map (kbd "C-c t >") #'project-tab-bar-scroll-right)
    map)
  "Keymap for project tab bar mode.")

(provide 'project-tab-bar)

;;; project-tab-bar.el ends here