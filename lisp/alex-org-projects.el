;;; alex-org-projects.el --- Project management for org-mode -*- lexical-binding: t -*-

;; Author: Alex
;; Created: May 2, 2025

;;; Commentary:
;; Simplified project management functionality.
;; Makes it easy to create, organize, and track projects.

;;; Code:

(require 'org)
(require 'alex-org-core)

;;; Project management functions

(defun alex-org-create-project (name)
  "Create a new project with NAME in the projects file."
  (interactive "sProject name: ")
  (find-file alex-org-projects-file)
  (goto-char (point-min))
  (if (re-search-forward "^\\* Active Projects" nil t)
      (progn
        (end-of-line)
        (newline 2)
        (insert (format "** %s\n" name))
        (backward-char)
        (org-set-property "PROJECT" "t")
        (org-set-property "CATEGORY" name))
    (message "Could not find 'Active Projects' heading in projects file"))
  (save-buffer))

(defun alex-org-get-all-projects ()
  "Return a list of all projects and their locations."
  (let ((projects nil))
    (with-current-buffer (find-file-noselect alex-org-projects-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\* \\(.+\\)" nil t)
          (let ((project-name (match-string-no-properties 1))
                (project-point (point-marker)))
            (save-excursion
              (org-back-to-heading t)
              (when (org-entry-get (point) "PROJECT")
                (push (cons project-name project-point) projects)))))))
    (nreverse projects)))

(defun alex-org-select-project ()
  "Prompt to select a project and return its marker position."
  (let* ((projects (alex-org-get-all-projects))
         (names (mapcar #'car projects))
         (selected (completing-read "Select project: " names nil t)))
    (cdr (assoc selected projects))))

(defun alex-org-toggle-project-status ()
  "Toggle the current project between Active, Planned, and Completed states."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (when (not (org-at-heading-p))
      (org-back-to-heading t))
    
    ;; Check if this is a project heading (level 2 with PROJECT property)
    (let ((level (org-current-level)))
      (if (or (not (= level 2))
              (not (org-entry-get (point) "PROJECT")))
          (error "Not at a project heading")
        
        ;; Determine current section and decide where to move
        (let* ((current-heading (org-get-heading t t t t))
               (current-section (save-excursion
                                  (while (org-up-heading-safe))
                                  (org-get-heading t t t t)))
               (target-section (cond
                                ((string= current-section "Active Projects") "Planned Projects")
                                ((string= current-section "Planned Projects") "Completed Projects")
                                ((string= current-section "Completed Projects") "Active Projects")
                                (t "Active Projects"))))
          
          ;; Find target section
          (goto-char (point-min))
          (if (re-search-forward (format "^\\* %s" target-section) nil t)
              (progn
                ;; Cut the current subtree
                (org-back-to-heading t)
                (org-cut-subtree)
                
                ;; Go to target section and paste
                (goto-char (point-min))
                (re-search-forward (format "^\\* %s" target-section) nil t)
                (end-of-line)
                (newline)
                (org-yank)
                
                ;; Set status property based on section
                (org-back-to-heading t)
                (org-set-property "STATUS" 
                                   (cond
                                    ((string= target-section "Active Projects") "ACTIVE")
                                    ((string= target-section "Planned Projects") "PLANNED")
                                    ((string= target-section "Completed Projects") "COMPLETED")
                                    (t "ACTIVE")))
                
                (message "Project moved to %s" target-section))
            (message "Could not find target section: %s" target-section)))))))

(defun alex-org-rename-project ()
  "Rename the current project and update its properties."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    
    (let ((level (org-current-level)))
      (if (or (not (= level 2))
              (not (org-entry-get (point) "PROJECT")))
          (error "Not at a project heading")
        
        (let* ((current-name (org-get-heading t t t t))
               (new-name (read-string "New project name: " current-name)))
          ;; Rename the heading
          (org-edit-headline new-name)
          ;; Update the category property
          (org-set-property "CATEGORY" new-name)
          (message "Project renamed to '%s'" new-name))))))

(defun alex-org-assign-task-to-project ()
  "Assign the current task to a project with proper hierarchy maintenance."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (when (not (org-at-heading-p))
      (org-back-to-heading t))
    
    ;; Make sure we're at a task heading (not a project)
    (when (org-entry-get (point) "PROJECT")
      (error "This is a project, not a task"))
    
    ;; Store current task
    (org-cut-subtree)
    
    ;; Select and go to project
    (let* ((project-marker (alex-org-select-project))
           (project-name ""))
      (switch-to-buffer (marker-buffer project-marker))
      (goto-char project-marker)
      
      ;; Get project name
      (save-excursion
        (org-back-to-heading t)
        (setq project-name (org-get-heading t t t t)))
      
      ;; Check if we need a Tasks subheading
      (let ((has-tasks-heading nil)
            (project-level (org-current-level)))
        
        ;; Look for a Tasks subheading at level+1
        (save-excursion
          (org-back-to-heading t)
          (let ((end (save-excursion (org-end-of-subtree t))))
            (while (and (not has-tasks-heading)
                        (outline-next-heading)
                        (< (point) end))
              (when (and (= (org-current-level) (1+ project-level))
                         (string= (org-get-heading t t t t) "Tasks"))
                (setq has-tasks-heading t)))))
        
        ;; Create Tasks heading if needed with proper level stars
        (if (not has-tasks-heading)
            (progn
              (org-back-to-heading t)
              (org-end-of-meta-data t) ;; Skip properties if any
              (newline)
              (insert (make-string (1+ project-level) ?*))
              (insert " Tasks")
              (newline))
          ;; Find the existing Tasks heading
          (org-back-to-heading t)
          (let ((end (save-excursion (org-end-of-subtree t))))
            (while (and (outline-next-heading)
                        (< (point) end))
              (when (and (= (org-current-level) (1+ project-level))
                         (string= (org-get-heading t t t t) "Tasks"))
                (org-end-of-subtree t)
                (newline)
                (org-yank)
                (setq has-tasks-heading t)
                (return)))))
        
        (message "Task assigned to project '%s'" project-name)))))

;;; Agenda integration
(defun alex-org-agenda-assign-to-project ()
  "When in agenda view, assign current task to a project."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                    (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    ;; We temporarily visit the task buffer to assign it
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (call-interactively 'alex-org-assign-task-to-project)))
    ;; Refresh the agenda to show changes
    (org-agenda-redo)))

(with-eval-after-load 'org-agenda
  (org-defkey org-agenda-mode-map "P" 'alex-org-agenda-assign-to-project))

;;; Keybindings
(global-set-key (kbd "C-c p n") 'alex-org-create-project)
(define-key org-mode-map (kbd "C-c p s") 'alex-org-toggle-project-status)
(define-key org-mode-map (kbd "C-c p a") 'alex-org-assign-task-to-project)
(define-key org-mode-map (kbd "C-c p r") 'alex-org-rename-project)

(defun alex-org-verify-project-structure ()
  "Verify the structure of the current project.
Returns information about any issues found with project tasks."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (org-back-to-heading t)
    
    ;; Check if this is a project heading
    (if (not (org-entry-get (point) "PROJECT"))
        (message "Not at a project heading")
      
      (let ((project-name (org-get-heading t t t t))
            (project-level (org-current-level))
            (has-tasks-heading nil)
            (tasks-heading-level nil)
            (issues '()))
        
        ;; Check for Tasks subheading
        (save-excursion
          (let ((end (save-excursion (org-end-of-subtree t))))
            (while (and (outline-next-heading)
                        (< (point) end))
              (when (and (= (org-current-level) (1+ project-level))
                         (string= (org-get-heading t t t t) "Tasks"))
                (setq has-tasks-heading t)
                (setq tasks-heading-level (org-current-level))))))
        
        ;; Report findings
        (unless has-tasks-heading
          (push "Missing Tasks subheading" issues))
        
        (when (and has-tasks-heading
                   (not (= tasks-heading-level (1+ project-level))))
          (push (format "Tasks heading at wrong level (%d), should be %d"
                        tasks-heading-level (1+ project-level))
                issues))
        
        ;; Check if any tasks exist under the Tasks heading
        (when has-tasks-heading
          (save-excursion
            (let ((found-tasks nil))
              (org-back-to-heading t)
              (let ((end (save-excursion (org-end-of-subtree t))))
                (while (and (outline-next-heading)
                            (< (point) end))
                  (when (and (= (org-current-level) (1+ project-level))
                             (string= (org-get-heading t t t t) "Tasks"))
                    (let ((tasks-end (save-excursion (org-end-of-subtree t))))
                      (while (and (outline-next-heading)
                                  (< (point) tasks-end))
                        (setq found-tasks t))))))
              (unless found-tasks
                (push "No tasks found under Tasks heading" issues)))))
        
        ;; Report results
        (if issues
            (message "Project structure issues in '%s': %s" 
                     project-name 
                     (mapconcat 'identity issues "; "))
          (message "Project '%s' structure verified OK" project-name))))))

(provide 'alex-org-projects)
;;; alex-org-projects.el ends here
