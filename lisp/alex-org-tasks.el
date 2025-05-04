;;; alex-org-tasks.el --- Task management for org-mode -*- lexical-binding: t -*-

;; Author: Alex
;; Created: May 2, 2025

;;; Commentary:
;; Streamlined task management with intuitive functions.
;; Makes it easy to create, edit, and organize tasks.

;;; Code:

(require 'org)
(require 'alex-org-core)

;;; Task management functions

(defun alex-org-create-task (description)
  "Create a new task with DESCRIPTION in the todo file."
  (interactive "sTask description: ")
  (find-file alex-org-todo-file)
  (goto-char (point-min))
  (if (re-search-forward "^\\* Tasks" nil t)
      (progn
        (end-of-line)
        (newline)
        (insert (format "** TODO %s" description))
        (save-buffer))
    (message "Could not find 'Tasks' heading in todo file")))

(defun alex-org-set-priority ()
  "Set the priority of the current task with visual preview."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (when (not (org-at-heading-p))
      (org-back-to-heading t))
    
    (let* ((current-priority (org-entry-get (point) "PRIORITY"))
           (current-char (if current-priority
                             (string-to-char current-priority)
                           org-default-priority))
           (choices '((?A . "High")
                      (?B . "Medium")
                      (?C . "Low")
                      (?D . "None")))
           (prompt (format "Priority [current: %s]: "
                           (cdr (assoc current-char choices))))
           (new-priority (completing-read prompt (mapcar #'cdr choices) nil t))
           (new-char (car (rassoc new-priority choices))))
      
      (if (eq new-char ?D)
          (org-priority ?\ ) ; Remove priority
        (org-priority new-char)))))

(defun alex-org-toggle-task-state ()
  "Cycle through task states for the current task."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (when (not (org-at-heading-p))
      (org-back-to-heading t))
    
    (let* ((state (org-get-todo-state))
           (next-state (cond
                        ((string= state "TODO") "IN-PROGRESS")
                        ((string= state "IN-PROGRESS") "DONE")
                        ((string= state "DONE") "TODO")
                        (t "TODO"))))
      (org-todo next-state))))

(defun alex-org-clone-task ()
  "Create a duplicate of the current task."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (when (not (org-at-heading-p))
      (org-back-to-heading t))
    
    ;; Copy the current subtree
    (org-copy-subtree)
    
    ;; Create a new entry at the same level
    (org-back-to-heading t)
    (end-of-subtree t)
    (newline)
    (org-yank)
    
    ;; Reset task state to TODO
    (org-back-to-heading t)
    (org-todo "TODO")
    
    ;; Remove any scheduled/deadline dates
    (org-schedule '(4)) ; With prefix to remove
    (org-deadline '(4)) ; With prefix to remove
    
    (message "Task cloned")))

(defun alex-org-set-deadline ()
  "Set a deadline for the current task with improved date selection."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (when (not (org-at-heading-p))
      (org-back-to-heading t))
    
    ;; Improved date selection
    (let* ((current (org-entry-get (point) "DEADLINE"))
           (default (if current
                        (format-time-string "%Y-%m-%d" 
                                            (org-time-string-to-time current))
                      (format-time-string "%Y-%m-%d")))
           (presets '("today" "tomorrow" "+1w" "+2w" "+1m"))
           (selection (completing-read 
                       (format "Deadline [default: %s]: " default)
                       presets nil nil nil nil default)))
      
      (cond
       ((string= selection "today")
        (org-deadline nil "."))
       ((string= selection "tomorrow")
        (org-deadline nil "+1d"))
       ((string-match "\\+\\([0-9]+\\)\\([wmd]\\)" selection)
        (org-deadline nil selection))
       (t
        (org-deadline nil selection))))))

(defun alex-org-set-scheduled ()
  "Schedule the current task with improved date selection."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  
  (save-excursion
    (when (not (org-at-heading-p))
      (org-back-to-heading t))
    
    ;; Improved date selection
    (let* ((current (org-entry-get (point) "SCHEDULED"))
           (default (if current
                        (format-time-string "%Y-%m-%d" 
                                            (org-time-string-to-time current))
                      (format-time-string "%Y-%m-%d")))
           (presets '("today" "tomorrow" "+1w" "+2w" "+1m"))
           (selection (completing-read 
                       (format "Schedule [default: %s]: " default)
                       presets nil nil nil nil default)))
      
      (cond
       ((string= selection "today")
        (org-schedule nil "."))
       ((string= selection "tomorrow")
        (org-schedule nil "+1d"))
       ((string-match "\\+\\([0-9]+\\)\\([wmd]\\)" selection)
        (org-schedule nil selection))
       (t
        (org-schedule nil selection))))))

;;; Agenda integration
(defun alex-org-agenda-toggle-state ()
  "When in agenda view, toggle the state of the current task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                    (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (goto-char pos)
    (call-interactively 'alex-org-toggle-task-state)
    (org-agenda-redo)))

(defun alex-org-agenda-set-priority (priority-char)
  "When in agenda view, set the priority of the current task to PRIORITY-CHAR."
  (interactive "cPriority ([A]high, [B]medium, [C]low, [SPC]none): ")
  (org-agenda-check-type t 'agenda 'todo)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                    (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (goto-char pos)
    (org-priority priority-char)
    (org-agenda-redo)))

(defun alex-org-agenda-increment-priority ()
  "When in agenda view, increment the priority of the current task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                    (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (goto-char pos)
    (org-priority-up)
    (org-agenda-redo)))

(defun alex-org-agenda-decrement-priority ()
  "When in agenda view, decrement the priority of the current task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                    (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (goto-char pos)
    (org-priority-down)
    (org-agenda-redo)))

(with-eval-after-load 'org-agenda
  (org-defkey org-agenda-mode-map "T" 'alex-org-agenda-toggle-state)
  (org-defkey org-agenda-mode-map "+" 'alex-org-agenda-increment-priority)
  (org-defkey org-agenda-mode-map "-" 'alex-org-agenda-decrement-priority))

;;; Keybindings
(global-set-key (kbd "C-c t n") 'alex-org-create-task)
(define-key org-mode-map (kbd "C-c t p") 'alex-org-set-priority)
(define-key org-mode-map (kbd "C-c t s") 'alex-org-toggle-task-state)
(define-key org-mode-map (kbd "C-c t c") 'alex-org-clone-task)
(define-key org-mode-map (kbd "C-c t d") 'alex-org-set-deadline)
(define-key org-mode-map (kbd "C-c t i") 'alex-org-set-scheduled)

(provide 'alex-org-tasks)
;;; alex-org-tasks.el ends here
