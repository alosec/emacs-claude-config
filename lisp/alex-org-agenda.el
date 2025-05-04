;;; alex-org-agenda.el --- Enhanced agenda views -*- lexical-binding: t -*-

;; Author: Alex
;; Created: May 2, 2025

;;; Commentary:
;; Enhanced agenda views that make it easier to visualize and manage your work.
;; Provides optimized views for different contexts and workflows.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'alex-org-core)

;;; Enhanced agenda views

;; Day view with clear section separation
(defun alex-org-agenda-day ()
  "Show an optimized day view of the agenda."
  (interactive)
  (org-agenda nil "d"))

;; Project-focused view - completely rewritten to be more robust
(defun alex-org-agenda-projects ()
  "Show a project-focused view of tasks."
  (interactive)
  (let ((org-agenda-custom-commands
         '(("z" "Projects"
            ((tags "PROJECT=\"t\""
                   ((org-agenda-overriding-header "ACTIVE PROJECTS\n")))
             (todo "TODO"
                   ((org-agenda-overriding-header "TASKS BY PROJECT\n")
                    (org-agenda-prefix-format '((todo . " %i %-12:c")))
                    (org-agenda-sorting-strategy '(category-keep priority-down)))))))))
    (org-agenda nil "z")))

;; Priority view
(defun alex-org-agenda-priority ()
  "Show tasks organized by priority."
  (interactive)
  (org-agenda nil "h"))

;; Next actions view
(defun alex-org-agenda-next ()
  "Show only next actions across all projects."
  (interactive)
  (org-agenda nil "n"))

;; Helper function to show project info in agenda
(defun alex-org-agenda-project-prefix ()
  "Return project name for a task in the agenda view."
  (let* ((marker (or (org-get-at-bol 'org-marker)
                    (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (let ((project-name "No Project"))
          (save-excursion
            (while (org-up-heading-safe)
              (when (org-entry-get (point) "PROJECT")
                (setq project-name (org-get-heading t t t t))
                (return))))
          project-name)))))

;;; Custom agenda commands configuration
(setq org-agenda-custom-commands
      '(("d" "Day view"
         ((agenda "" ((org-agenda-span 'day)
                    (org-agenda-start-day nil)
                    (org-agenda-start-on-weekday nil)
                    (org-agenda-todo-keyword-format "%-12s")
                    (org-agenda-overriding-header "TODAY'S SCHEDULE\n")
                    (org-agenda-current-time-string "⏰ NOW ───────────────────")))
          (todo "IN-PROGRESS"
                ((org-agenda-overriding-header "IN PROGRESS\n")
                 (org-agenda-sorting-strategy '(priority-down category-keep todo-state-down))))
          (todo "TODO"
                ((org-agenda-overriding-header "NEXT TASKS\n")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                 (org-agenda-sorting-strategy '(priority-down category-keep))))))
        
        ;; Note: We're using the alex-org-agenda-projects function instead of this
        ;; but keeping this for reference
        ("p" "Project view (via function instead)"
         ((tags "PROJECT=\"t\""
                ((org-agenda-overriding-header "ACTIVE PROJECTS\n")))
          (todo "TODO"
                ((org-agenda-overriding-header "TASKS BY PROJECT\n")
                 (org-agenda-prefix-format '((todo . " %i %-12:c")))
                 (org-agenda-sorting-strategy '(category-keep priority-down))))))
        
        ("h" "Priority view"
         ((tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "HIGH PRIORITY TASKS\n")))
          (tags-todo "+PRIORITY=\"B\""
                      ((org-agenda-overriding-header "MEDIUM PRIORITY TASKS\n")))
          (tags-todo "+PRIORITY=\"C\""
                      ((org-agenda-overriding-header "LOW PRIORITY TASKS\n")))))
        
        ("n" "Next actions"
         ((todo "TODO"
                ((org-agenda-overriding-header "NEXT ACTIONS\n")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                 (org-agenda-sorting-strategy '(priority-down category-keep))))))))

;;; Agenda appearance settings
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

;; Set up colors for priorities in agenda
(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold))
        (?B . (:foreground "orange"))
        (?C . (:foreground "yellow"))))

;; Add special faces for deadlines
(setq org-agenda-deadline-faces
      '((1.0 . (:foreground "red" :weight bold))
        (0.5 . (:foreground "orange" :weight bold))
        (0.0 . (:foreground "green"))))

;; Display settings for time grid
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "──────────────────"))

(setq org-agenda-current-time-string "⏰ NOW ───────────────────")

;; Skip deadline prewarning for DONE items
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;;; Additional agenda navigation helpers
(defun alex-org-agenda-goto-today ()
  "Jump to today's agenda."
  (interactive)
  (org-agenda-goto-today)
  (org-agenda-day-view))

;;; Keybindings for agenda navigation
(global-set-key (kbd "C-c a d") 'alex-org-agenda-day)
(global-set-key (kbd "C-c a p") 'alex-org-agenda-projects)
(global-set-key (kbd "C-c a h") 'alex-org-agenda-priority)
(global-set-key (kbd "C-c a n") 'alex-org-agenda-next)
(global-set-key (kbd "C-c a t") 'alex-org-agenda-goto-today)

;; Additional keys in agenda mode
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "d" 'alex-org-agenda-day)
  (define-key org-agenda-mode-map "p" 'alex-org-agenda-projects)
  (define-key org-agenda-mode-map "h" 'alex-org-agenda-priority)
  (define-key org-agenda-mode-map "n" 'alex-org-agenda-next))

(provide 'alex-org-agenda)
;;; alex-org-agenda.el ends here
