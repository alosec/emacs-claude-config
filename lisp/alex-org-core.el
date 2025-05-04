;;; alex-org-core.el --- Core org-mode configuration -*- lexical-binding: t -*-

;; Author: Alex
;; Created: May 2, 2025

;;; Commentary:
;; Core org-mode settings and configuration.
;; This module sets up the foundation for all org-mode functionality.

;;; Code:

(require 'org)

;;; Directory and file setup
(defvar alex-org-directory (expand-file-name "~/org/")
  "Base directory for org files.")

(defvar alex-org-todo-file (expand-file-name "todo.org" alex-org-directory)
  "File for capturing tasks.")

(defvar alex-org-projects-file (expand-file-name "projects.org" alex-org-directory)
  "File for project management.")

(defvar alex-org-journal-directory (expand-file-name "journal/" alex-org-directory)
  "Directory for journal entries.")

;; Create required directories if they don't exist
(unless (file-exists-p alex-org-directory)
  (make-directory alex-org-directory t))

(unless (file-exists-p alex-org-journal-directory)
  (make-directory alex-org-journal-directory t))

;; Create initial files if they don't exist
(unless (file-exists-p alex-org-todo-file)
  (with-temp-file alex-org-todo-file
    (insert (concat "#+TITLE: Todo List\n"
                    "#+AUTHOR: Alex\n"
                    "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n"
                    "* Tasks\n\n"
                    "* Recurring\n\n"
                    "* Someday/Maybe\n"))))

(unless (file-exists-p alex-org-projects-file)
  (with-temp-file alex-org-projects-file
    (insert (concat "#+TITLE: Projects\n"
                    "#+AUTHOR: Alex\n"
                    "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n"
                    "* Active Projects\n\n"
                    "* Planned Projects\n\n"
                    "* Completed Projects\n"))))

;;; Basic org settings
(setq org-directory alex-org-directory)
(setq org-default-notes-file alex-org-todo-file)

;; Files included in agenda
(setq org-agenda-files 
      (list alex-org-todo-file 
            alex-org-projects-file
            ;; Include all journal files
            (expand-file-name "journal" org-directory)))

;; Task states
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; Task state colors
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "gray" :weight bold))))

;; Logging settings
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

;; Indentation and visual settings
(setq org-startup-indented t)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-adapt-indentation nil)
(setq org-pretty-entities t)
(setq org-hide-emphasis-markers t)

;; Display settings
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid 
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "─────────────────"))

;; Tags configuration
(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@errands" . ?e)
                      ("urgent" . ?u)
                      ("important" . ?i)
                      ("project" . ?p)))

;; Refile settings
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;; Global keybindings
;; Primary org navigation from anywhere
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)

;; Quick access to common org files
(defun alex-org-open-todo ()
  "Open the main TODO file."
  (interactive)
  (find-file alex-org-todo-file))

(defun alex-org-open-projects ()
  "Open the projects file."
  (interactive)
  (find-file alex-org-projects-file))

(defun alex-org-open-file-menu ()
  "Open a menu to select an org file to visit."
  (interactive)
  (let ((file (completing-read "Org file: " 
                             (list "todo.org" "projects.org"))))
    (find-file (expand-file-name file alex-org-directory))))

(global-set-key (kbd "C-c o f") 'alex-org-open-file-menu)
(global-set-key (kbd "C-c o t") 'alex-org-open-todo)
(global-set-key (kbd "C-c o p") 'alex-org-open-projects)

;; For clarity, add direct access to project view (to complement C-c a p)
(global-set-key (kbd "C-c o v p") 'alex-org-agenda-projects)

;; Buffer-local keys for org-mode
(define-key org-mode-map (kbd "C-c o r") 'org-refile)

(provide 'alex-org-core)
;;; alex-org-core.el ends here
