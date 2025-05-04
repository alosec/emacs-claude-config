;;; alex-org-capture.el --- Custom capture templates -*- lexical-binding: t -*-

;; Author: Alex
;; Created: May 2, 2025

;;; Commentary:
;; Streamlined templates for quickly capturing different types of information.
;; Makes it easy to capture tasks, notes, and other information.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'alex-org-core)
(require 'alex-org-projects)

;;; Custom capture functions

(defun alex-org-capture-task ()
  "Capture a basic task using the task template."
  (interactive)
  (org-capture nil "t"))

(defun alex-org-capture-project-task ()
  "Capture a task directly into a project."
  (interactive)
  (org-capture nil "p"))

(defun alex-org-capture-meeting ()
  "Capture meeting notes with attendees."
  (interactive)
  (org-capture nil "m"))

(defun alex-org-capture-idea ()
  "Capture a quick idea or note."
  (interactive)
  (org-capture nil "i"))

;;; Journal functions

(defun alex-org-journal-file ()
  "Return filename for today's journal entry."
  (let* ((time (decode-time))
         (year (nth 5 time))
         (month (nth 4 time))
         (day (nth 3 time))
         (journal-dir (expand-file-name (format "%d/%02d" year month) alex-org-journal-directory)))
    (unless (file-exists-p journal-dir)
      (make-directory journal-dir t))
    (expand-file-name (format "%02d-%02d-%d.org" month day year) journal-dir)))

(defun alex-org-capture-journal ()
  "Capture a journal entry for today."
  (interactive)
  (org-capture nil "j"))

;;; Project task capture helper

(defun alex-org-capture-select-project ()
  "Select a project and find or create a Tasks subheading."
  (let ((marker (alex-org-select-project)))
    (set-buffer (marker-buffer marker))
    (goto-char (marker-position marker))
    
    ;; Check if "Tasks" subheading exists
    (let ((tasks-exists nil)
          (project-level (org-current-level)))
      (save-excursion
        (org-back-to-heading t)
        (org-map-entries
         (lambda ()
           (when (and (= (org-current-level) (1+ project-level))
                      (string= (org-get-heading t t t t) "Tasks"))
             (setq tasks-exists t)))
         nil 'tree))
      
      ;; If no Tasks subheading, create one
      (if (not tasks-exists)
          (progn
            (org-back-to-heading t)
            (org-end-of-meta-data t) ;; Skip properties if any
            (newline)
            (insert (format "*** Tasks\n"))))
      
      ;; Find the Tasks subheading
      (org-back-to-heading t)
      (let ((found nil))
        (org-map-entries
         (lambda ()
           (when (and (= (org-current-level) (1+ project-level))
                      (string= (org-get-heading t t t t) "Tasks"))
             (setq found t)
             (org-end-of-subtree t)
             (if (not (bolp)) (newline))))
         nil 'tree)
        
        ;; If we somehow couldn't find it, just return the project point
        (if (not found)
            (org-back-to-heading t)))
      
      ;; Return the current point as marker
      (point-marker))))

;;; Capture templates

(setq org-capture-templates
      `(("t" "Task" entry
         (file+headline alex-org-todo-file "Tasks")
         "* TODO [#%^{Priority|A|B|C}] %^{Description}\n%i\n%U\n%?"
         :empty-lines 1)
        
        ("p" "Project Task" entry
         (function alex-org-capture-select-project)
         "* TODO [#%^{Priority|A|B|C}] %^{Task}\n%i\n%U\nDeadline: %^{Deadline}t\n%?"
         :empty-lines 1)
        
        ("m" "Meeting" entry
         (file+headline alex-org-todo-file "Tasks")
         "* MEETING [#%^{Priority|A|B|C}] %^{Meeting Title}\n%i\n%U\nAttendees: %^{Attendees}\n\n%?"
         :empty-lines 1)
        
        ("i" "Idea" entry
         (file+headline alex-org-todo-file "Someday/Maybe")
         "* IDEA %^{Idea}\n%i\n%U\n%?"
         :empty-lines 1)
        
        ("j" "Journal" entry
         (file+olp+datetree alex-org-journal-file)
         "* %<%H:%M> %^{Title}\n%i\n%?"
         :empty-lines 1)))

;; Customize the capture dialog format
(setq org-capture-templates-contexts '())
(setq org-capture-use-agenda-date nil)

;;; Add journal functions
(defun alex-org-journal-new-entry ()
  "Create a new journal entry for today."
  (interactive)
  (let ((journal-file (alex-org-journal-file)))
    (unless (file-exists-p journal-file)
      (with-temp-file journal-file
        (insert (format "#+TITLE: Journal Entry %s\n#+DATE: %s\n\n* Journal\n" 
                        (format-time-string "%Y-%m-%d") 
                        (format-time-string "%Y-%m-%d %A")))))
    (find-file journal-file)
    (goto-char (point-max))))

(defun alex-org-journal-view-month ()
  "View journal entries for the current month."
  (interactive)
  (let* ((time (decode-time))
         (year (nth 5 time))
         (month (nth 4 time))
         (journal-dir (expand-file-name (format "%d/%02d" year month) alex-org-journal-directory)))
    (if (file-exists-p journal-dir)
        (dired journal-dir)
      (message "No journal entries for %d-%02d" year month))))

(defun alex-org-journal-search ()
  "Search journal entries."
  (interactive)
  (let ((search-term (read-string "Search journal for: ")))
    (org-search-view nil search-term)))

;;; Keybindings
(global-set-key (kbd "C-c c t") 'alex-org-capture-task)
(global-set-key (kbd "C-c c p") 'alex-org-capture-project-task)
(global-set-key (kbd "C-c c m") 'alex-org-capture-meeting)
(global-set-key (kbd "C-c c i") 'alex-org-capture-idea)
(global-set-key (kbd "C-c c j") 'alex-org-capture-journal)

;; Journal specific keys
(global-set-key (kbd "C-c j j") 'alex-org-journal-new-entry)
(global-set-key (kbd "C-c j m") 'alex-org-journal-view-month)
(global-set-key (kbd "C-c j s") 'alex-org-journal-search)

(provide 'alex-org-capture)
;;; alex-org-capture.el ends here
