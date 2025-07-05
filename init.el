;; Streamlined Emacs configuration for Debian
;; Based on original from Thu Jul 29, 2021

(global-visual-line-mode 1)

;; Disable the welcome screen
(setq inhibit-startup-screen t)

;; Silence warnings
(setq byte-compile-warnings '(not cl-functions obsolete free-vars unresolved noruntime lexical make-local))

;; Allow upgrading built-in packages (needed for seq package)
(setq package-install-upgrade-built-in t)

;; Ensure we have the latest seq loaded
(eval-after-load 'package
  '(progn
     (when (package-installed-p 'seq)
       (unload-feature 'seq t)
       (require 'seq))))

;; Silence native compilation warnings
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil))

;; Disable GUI elements conditionally
(menu-bar-mode -1)
(when (display-graphic-p)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)))

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use-package setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; TRAMP setup for remote access
(require 'tramp)
(setq tramp-default-method "ssh")

;; Font size (adjust as needed for your display)
(when (display-graphic-p)
  (let ((font-size (if (>= (x-display-pixel-width) 2560) 
                       130  ;; HiDPI display 
                     120))) ;; Standard display
    (set-face-attribute 'default nil :height font-size)))

;; Useful keybindings
(global-set-key (kbd "C-w") 'backward-word)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x r") 'jump-to-register)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'gptel-send)

;; Common registers
(set-register ?e (cons 'file "~/.emacs.d/init.el"))  ;; Updated to point to the new init.el location
(set-register ?g (cons 'file "~/.gtd"))

;; Function to reload init file
(defun reload-dotemacs ()
  (interactive)
  (load-file user-init-file))  ;; Updated to use user-init-file variable
(global-set-key (kbd "C-c <f12>") 'reload-dotemacs)

;; Load GTD file if it exists
(when (file-exists-p "~/.gtd")
  (load-file "~/.gtd"))

;; Mode associations
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Org-mode configuration - Modular enhanced version
(use-package org
  :ensure t
  :config
  ;; Load enhanced org modules
  (require 'alex-org-core)
  (require 'alex-org-projects)
  (require 'alex-org-tasks)
  (require 'alex-org-agenda)
  (require 'alex-org-capture))

;; Keep the existing org-cycle-separator-lines setting
(setq org-cycle-separator-lines 1)

;; Create directory for org-agenda icons if it doesn't exist yet
(unless (file-exists-p (expand-file-name "icons" user-emacs-directory))
  (make-directory (expand-file-name "icons" user-emacs-directory)))

;; Use smooth scrolling if available
(use-package smooth-scroll
  :if (package-installed-p 'smooth-scroll)
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 0.1))

;; Use gptel (ChatGPT integration) if available
(use-package gptel
  :if (package-installed-p 'gptel))

;; Optional: Magit for Git integration
(use-package magit
  :if (package-installed-p 'magit))

;; Optional: Flycheck for syntax checking
(use-package flycheck
  :if (package-installed-p 'flycheck)
  :hook (typescript-mode . flycheck-mode))

;; Helm configuration
(use-package helm
  :diminish
  :init
  (progn
    ;; (require 'helm-config) - Removed as it's no longer needed in newer Helm versions
    (setq helm-candidate-number-limit 100)
    ;; From https://tuhdo.github.io/helm-intro.html
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (setq helm-split-window-in-side-p t ; Open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source t ; Move to end or beginning of source when reaching top or bottom of source
          helm-ff-search-library-in-sexp t ; Search for library in `require' and `declare-function' sexp
          helm-scroll-amount 8 ; Scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t))
  :config
  (progn
    (setq helm-autoresize-max-height 0
          helm-autoresize-min-height 20)
    (helm-autoresize-mode 1)
    (helm-mode 1))
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-s" . helm-occur)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; Rebind tab to do persistent action
         ("C-i" . helm-execute-persistent-action) ; Make TAB works in terminal
         ("C-z" . helm-select-action) ; List actions using C-z
         ))

;; Helm-swoop for improved searching
(use-package helm-swoop
  :after helm
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)
         ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop))
  :config
  (setq helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-speed-or-color t
        helm-swoop-move-to-line-cycle t
        helm-swoop-use-line-number-face t
        helm-swoop-use-fuzzy-match t))

;; Add projectile and helm-projectile for project management
(use-package projectile
  :ensure t
  :init
  ;; Set up directory for projects
  (setq projectile-project-search-path '("~/projects/"))
  ;; Enable caching for better performance
  (setq projectile-enable-caching t)
  :config
  ;; Enable projectile mode
  (projectile-mode +1)
  ;; Explicitly define the keymap binding - THIS IS THE CRITICAL PART
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  ;; Set helm-projectile as the default action when switching projects
  (setq projectile-switch-project-action #'helm-projectile))

;; Helm-projectile configuration 
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :commands (helm-projectile-find-file helm-projectile-switch-project)
  :init
  ;; Ensure keybindings are set up properly before loading
  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p h") #'helm-projectile))
  :config
  ;; Enable all helm-projectile features
  (helm-projectile-on))

;; Custom notes directory fuzzy search with Helm
(defvar my-notes-directory "~/notes"
  "Directory containing personal notes files.")

(defun my-helm-notes-find-files ()
  "Quickly open files in `my-notes-directory' using Helm with fuzzy matching."
  (interactive)
  (helm-find-files-1 my-notes-directory))

(defun my-helm-search-notes ()
  "Search for files in notes directory with Helm."
  (interactive)
  (let ((helm-source-list
         (list
          (helm-build-sync-source "Notes Files"
            :candidates (lambda ()
                          (directory-files-recursively 
                           my-notes-directory 
                           ".*" t))
            :fuzzy-match t
            :action '(("Open file" . find-file))))))
    (helm :sources helm-source-list
          :buffer "*helm notes*")))

;; Create a key binding for quick access to notes
(global-set-key (kbd "C-c n") 'my-helm-search-notes)

;; Set theme conditionally based on GUI
(when (display-graphic-p)
  (load-theme 'deeper-blue t))
;; No theme loading for terminal mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASTRO MODE CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a derived mode based on web-mode for .astro files
(define-derived-mode astro-mode web-mode "Astro"
  "Major mode for editing Astro templates derived from web-mode.")

;; Associate .astro files with astro-mode
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

;; LSP-MODE CONFIGURATION FOR ASTRO
(with-eval-after-load 'lsp-mode
  ;; Register Astro language server
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/home/alex/.nvm/versions/node/v20.19.0/bin/astro-ls" "--stdio"))
                    :activation-fn (lsp-activate-on "astro")
                    :server-id 'astro-ls))
  
  ;; Auto-start LSP when editing Astro files
  (add-hook 'astro-mode-hook #'lsp))

;; Basic web-mode settings for Astro files
(add-hook 'astro-mode-hook
          (lambda ()
            ;; Set indentation
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-SUPER-AGENDA CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install and configure org-super-agenda
(use-package org-super-agenda
  :ensure t
  :after org
  :init
  ;; Ensure package is installed
  (unless (package-installed-p 'org-super-agenda)
    (package-refresh-contents)
    (package-install 'org-super-agenda))
  
  :config
  ;; Enable org-super-agenda mode
  (org-super-agenda-mode)
  
  ;; Define our three configurations
  
  ;; 1. Project-Focused View
  (setq org-super-agenda-groups-project-focused
        '((:name "⚠ Critical Deadlines"
                :and (:deadline today :priority "A"))
          (:name "⏰ Today's Schedule"
                :time-grid t)
          (:name "✅ Active Projects"
                :and (:tag "PROJECT" :todo "NEXT"))
          (:name "⚙ In-Progress Tasks"
                :todo "NEXT")
          (:name "⏳ Waiting Tasks"
                :todo "WAITING")
          (:name "📋 Project Tasks"
                :and (:property ("PROJECT") :todo "TODO"))
          (:name "🔄 Recurring Tasks"
                :tag "RECURRING")
          (:name "📍 Standalone Tasks"
                :todo "TODO")))
  
  ;; 2. Context-Based Organization
  (setq org-super-agenda-groups-context-based
        '((:name "🔥 Priority Items"
                :priority "A")
          (:name "⏰ Time-Sensitive"
                :or (:scheduled today :deadline today))
          (:name "💻 @Office"
                :tag "@office")
          (:name "🏠 @Home"
                :tag "@home")
          (:name "🚗 @Errands"
                :tag "@errand")
          (:name "📞 Communications"
                :or (:tag "PHONE" :tag "EMAIL"))
          (:name "🛠 Active Projects" 
                :and (:tag "PROJECT" :todo "NEXT"))
          (:name "🔍 Research & Reading"
                :tag "READ")
          (:auto-category t)))
  
  ;; 3. Time-Oriented Dashboard
  (setq org-super-agenda-groups-time-oriented
        '((:name "⚠ Overdue"
                :and (:deadline past :not (:todo "DONE")))
          (:name "🚨 Today's Deadlines"
                :deadline today)
          (:name "📅 Today's Schedule"
                :time-grid t)
          (:name "🔜 Coming Up This Week"
                :and (:deadline future :not (:todo "WAITING")))
          (:name "⏯ Currently Active"
                :todo "NEXT")
          (:name "⌛ Waiting For"
                :todo "WAITING")
          (:name "🔭 Future Projects"
                :and (:todo "TODO" :tag "PROJECT" 
                      :not (:deadline t) :not (:scheduled t)))
          (:discard (:anything t))))
  
  ;; Set the default view to project-focused
  (setq org-super-agenda-groups org-super-agenda-groups-project-focused)
  
  ;; Custom faces for org-super-agenda headers
  (custom-set-faces
   '(org-super-agenda-header 
     ((t (:inherit default :height 1.2 :weight bold 
                   :foreground "dodger blue" :background "#f0f0f0"
                   :box (:line-width 2 :color "grey75" :style released-button))))))
  
  ;; Function to toggle between the three views
  (defun alex-org-super-agenda-toggle-view ()
    "Cycle between different org-super-agenda view configurations."
    (interactive)
    (cond
     ;; If current view is project-focused, switch to context-based
     ((equal org-super-agenda-groups org-super-agenda-groups-project-focused)
      (setq org-super-agenda-groups org-super-agenda-groups-context-based)
      (message "Switched to Context-Based View"))
     ;; If current view is context-based, switch to time-oriented
     ((equal org-super-agenda-groups org-super-agenda-groups-context-based)
      (setq org-super-agenda-groups org-super-agenda-groups-time-oriented)
      (message "Switched to Time-Oriented View"))
     ;; Otherwise, switch to project-focused
     (t
      (setq org-super-agenda-groups org-super-agenda-groups-project-focused)
      (message "Switched to Project-Focused View")))
    ;; Refresh the agenda view
    (when (get-buffer "*Org Agenda*")
      (with-current-buffer "*Org Agenda*"
        (org-agenda-redo))))
  
  ;; Create custom agenda commands for each view
  (setq org-agenda-custom-commands
        '(("p" "Project-Focused View"
           ((agenda "" ((org-super-agenda-groups org-super-agenda-groups-project-focused)))))
          ("c" "Context-Based View"
           ((agenda "" ((org-super-agenda-groups org-super-agenda-groups-context-based)))))
          ("t" "Time-Oriented View"
           ((agenda "" ((org-super-agenda-groups org-super-agenda-groups-time-oriented)))))))
  
  ;; Define key bindings for org-super-agenda views
  (global-set-key (kbd "C-c a s") 'org-super-agenda-header-map)
  (global-set-key (kbd "C-c a v") 'alex-org-super-agenda-toggle-view)
  
  ;; Specific view access keys
  (global-set-key (kbd "C-c a 1") 
                  (lambda () (interactive) 
                    (setq org-super-agenda-groups org-super-agenda-groups-project-focused)
                    (org-agenda nil "a")
                    (message "Project-Focused View")))
  
  (global-set-key (kbd "C-c a 2") 
                  (lambda () (interactive) 
                    (setq org-super-agenda-groups org-super-agenda-groups-context-based)
                    (org-agenda nil "a")
                    (message "Context-Based View")))
  
  (global-set-key (kbd "C-c a 3") 
                  (lambda () (interactive) 
                    (setq org-super-agenda-groups org-super-agenda-groups-time-oriented)
                    (org-agenda nil "a")
                    (message "Time-Oriented View"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLAUDE-CODE.EL CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add claude-code to load path
(add-to-list 'load-path (expand-file-name "site-lisp/claude-code" user-emacs-directory))

;; Load and configure claude-code
(use-package claude-code
  :ensure nil  ; We installed it manually
  :demand t    ; Load immediately
  :config
  ;; Enable claude-code-mode
  (claude-code-mode 1)
  
  ;; Basic configuration
  (setq claude-code-terminal-backend 'eat)  ; Use EAT terminal (correct variable name)
  
  ;; Optional: Set default Claude Code CLI path if not in PATH
  ;; (setq claude-code-program "claude-code")
  
  :bind-keymap
  ;; Bind the entire command map to C-c c prefix
  ("C-c c" . claude-code-command-map))

;; Custom-set-variables maintained from original ~/.emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (if (display-graphic-p) '(deeper-blue) nil))
 '(package-selected-packages
   '(eat eglot eww-lnum helm helm-projectile helm-swoop lsp-mode
	 markdown-mode markdown-preview-eww org-super-agenda
	 projectile use-package vmd-mode web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-super-agenda-header ((t (:inherit default :height 1.2 :weight bold :foreground "dodger blue" :background "#f0f0f0" :box (:line-width 2 :color "grey75" :style released-button))))))
