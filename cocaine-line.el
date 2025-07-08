;;; cocaine-line.el --- A customized modeline for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; This package offers a customized modeline for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'battery)
(require 'cl-lib)
(require 'evil)
(require 'mode-line-hud)
(require 'nerd-icons)
(require 'project)
(require 'vc)
(require 'doom-modeline)

;; Import all new modular components
(require 'cocaine-line-vc)
(require 'cocaine-line-macro)
(require 'cocaine-line-music)
(require 'cocaine-line-modal)
(require 'cocaine-line-battery)
(require 'cocaine-line-misc)
(require 'cocaine-line-weather)
(require 'cocaine-line-term)
(require 'cocaine-line-systemmonitor)
(require 'cocaine-line-package)
(require 'cocaine-line-what-am-i-doing)
(require 'cocaine-line-spinner)

(unless (bound-and-true-p battery-status-function)
  (battery-update-handler))

(defvar-local cocaine-line-is-active t
  "Indicates if the current window is active.")

(defgroup cocaine-line nil
  "Customizations for cocaine-line."
  :group 'mode-line)

;; Configuration options (preserved from original)
(defcustom cocaine-right-padding 0
  "Number of spaces to add after the time."
  :type 'integer
  :group 'cocaine-line)

(defcustom cocaine-show-column-info nil
  "If set to t, show column information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-buffer-position nil
  "If set to t, show buffer position."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-battery-info t
  "If set to t, show battery icons with nerdicons."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-use-nerd-icons t
  "If set to t, show file icons with nerdicons."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-processes-info t
  "If set to t, show active processes."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-misc-info nil
  "If set to t, show misc processes."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-git-info t
  "If set to t, show Git branch and status."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-copilot-info t
  "If set to t, show copilot icon."
  :type 'boolean
  :group 'cocaine-line)

;; Preserve original cocaine-line faces and color customizations
(defface cocaine-line-buffer-name-face
  '((t :foreground "#a0a0ae" :weight bold))
  "Face for buffer name."
  :group 'cocaine-line)

(defface cocaine-line-major-mode-face
  '((t :inherit doom-modeline-buffer-major-mode :weight bold))
  "Face for major mode."
  :group 'cocaine-line)

(defface cocaine-line-position-face
  '((t :foreground "#FFA07A"))
  "Face for buffer position."
  :group 'cocaine-line)

(defface cocaine-line-time-face
  '((t :foreground "#888899":weight light))
  "Face for time display."
  :group 'cocaine-line)

(defface cocaine-line-git-face
  '((t :foreground "#a0a0ae"))
  "Standard face for Git information."
  :group 'cocaine-line)

(defface cocaine-line-project-face
  '((t :foreground "#FFA066" :weight bold))
  "Standard face for project information."
  :group 'cocaine-line)

(defface cocaine-line-eglot-icon-face
  '((t :inherit eglot-mode-line))
  "Standard face for project information."
  :group 'cocaine-line)

;; Git faces customization (preserved)
(defcustom cocaine-git-faces
  '((edited . cocaine-line-git-edited-face)
    (added . cocaine-line-git-added-face)
    (removed . cocaine-line-git-removed-face)
    (conflict . cocaine-line-git-conflict-face)
    (default . cocaine-line-git-face))
  "Faces for different Git states."
  :type '(alist :key-type symbol :value-type face)
  :group 'cocaine-line)

(defface cocaine-line-git-edited-face
  '((t :foreground "#F1FA8C"))
  "Face for edited Git files."
  :group 'cocaine-line)

(defface cocaine-line-git-added-face
  '((t :foreground "#50FA7B"))
  "Face for added Git files."
  :group 'cocaine-line)

(defface cocaine-line-git-removed-face
  '((t :foreground "#FF5555"))
  "Face for removed Git files."
  :group 'cocaine-line)

(defface cocaine-line-git-conflict-face
  '((t :foreground "#FF79C6"))
  "Face for Git conflicts."
  :group 'cocaine-line)

;; Evil mode faces customization (preserved)
(defcustom cocaine-evil-faces
  '((normal . cocaine-line-evil-normal-face)
    (insert . cocaine-line-evil-insert-face)
    (visual . cocaine-line-evil-visual-face)
    (replace . cocaine-line-evil-replace-face)
    (emacs . cocaine-line-evil-emacs-face))
  "Faces for different Evil states."
  :type '(alist :key-type symbol :value-type face)
  :group 'cocaine-line)

(defface cocaine-line-evil-normal-face
  '((t :foreground "#FFFFFF" :background "#2D4F67" :weight bold
       :box (:line-width 8 :color "#2D4F67")))
  "Face for Evil normal state."
  :group 'cocaine-line)

(defface cocaine-line-evil-insert-face
  '((t :foreground "#333333" :background "#E6C384" :weight bold
       :box (:line-width 8 :color "#E6C384")))
  "Face for Evil insert state."
  :group 'cocaine-line)

(defface cocaine-line-evil-visual-face
  '((t :foreground "#333333" :background "#D27E99" :weight bold
       :box (:line-width 8 :color "#D27E99")))
  "Face for Evil visual state."
  :group 'cocaine-line)

(defface cocaine-line-evil-replace-face
  '((t :foreground "#333333" :background "#FF5D62" :weight bold
       :box (:line-width 8 :color "#FF5D62")))
  "Face for Evil replace state."
  :group 'cocaine-line)

(defface cocaine-line-evil-emacs-face
  '((t :foreground "#333333" :background "#B0C4DE" :weight bold
       :box (:line-width 8 :color "#B0C4DE")))
  "Face for Emacs state."
  :group 'cocaine-line)

(defface cocaine-line-separator-face
  '((t :foreground "#54536D" :weight bold :height 0.8))
  "Face for the separator between sections in cocaine-line."
  :group 'cocaine-line)

;; Additional faces for new features (minimal styling to match existing)
(defface cocaine-line-inactive-face
  '((t :inherit mode-line-inactive))
  "Face for inactive mode-line elements."
  :group 'cocaine-line)

(defface cocaine-line-what-am-i-doing-face
  '((t :inherit mode-line-emphasis))
  "Face for the what-am-i-doing functionality."
  :group 'cocaine-line)

(defface cocaine-line-what-am-i-doing-count-face
  '((t :inherit success))
  "Face for displaying current task in mode line."
  :group 'cocaine-line)

(defface cocaine-line-system-monitor-cpu-face
  '((t :inherit font-lock-constant-face))
  "Face for CPU usage in system monitor."
  :group 'cocaine-line)

(defface cocaine-line-system-monitor-memory-face
  '((t :inherit font-lock-function-call-face))
  "Face for memory usage in system monitor."
  :group 'cocaine-line)

(defface cocaine-line-music-face
  '((t (:inherit font-lock-comment-face)))
  "Face for music information."
  :group 'cocaine-line)

(defface cocaine-line-music-apple-face
  '((t (:foreground "#ff2d55")))
  "Face for apple music information."
  :group 'cocaine-line)

(defface cocaine-line-music-spotify-face
  '((t (:foreground "#1db954")))
  "Face for spotify music information."
  :group 'cocaine-line)

(defface cocaine-line-macro-face
  '((t :foreground "#333333" :background "#B0C4DE" :weight bold))
  "Face for macro recording state."
  :group 'cocaine-line)

(defface cocaine-line-macro-recording-face
  '((t :foreground "#222233" :background "#FF5D62" :weight bold))
  "Face for macro recording state."
  :group 'cocaine-line)

;; Caching and update system from punch-line
(defvar cocaine-line--update-timer nil
  "Timer for debouncing mode-line updates.")

(defvar-local cocaine-line--cached-left nil
  "Cache for the left section of the mode-line.")

(defvar-local cocaine-line--cached-right nil
  "Cache for the right section of the mode-line.")

(defvar-local cocaine-line--cached-right-str nil
  "Cached string for the right section of the mode-line.")

(defvar cocaine-line--last-update 0
  "Timestamp of last mode-line update.")

(defcustom cocaine-line-min-update-interval 0.1
  "Minimum interval between mode-line updates in seconds."
  :type 'number
  :group 'cocaine-line)

(defvar cocaine-line-is-active nil)

(defvar-local cocaine-line--cached-fill nil
  "Cached fill for the mode-line.")

(defvar-local cocaine-line--cached-right-width nil
  "Cached width of the right section of the mode-line.")

(defvar cocaine-line-active-window nil
  "Stores the currently active window.")

(defun cocaine-line-window-active-p ()
  "Return non-nil if the current window is active."
  (let ((current (get-buffer-window))
        (active cocaine-line-active-window))
    (eq current active)))

;; Preserved separator configurations
(defcustom cocaine-line-separator " | "
  "Separator used between sections in the modeline."
  :type 'string
  :group 'cocaine-line)

(defcustom cocaine-line-left-separator "  "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'cocaine-line)

(defcustom cocaine-line-right-separator "  "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'cocaine-line)

(defcustom cocaine-line-separator-face 'cocaine-line-separator-face
  "Face for the separator between sections."
  :type 'face
  :group 'cocaine-line)

(defun cocaine-line-inactive-bg ()
  "Get the background color of the mode-line-inactive face."
  (face-background 'mode-line-inactive nil t))

(defun cocaine-line-update-inactive-face ()
  "Update the cocaine-line-inactive-face with the current theme colors."
  (let* ((bg-color (face-background 'mode-line-inactive nil t))
         (fg-color (face-foreground 'mode-line-inactive nil t)))
    (set-face-attribute 'cocaine-line-inactive-face nil
                        :background bg-color
                        :foreground fg-color  
                        :box `(:line-width ,(cocaine-line-modal-height) :color ,bg-color)
                        :underline nil)))

;; Enhanced separator function with improved styling
(cl-defun cocaine-add-separator (&key str separator leftside (last nil)
                                      (face 'cocaine-line-separator-face))
  "Add a separator after STR if it is not empty or last.
    LAST indicates if this is the last element.
    FACE specifies which face to use for the separator."
  (if (and str (not (string-empty-p str)) (not last))
      (if (not separator)
          str
        (let* ((height (cocaine-line-get-divider-icon-height))
               (divider (propertize separator
                                  'face `(:inherit ,face
                                         :height ,height))))
          (if leftside
              (concat str divider)
            (concat divider str))))
    str))

;; Preserved custom functions from original cocaine-line
(defun cocaine-flycheck-mode-line ()
  "Custom flycheck modeline with icons and counts."
  (when (and (bound-and-true-p flycheck-mode)
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (let* ((count (flycheck-count-errors flycheck-current-errors))
           (info (or (cdr (assq 'info count)) 0))
           (warning (or (cdr (assq 'warning count)) 0))
           (error (or (cdr (assq 'error count)) 0)))
      (concat
       (when (> info 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-lightbulb") info)
                     'face '(:inherit success)))
       (when (> warning 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-warning") warning)
                     'face '(:inherit warning)))
       (when (> error 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-error") error)
                     'face '(:inherit error)))))))

(defun cocaine-eglot-info ()
  "Return a string representing the current Eglot status for the mode line using nerd-icons."
  (if (bound-and-true-p eglot--managed-mode)
      (let* ((server (eglot-current-server))
             (nick (and server (eglot--project-nickname server)))
             (icon (propertize (nerd-icons-octicon "nf-oct-plug")
                               'face 'cocaine-line-eglot-icon-face)))
        (if server
            (concat (propertize (or nick "") 'face 'cocaine-line-project-face) " " icon " ")
          icon))
    (cocaine-project-info)))

(defun cocaine-project-name ()
  "Get the project name if any."
  (or
   (and (fboundp 'project-name)
        (project-current)
        (project-name (project-current)))
   (and (fboundp 'projectile-project-name)
        (projectile-project-name))))

(defface cocaine-modeline-project-dir
  '((t (:inherit (doom-modeline font-lock-string-face bold) :weight bold)))
  "Face used for the project directory of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defun cocaine-project-info ()
  "Show project information."
  (let ((project (cocaine-project-name)))
    (propertize project 'face 'doom-modeline-project-parent-dir)))

(defface cocaine-modeline-buffer-modified
  '((t (:inherit (doom-modeline warning bold) :background unspecified :weight bold)))
  "Face used for the \\='unsaved\\=' symbol in the mode-line."
  :group 'doom-modeline-faces)

;; Preserved doom modeline bar function
(defsubst salih/doom-modeline--bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and doom-modeline--bar-active doom-modeline--bar-inactive)
    (let ((width doom-modeline-bar-width)
          (height (max doom-modeline-height (doom-modeline--font-height))))
      (setq doom-modeline--bar-active
            (doom-modeline--create-bar-image
             'mode-line width height)
            doom-modeline--bar-inactive
            (doom-modeline--create-bar-image
             'mode-line-inactive width height))))
  (if (doom-modeline--active)
      doom-modeline--bar-active
    doom-modeline--bar-inactive))

;; Enhanced format functions with new features
(defun cocaine-line-format-left ()
  "Create the left section of the mode-line with caching."
  (list (concat
         (salih/doom-modeline--bar) ;; preserved doom modeline height adjustment
         (cocaine-add-separator :str
                                (doom-modeline-segment--modals)
                                :leftside t
                                :separator "| ")
         (cocaine-add-separator :str (cocaine-macro-info) :separator cocaine-line-left-separator)
         (cocaine-add-separator :str (cocaine-iedit-info) :separator cocaine-line-left-separator)
         (cocaine-add-separator :str (cocaine-evil-mc-info) :separator cocaine-line-left-separator)
         (cocaine-add-separator :str (cocaine-evil-status) :separator cocaine-line-left-separator)
         (cocaine-add-separator :str (cocaine-buffer-name) :separator "|")
         (cocaine-add-separator :str (cocaine-major-mode) :separator " | ")
         (cocaine-add-separator :str (cocaine-project-info) :separator cocaine-line-left-separator)
         (cocaine-add-separator :str (cocaine-flycheck-info) :separator cocaine-line-left-separator)
         (cocaine-add-separator :str (cocaine-what-am-i-doing-info) :separator cocaine-line-left-separator)
         (cocaine-add-separator :str (mode-line-segment-hud))
         (cocaine-add-separator :str (cocaine-process-info)))))

(defun cocaine-line-format-right ()
  "Create the right section of the mode-line with caching."
  (concat
   ;; preserved doom modeline segments
   (if (derived-mode-p 'pdf-view-mode)
       (cocaine-add-separator
        :str (propertize (salih/doom-modeline-update-pdf-pages-no-percent)) :leftside t)
     "")
   ;; new features from punch-line
   (cocaine-add-separator :str (cocaine-line-music-info) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-system-monitor-info) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-line-col) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-buffer-position) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-copilot-info) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-term-info) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-misc-info) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-git-info) :separator cocaine-line-right-separator :leftside t)
   (cocaine-add-separator :str (cocaine-weather-info) :separator cocaine-line-right-separator :leftside t)
   ;; preserved doom modeline segments
   (cocaine-add-separator :str (doom-modeline-segment--salih/selection-info) :leftside t)
   (cocaine-add-separator :str (doom-modeline-segment--matches) :leftside t)
   (cocaine-add-separator :str (doom-modeline-segment--irc) :leftside t)
   (cocaine-add-separator :str (and (boundp 'mu4e-alert-mode-line) (or mu4e-alert-mode-line ""))  :leftside t)
   (cocaine-add-separator :str (doom-modeline-segment--lsp) :leftside t)
   (cocaine-add-separator :str (doom-modeline-segment--minor-modes) :leftside t)
   (cocaine-add-separator :str (cocaine-flycheck-mode-line) :leftside t)
   (cocaine-add-separator :str (doom-modeline-segment--vcs) :leftside t)
   (cocaine-battery-info)
   (cocaine-time-info)))

(defun cocaine-line-format-inactive ()
  "Inactive format with Evil status and buffer name in gray."
  (propertize (concat " " (cocaine-buffer-name)) 'face 'cocaine-line-inactive-face))

(defun cocaine-line-format ()
  "Generate the mode-line format."
  (if (cocaine-line-window-active-p)
      (list (cocaine-line-format-left)
            (cocaine-line-get-fill)
            (cocaine-line-format-right))
    (cocaine-line-format-inactive)))

;; Enhanced update system from punch-line
(defun cocaine-line-update (&optional force)
  "Update mode-line for all windows.
If FORCE is non-nil, bypass the update interval check."
  (let ((current-time (float-time)))
    (when (or force
              (> (- current-time cocaine-line--last-update)
                 cocaine-line-min-update-interval))
      (when cocaine-line--update-timer
        (cancel-timer cocaine-line--update-timer))
      (setq cocaine-line--update-timer
            (run-with-idle-timer
             0.05 nil
             (lambda ()
               (setq cocaine-line--last-update current-time
                     cocaine-line-active-window (selected-window)
                     cocaine-line--cached-left nil
                     cocaine-line--cached-right nil)
               (force-mode-line-update t)))))))

(defun cocaine-line-set-mode-line ()
  "Set the mode-line format for cocaine-line."
  (setq-default mode-line-format '(:eval (cocaine-line-format))))

;; Enhanced fill calculation with caching
(defun cocaine-line-calculate-fill (right-section)
  "Calculate the fill space needed to right-align the RIGHT-SECTION."
  (let ((right-width (string-width (or right-section ""))))
    (setq cocaine-line--cached-right-width right-width)
    (propertize " " 'display
                `((space :align-to (- right ,(- right-width 1)))))))

(defun cocaine-line-get-fill ()
  "Get the fill space needed to right-align content with caching."
  (let* ((right-section (cocaine-line-format-right))
         (current-width (string-width (or right-section ""))))
    (if (and cocaine-line--cached-fill
             cocaine-line--cached-right-width
             (= current-width cocaine-line--cached-right-width))
        cocaine-line--cached-fill
      (setq cocaine-line--cached-fill
            (cocaine-line-calculate-fill right-section)))))

(defun cocaine-line-invalidate-fill-cache ()
  "Invalidate the fill cache."
  (setq cocaine-line--cached-fill nil
        cocaine-line--cached-right-width nil
        cocaine-line--cached-right-str nil))

(defun cocaine-line-invalidate-caches ()
  "Invalidate all caches."
  (cocaine-line-invalidate-fill-cache))

;; Enhanced hook management
(defun cocaine-line-register-hooks ()
  "Register hooks to update the mode-line."
  (add-hook 'post-command-hook #'cocaine-line-update)
  (add-hook 'window-configuration-change-hook #'cocaine-line-update)
  (add-hook 'focus-in-hook #'cocaine-line-update)
  (add-hook 'focus-out-hook #'cocaine-line-update)
  (add-hook 'window-buffer-change-functions #'cocaine-line-update)
  (add-hook 'window-state-change-hook #'cocaine-line-update)
  (add-hook 'window-size-change-functions (lambda (_) (cocaine-line-invalidate-fill-cache)))
  (add-hook 'after-load-theme-hook #'cocaine-line-update-inactive-face))

(defun cocaine-line-remove-hooks ()
  "Remove hooks to update the mode-line."
  (remove-hook 'post-command-hook #'cocaine-line-update)
  (remove-hook 'window-configuration-change-hook #'cocaine-line-update)
  (remove-hook 'focus-in-hook #'cocaine-line-update)
  (remove-hook 'focus-out-hook #'cocaine-line-update)
  (remove-hook 'window-buffer-change-functions #'cocaine-line-update)
  (remove-hook 'window-state-change-hook #'cocaine-line-update)
  (remove-hook 'window-size-change-functions (lambda (_) (cocaine-line-invalidate-fill-cache)))
  (remove-hook 'after-load-theme-hook #'cocaine-line-update-inactive-face))

(define-minor-mode cocaine-line-mode
  "Activate Cocaine Line mode."
  :group 'cocaine-line
  :global t
  :lighter nil
  (if cocaine-line-mode
      (progn
        (cocaine-line-set-mode-line)
        (cocaine-line-register-hooks)
        (cocaine-line-update-inactive-face)
        (cocaine-line-update))
    (setq-default mode-line-format (default-value 'mode-line-format))
    (cocaine-line-remove-hooks)
    (force-mode-line-update t)))

(provide 'cocaine-line)
;;; cocaine-line.el ends here
