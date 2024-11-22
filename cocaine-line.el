;;; cocaine-line.el --- A customized modeline for Emacs with Evil status and advanced customizations

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

(unless (bound-and-true-p battery-status-function)
  (battery-update-handler))

(defvar-local cocaine-line-is-active t
  "Indicates if the current window is active.")

(defgroup cocaine-line nil
  "Customizations for cocaine-line."
  :group 'mode-line)

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

;; Git faces customization
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

;; Evil mode faces customization
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

(defcustom cocaine-line-separator " | "
  "Separator used between sections in the modeline."
  :type 'string
  :group 'cocaine-line)

(defcustom cocaine-line-separator-face 'cocaine-line-separator-face
  "Face for the separator between sections."
  :type 'face
  :group 'cocaine-line)

(defface cocaine-line-separator-face
  '((t :foreground "#54536D" :weight bold :height 0.8))
  "Face for the separator between sections in cocaine-line."
  :group 'cocaine-line)

(cl-defun cocaine-add-separator (&key str separator leftside (last nil)
                                      (face 'cocaine-line-separator-face))
  "Add a separator after STR if it is not empty or last.
    LAST indicates if this is the last element.
    FACE specifies which face to use for the separator."
  (if (and str (not (string-empty-p str)) (not last))
      (if leftside
          (progn
            (if separator
                (concat str (propertize separator 'face face))
              (concat str (propertize cocaine-line-separator 'face face))))
        (progn
          (if separator
              (concat (propertize separator 'face face) str)
            (concat (propertize cocaine-line-separator 'face face) str))))

    str))

;; Evil status function
(defun cocaine-evil-status ()
  "Show Evil status with custom face and correct vertical alignment."
  (let* ((evil-state (if (and (bound-and-true-p evil-local-mode)
                              (boundp 'evil-state))
                         evil-state
                       'emacs))
         (state-face (or (cdr (assq evil-state cocaine-evil-faces))
                         'cocaine-line-evil-emacs-face))
         (state-name (if (eq evil-state 'emacs)
                         "EMACS"
                       (upcase (symbol-name evil-state)))))
    (propertize (format " %s " state-name)
                'face state-face)))

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

(defun cocaine-process-info ()
  "Show information about active processes."
  (when cocaine-show-processes-info
   (let ((process-info (format-mode-line mode-line-process)))
     (unless (string-blank-p process-info)
       (string-trim process-info)))))

(defun cocaine-misc-info ()
  "Show information about misc info."
  (when cocaine-show-misc-info
    (let ((misc-info (format-mode-line mode-line-misc-info)))
      (unless (string-blank-p misc-info)
        (string-trim misc-info)))))

(defun cocaine-vc--rev (file backend)
  "Get the revision for FILE in BACKEND."
  (when-let ((rev (vc-working-revision file backend)))
    (substring rev 0 (min (length rev) 7))))

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

(defun cocaine-git-info ()
  "Show Git branch and status with custom faces."
  (when (and cocaine-show-git-info
             (buffer-file-name)
             (vc-git-registered (buffer-file-name)))
    (let* ((branch (vc-git-mode-line-string (buffer-file-name)))
           (state (vc-state (buffer-file-name)))
           (state-face (alist-get state cocaine-git-faces
                                  (alist-get 'default cocaine-git-faces)))
           (status-indicator (if (eq state 'up-to-date) "" "")))
      (when branch
        (propertize (format "%s %s%s"
                            (nerd-icons-octicon "nf-oct-git_branch")
                            (replace-regexp-in-string "^Git[:-]" "" branch)
                            status-indicator)
                    'face state-face)))))

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

;; Custom functions for left section
(defun cocaine-buffer-name ()
  "Show buffer name with custom face and icon (if available)."
  (let* ((file-name (buffer-file-name))
         (icon (when (and cocaine-show-use-nerd-icons file-name)
                 (nerd-icons-icon-for-file file-name t)))
         (buffer-name (file-name-sans-extension
                       (substring-no-properties (format-mode-line "%b "))))
         (face (if (buffer-modified-p)
                   'cocaine-modeline-buffer-modified
                 'doom-modeline-buffer-file)))
    (if icon
        (concat icon " " (propertize buffer-name 'face face))
      (propertize buffer-name 'face 'doom-modeline-buffer-file))))

(defun cocaine-major-mode ()
  "Show major mode with custom face."
  (propertize (substring-no-properties (format-mode-line mode-name))
              'face 'doom-modeline-buffer-file))

(defun cocaine-line-col ()
  "Show line and column with custom face."
  (when cocaine-show-column-info
    (propertize "[%l:%c]" 'face 'cocaine-line-position-face)))

(defun cocaine-buffer-position ()
  "Show buffer position percentage with custom face."
  (when cocaine-show-buffer-position
    (propertize "%p%" 'face 'cocaine-line-position-face)))

(defun cocaine-copilot-info ()
  "HUD for Copilot."
  (when (bound-and-true-p copilot-mode)
    (propertize " " 'face '(:inherit success))))

(defun cocaine-line-spacer ()
  "Show an empty string."
  (propertize " "))

(defun cocaine-time ()
  "Show time with custom face."
  (propertize (format-time-string "%H:%M") 'face 'doom-modeline-time))

(defun cocaine-battery-info ()
  "Show battery percentage or charging status using text and nerd-font icons on macOS."
  (when (and cocaine-show-battery-info
             (bound-and-true-p display-battery-mode))
    (let* ((battery-plist (funcall battery-status-function))
           (percentage (string-to-number (battery-format "%p" battery-plist)))
           (status (battery-format "%B" battery-plist))
           (charging (string= status "AC"))
           (icon (cond
                  (charging (nerd-icons-faicon "nf-fa-plug"))
                  ((>= percentage 87.5) (nerd-icons-faicon "nf-fa-battery"))
                  ((>= percentage 62.5) (nerd-icons-faicon "nf-fa-battery_3"))
                  ((>= percentage 37.5) (nerd-icons-faicon "nf-fa-battery_2"))
                  ((>= percentage 12.5) (nerd-icons-faicon "nf-fa-battery_1"))
                  (t (nerd-icons-faicon "nf-fa-battery_0")))))
      (if (and percentage status)
          (format "%s"
                  icon
                  (if charging " (Charging)" ""))
        "No battery info"))))

(defsubst salih/doom-modeline--bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and doom-modeline--bar-active doom-modeline--bar-inactive)
    (let ((width doom-modeline-bar-width)
          (height (max doom-modeline-height (doom-modeline--font-height))))
      (setq doom-modeline--bar-active
            (doom-modeline--create-bar-image 'doom-modeline-bar width height)
            doom-modeline--bar-inactive
            (doom-modeline--create-bar-image
             'doom-modeline-bar-inactive width height))))
  (if (doom-modeline--active)
      doom-modeline--bar-inactive
    doom-modeline--bar-inactive))

(defun cocaine-left-section ()
  "Create the left section of the modeline."
  (let ((left-section
         (list
          (concat
           (salih/doom-modeline--bar) ;; just to adjust height.
           ;; (propertize " "
           ;;                   'display '(raise 0.3)
           ;;                   'face 'mode-line)

           (cocaine-add-separator :str
                                  (doom-modeline-segment--modals)
                                  :leftside t
                                  :separator "| ")
           (cocaine-line-spacer)
           (cocaine-buffer-name)
           (cocaine-add-separator :str
                                  (cocaine-major-mode)
                                  :separator " | ")
           (cocaine-add-separator
            :str
            (if (derived-mode-p
                 'pdf-view-mode)
                (concat " " (salih/doom-modeline-update-pdf-pages-only-percent))
              (substring (format-mode-line (doom-modeline-segment--buffer-position)) 1)))

           (cocaine-add-separator :str (mode-line-segment-hud))
           (cocaine-add-separator :str (cocaine-process-info))))))


    left-section))
(defun cocaine-right-section ()
  "Create the right section of the modeline."
  (let ((right-section (concat
                        (if (derived-mode-p 'pdf-view-mode)
                            (cocaine-add-separator
                             :str (propertize (salih/doom-modeline-update-pdf-pages-no-percent)) :leftside t)
                            "")
                        ;; (cocaine-add-separator :str (doom-modeline-segment--salih/word-count) :leftside t)
                        (cocaine-add-separator :str (doom-modeline-segment--salih/selection-info) :leftside t)
                        (cocaine-add-separator :str (doom-modeline-segment--matches) :leftside t)
                        (cocaine-add-separator :str (doom-modeline-segment--irc) :leftside t)
                        (cocaine-add-separator :str (and (boundp 'mu4e-alert-mode-line) (or mu4e-alert-mode-line ""))  :leftside t)
                        (cocaine-add-separator :str (doom-modeline-segment--lsp) :leftside t)
                        (cocaine-add-separator :str (doom-modeline-segment--minor-modes) :leftside t)
                        (cocaine-add-separator :str (cocaine-flycheck-mode-line) :leftside t)
                        (cocaine-add-separator :str (doom-modeline-segment--vcs) :leftside t)
                        (cocaine-time)
                        (cocaine-add-separator :str awqat-mode-line-string :leftside nil))))


    (list (propertize " " 'display `((space :align-to (- right ,(string-width right-section)))))
          right-section)))

(defun cocaine-mode-line-format ()
  "Generate the format for cocaine-line modeline."
  (let ((left (cocaine-left-section))
        (right (cocaine-right-section)))
    (append left right)))

(defun cocaine-update-mode-line (&optional _)
  "Update modeline for all windows."
  (force-mode-line-update t))

(defun cocaine-set-mode-line ()
  "Set the modeline format for cocaine-line."
  (setq-default mode-line-format '(:eval (cocaine-mode-line-format))))

(defun cocaine-register-hooks ()
  "Register hooks to update the modeline."
  (add-hook 'post-command-hook #'cocaine-update-mode-line)
  (add-hook 'window-configuration-change-hook #'cocaine-update-mode-line)
  (add-hook 'focus-in-hook #'cocaine-update-mode-line)
  (add-hook 'focus-out-hook #'cocaine-update-mode-line)
  (add-hook 'window-state-change-hook #'cocaine-update-mode-line)  ; Add this hook
  (add-hook 'after-load-theme-hook #'cocaine-update-mode-line))

(defun cocaine-remove-hooks ()
  "Remove hooks to update the modeline."
  (remove-hook 'post-command-hook #'cocaine-update-mode-line)
  (remove-hook 'window-configuration-change-hook #'cocaine-update-mode-line)
  (remove-hook 'focus-in-hook #'cocaine-update-mode-line)
  (remove-hook 'focus-out-hook #'cocaine-update-mode-line)
  (remove-hook 'window-state-change-hook #'cocaine-update-mode-line)  ; Remove this hook
  (remove-hook 'after-load-theme-hook #'cocaine-update-mode-line))

(define-minor-mode cocaine-line-mode
  "Activate Cocaine Line mode."
  :group 'cocaine-line
  :global t
  :lighter nil
  (if cocaine-line-mode
      (progn
        (cocaine-set-mode-line)
        (cocaine-register-hooks)
        (cocaine-update-mode-line))
    (setq-default mode-line-format (default-value 'mode-line-format))
    (cocaine-remove-hooks)
    (force-mode-line-update t)))



(provide 'cocaine-line)
;;; cocaine-line.el ends here
