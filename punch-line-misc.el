;;; punch-line-misc.el --- A customized mode-line for Emacs with Evil status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with Evil status,
;; configurable colors, and the ability to customize displayed information.
;;; Code:

(require 'eglot)
(require 'flycheck)
(require 'nerd-icons)
(require 'project)
(require 'punch-line-colors)

(when (featurep 'projectile)
  (require 'projectile))

(defgroup punch-line nil
  "Customization group for punch-line."
  :group 'convenience)

(defcustom punch-show-processes-info t
  "If set to t, show active processes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-misc-info nil
  "If set to t, show misc processes."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-copilot-info t
  "If set to t, show copilot icon."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-show-time-info t
  "If set to t, show time."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-column-info nil
  "If set to t, show column information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-project-info t
  "If set to t, show project information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-lsp-info t
  "If set to t, show LSP information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-buffer-position nil
  "If set to t, show buffer position."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-flycheck-info t
  "If set to t, show flycheck information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-show-what-am-i-doing-info t
  "If set to t, show what-am-i-doing information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-task-file (expand-file-name "~/.emacs.d/current-task.el")
  "File to store the current task."
  :type 'file
  :group 'punch-line)

(defcustom punch-line-task-file (expand-file-name "~/.emacs.d/current-task.el")
  "File to store the current task."
  :type 'file
  :group 'punch-line)

(defcustom punch-line-what-am-i-doing nil
  "Stores the current task or activity."
  :type 'string
  :group 'punch-line)

(defun punch-load-task ()
  "Load the saved task explicitly."
  (interactive)
  (when (file-exists-p punch-line-task-file)
    (with-temp-buffer
      (insert-file-contents punch-line-task-file)
      (setq punch-line-what-am-i-doing (read (current-buffer))))))

(defun punch-line-what-am-i-doing (task)
  "Set the current task or activity and save it."
  (interactive "sWhat are you working on? ")
  (customize-set-variable 'punch-line-what-am-i-doing task))

(defun punch-flycheck-mode-line ()
  "Custom flycheck mode-line with icons and counts."
  (when (and (bound-and-true-p flycheck-mode)
             punch-show-flycheck-info
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (let* ((count (flycheck-count-errors flycheck-current-errors))
           (info (or (cdr (assq 'info count)) 0))
           (warning (or (cdr (assq 'warning count)) 0))
           (error (or (cdr (assq 'error count)) 0)))
      (concat
       (when (> info 0)
         (propertize (format "%s %d" (nerd-icons-codicon "nf-cod-lightbulb") info)
                     'face '(:inherit success)))
       (when (> warning 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-warning") warning)
                     'face '(:inherit warning)))
       (when (> error 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-error") error)
                     'face '(:inherit error)))))))

(defun punch-process-info ()
  "Show information about active processes."
  (when punch-show-processes-info
    (let ((process-info (concat " " (format-mode-line mode-line-process))))
        process-info)))

(defun punch-misc-info ()
  "Show information about misc info."
  (when punch-show-misc-info
    (let ((misc-info (format-mode-line mode-line-misc-info)))
      (unless (string-blank-p misc-info)
        (string-trim misc-info)))))

(defun punch-lsp-info ()
  "Return current LSP (Eglot or lsp-mode) status for the mode line using nerd-icons."
  (when punch-show-lsp-info
    (cond
     ((bound-and-true-p eglot--managed-mode)
      (let* ((server (eglot-current-server))
             (icon (propertize (nerd-icons-codicon "nf-cod-pulse")
                               'face 'punch-line-lsp-icon-face)))
        (if server
            icon
          "")))
     ((bound-and-true-p lsp-mode)
      (let ((icon (propertize (nerd-icons-codicon "nf-cod-pulse")
                              'face 'punch-line-lsp-icon-face)))
        (if (lsp-workspaces)
            icon
          "")))
     (t ""))))

(defun punch-project-info ()
  "Show project information."
  (cond
   ((and punch-show-project-info punch-show-lsp-info)
    (let ((project (punch--project-name))
          (lsp (or (punch-lsp-info) "")))
      (string-trim (concat project " " lsp))))
   (punch-show-project-info
    (punch--project-name))
   (punch-show-lsp-info
    (or (punch-lsp-info) ""))
   (t "")))

(defun punch-project-name ()
  "Get the project name if any."
  (or
   (and (fboundp 'project-name)
        (project-current)
        (project-name (project-current)))
   (and (fboundp 'projectile-project-name)
        (projectile-project-name))
   ""))

(defun punch--project-name ()
  "Show project information."
  (let ((project (punch-project-name)))
    (if (string-empty-p project)
        ""
      (propertize project 'face 'punch-line-project-face))))

;; Custom functions for left section
(defun punch-buffer-name ()
  "Show buffer name with custom face and icon (if available)."
  (let* ((file-name (buffer-file-name))
         (icon (when file-name
                 (nerd-icons-icon-for-file file-name t)))
         (buffer-name (file-name-sans-extension
                       (substring-no-properties (format-mode-line "%b ")))))
    (if icon
        (concat icon " " (propertize buffer-name 'face 'punch-line-buffer-name-face))
      (propertize buffer-name 'face 'punch-line-buffer-name-face))))

(defun punch-major-mode ()
  "Show major mode with custom face."
  (propertize (substring-no-properties (format-mode-line mode-name))
              'face 'punch-line-major-mode-face))

(defun punch-line-col ()
  "Show line and column with custom face."
  (when punch-show-column-info
    (propertize "[%l:%c]" 'face 'punch-line-position-face)))

(defun punch-buffer-position ()
  "Show buffer position percentage with custom face."
  (when punch-show-buffer-position
    (propertize "%p%" 'face 'punch-line-position-face)))

(defun punch-copilot-info ()
  "HUD for Copilot."
  (when (and punch-show-copilot-info (bound-and-true-p copilot-mode))
    (propertize " " 'face '(:inherit success))))

(defun punch-line-spacer ()
  "Show an empty string."
  " ")

(defun punch-time-info ()
  "Show time with custom face."
  (when punch-line-show-time-info
    (propertize (format-time-string "%H:%M ") 'face 'punch-line-time-face)))

(defun punch-what-am-i-doing-info ()
  "Show what-am-i-doing information with custom face."
  (when (and punch-show-what-am-i-doing-info punch-line-what-am-i-doing)
    (concat "Doing: "
        (propertize punch-line-what-am-i-doing
                    'face 'punch-line-what-am-i-doing-face))))

(provide 'punch-line-misc)
;;; punch-line-misc.el ends here
