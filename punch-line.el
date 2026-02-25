;;; punch-line.el --- A customizeable mode-line with Evil  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (async "1.9"))
;; URL: https://github.com/konrad1977/punch-line
;; Keywords: mode-line, faces

;;; Commentary:

;; This package offers a customized mode-line for Emacs.
;; Configurable colors, and the ability to customize displayed information.

;;; Code:
(require 'cl-lib)

(require 'punch-line-colors)
(require 'punch-line-vc)
(require 'punch-line-macro)
(require 'punch-line-music)
(require 'punch-line-modal)
(require 'punch-line-battery)
(require 'punch-line-misc)
(require 'punch-line-weather)
(require 'punch-line-term)
(require 'punch-line-systemmonitor)
(require 'punch-line-package)
(require 'punch-line-what-am-i-doing)

(require 'mode-line-hud)

(defgroup punch-line nil
  "Customizations for punch-line."
  :group 'mode-line)

(defcustom punch-line-section-backgrounds nil
  "Control section background tinting in the mode-line.
nil      - No section backgrounds.
t / auto - Automatic tinting based on mode-line background color.
alist    - Manual per-section colors, e.g. ((filename . \"#201010\"))."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Automatic tinting" t)
                 (const :tag "Automatic tinting (alias)" auto)
                 (alist :tag "Manual colors"
                        :key-type (symbol :tag "Section")
                        :value-type (string :tag "Color")))
  :group 'punch-line)

(defcustom punch-line-section-background-tint-start -20
  "Starting tint percentage for the outermost sections when using automatic backgrounds.
Negative values darken, positive values lighten.  For example, -20 means
the outermost sections are 20%% darker than the mode-line background."
  :type 'number
  :group 'punch-line)

(defcustom punch-line-section-background-tint-step 50
  "Percentage by which each inner section reduces the tint toward zero.
For example, with start=-20 and step=50, sections will be:
-20%%, -10%%, -5%%, -2.5%%, etc. (each step halves the tint)."
  :type 'number
  :group 'punch-line)

(defcustom punch-line-section-padding 1
  "Number of space characters to add on each side of a section with background.
A value of 1 adds one space before and one space after the section text.
Typical values are 0-3; larger values will make sections very wide."
  :type 'integer
  :group 'punch-line)

(defvar punch-line--update-timer nil
  "Timer for debouncing mode-line updates.")

(defvar punch-line--last-update 0
  "Timestamp of last mode-line update.")

(defcustom punch-line-min-update-interval 0.1
  "Minimum interval between mode-line updates in seconds."
  :type 'number
  :group 'punch-line)

(defvar punch-line-is-active nil)

(defvar-local punch-line--cached-fill nil
  "Cached fill for the mode-line.")

(defvar-local punch-line--cached-right-width nil
  "Cached width of the right section of the mode-line.")

(defvar punch-line-active-window nil
  "Stores the currently active window.")

(defun punch-line-window-active-p ()
  "Return non-nil if the current window is active."
  (let ((current (get-buffer-window))
        (active punch-line-active-window))
    (eq current active)))

(defcustom punch-line-left-separator "  "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'punch-line)

(defcustom punch-line-right-separator "  "
  "Separator used between sections in the mode-line."
  :type 'string
  :group 'punch-line)

(defun punch-line-inactive-bg ()
  "Get the background color of the mode-line-inactive face."
  (face-background 'mode-line-inactive nil t))

(defun punch-line-update-inactive-face ()
  "Update the punch-line-inactive-face with the current theme colors."
  (let* ((bg-color (or (face-background 'mode-line-inactive nil t)
                       (face-background 'mode-line nil t)
                       (face-background 'default nil t)
                       "#000000"))
         (fg-color (or (face-foreground 'mode-line-inactive nil t)
                       (face-foreground 'default nil t)
                       "#ffffff"))
         (height-adjust (max 1 (/ (punch-line-modal-height) 2))))
    (set-face-attribute 'punch-line-inactive-face nil
                        :background bg-color
                        :foreground fg-color
                        :box `(:line-width ,height-adjust :color ,bg-color)
                        :underline nil)))

(defun punch-line-calculate-section-background (section-name section-index &optional is-left-side)
  "Calculate background color for SECTION-NAME at SECTION-INDEX.
IS-LEFT-SIDE determines tinting direction. Returns a color string or nil."
  (cond
   ((and (listp punch-line-section-backgrounds)
         (assoc section-name punch-line-section-backgrounds))
    (cdr (assoc section-name punch-line-section-backgrounds)))
   ((memq punch-line-section-backgrounds '(auto t))
    (let* ((base-bg (or (face-background 'mode-line nil t)
                        (face-background 'default nil t)
                        "#000000"))
           ;; Calculate cumulative tint percentage
           (tint-amount (punch-line-calculate-cumulative-tint section-index is-left-side)))
      (adjust-color base-bg tint-amount)))
   (t nil)))

(defun punch-line-calculate-cumulative-tint (section-index _is-left-side)
  "Calculate cumulative tint percentage for SECTION-INDEX.
The outermost section (index 0) uses `punch-line-section-background-tint-start'
directly.  Each subsequent section moves the tint toward zero by
`punch-line-section-background-tint-step' percent."
  (let ((current-tint (float punch-line-section-background-tint-start)))
    (dotimes (_ section-index)
      (setq current-tint
            (* current-tint (- 1.0 (/ punch-line-section-background-tint-step 100.0)))))
    current-tint))

(defun punch-line-wrap-with-background (str section-name section-index &optional is-left-side)
  "Wrap STR with background color for SECTION-NAME at SECTION-INDEX.
IS-LEFT-SIDE determines tinting direction.
Edge sections (modal, time) also get :box for consistent mode-line height."
  (if (or (not str) (string-empty-p str))
      str
    (let ((bg (punch-line-calculate-section-background section-name section-index is-left-side)))
      (if bg
          (let* ((padding (make-string punch-line-section-padding ?\s))
                 (padded-str (concat padding str padding))
                 (result (copy-sequence padded-str))
                 (is-edge (memq section-name '(modal time)))
                 (height-adjust (max 1 (/ (punch-line-modal-height) 2)))
                 (face-spec (if is-edge
                                `(:background ,bg
                                  :box (:line-width ,height-adjust :color ,bg))
                              `(:background ,bg))))
            (add-face-text-property 0 (length result) face-spec nil result)
            result)
        str))))

(defun punch-line-wrap-with-background-visible (str section-name visible-sections is-left-side)
  "Wrap STR with background color based on visible sections only."
  (if (or (not str) (string-empty-p str))
      str
    (let* ((section-names (mapcar #'car visible-sections))
           (visible-index (cl-position section-name section-names)))
      (if visible-index
          (punch-line-wrap-with-background str section-name visible-index is-left-side)
        str))))

(defun punch-line-maybe-wrap-background (str section-name visible-sections is-left-side)
  "Conditionally wrap STR with background if backgrounds are enabled."
  (if punch-line-section-backgrounds
      (punch-line-wrap-with-background-visible str section-name visible-sections is-left-side)
    str))

(defun punch-line-get-section-background (section-name visible-sections is-left-side)
  "Get the background color for a section without wrapping the string."
  (if punch-line-section-backgrounds
      (let* ((section-names (mapcar #'car visible-sections))
             (visible-index (cl-position section-name section-names)))
        (if visible-index
            (punch-line-calculate-section-background section-name visible-index is-left-side)
          nil))
    nil))

(cl-defun punch-line-add-separator (&key str separator leftside (last nil) (face 'punch-line-separator-face) background)
  "Add a (SEPARATOR) around STR based on the arguments.
Add a separator after STR if it is not empty or last.  LAST
indicates if this is the last element.  FACE specifies which face
to use for the separator. BACKGROUND applies background color to separator."
  (if (and str (not (string-empty-p str)) (not last))
      (if (not separator)
          str
        (let* ((height (punch-line-get-divider-icon-height))
               (divider-face (if background
                               `(:inherit ,face :height ,height :background ,background)
                             `(:inherit ,face :height ,height)))
               (divider (propertize separator 'face divider-face)))
          (if leftside
              (concat str divider)
            (concat divider str))))
    str))

(defun punch-line-format-left ()
  "Create the left section of the mode-line with caching."
   (if punch-line-section-backgrounds
      ;; Background mode - use visible sections logic
      (let* ((modal-str (punch-evil-status))
             (sections (list
                        (cons 'modal modal-str)
                        (cons 'filename (punch-buffer-name))
                        (cons 'major-mode (punch-major-mode))
                        (cons 'project (punch-project-info))
                        (cons 'flycheck (punch-flycheck-info))
                        (cons 'what-am-i-doing (punch-what-am-i-doing-info))
                        (cons 'process (punch-process-info))))
             (visible-sections (cl-remove-if (lambda (section) 
                                               (or (not (cdr section))
                                                   (string-empty-p (cdr section))))
                                             sections)))
        (list (concat
               (punch-macro-info)
               (punch-iedit-info)
               (punch-evil-mc-info)
               (punch-line-wrap-with-background-visible modal-str 'modal visible-sections t)
               (punch-line-wrap-with-background-visible (punch-buffer-name) 'filename visible-sections t)
               (punch-line-add-separator 
                :str (punch-line-wrap-with-background-visible (punch-major-mode) 'major-mode visible-sections t) 
                :separator ""
                :background (punch-line-get-section-background 'major-mode visible-sections t))
               (punch-line-add-separator 
                :str (punch-line-wrap-with-background-visible (punch-project-info) 'project visible-sections t) 
                :separator punch-line-left-separator
                :background (punch-line-get-section-background 'project visible-sections t))
               (punch-line-add-separator 
                :str (punch-line-wrap-with-background-visible (punch-flycheck-info) 'flycheck visible-sections t) 
                :separator punch-line-left-separator
                :background (punch-line-get-section-background 'flycheck visible-sections t))
               (punch-line-add-separator 
                :str (punch-line-wrap-with-background-visible (punch-what-am-i-doing-info) 'what-am-i-doing visible-sections t) 
                :separator punch-line-left-separator
                :background (punch-line-get-section-background 'what-am-i-doing visible-sections t))
               (punch-line-wrap-with-background-visible (punch-process-info) 'process visible-sections t)
               (mode-line-segment-hud))))
    ;; No backgrounds - use original simple format
    (list (concat
           (punch-macro-info)
           (punch-iedit-info)
           (punch-evil-mc-info)
           (punch-evil-status)
           (punch-buffer-name)
           (punch-line-add-separator :str (punch-major-mode) :separator "|")
           (punch-line-add-separator :str (punch-project-info) :separator punch-line-left-separator)
           (punch-line-add-separator :str (punch-flycheck-info) :separator punch-line-left-separator)
           (punch-line-add-separator :str (punch-what-am-i-doing-info) :separator punch-line-left-separator)
           (punch-process-info)
           (mode-line-segment-hud)))))

(defun punch-line-format-right ()
  "Create the right section of the mode-line with caching."
   (if punch-line-section-backgrounds
      ;; Background mode - use visible sections logic
      (let* ((time-str (punch-time-info))
             (sections (list
                       (cons 'music (punch-line-music-info))
                       (cons 'system-monitor (punch-system-monitor-info))
                       (cons 'column (punch-line-col))
                       (cons 'position (punch-buffer-position))
                       (cons 'copilot (punch-copilot-info))
                       (cons 'term (punch-term-info))
                       (cons 'misc (punch-misc-info))
                       (cons 'git (punch-git-info))
                       (cons 'weather (punch-weather-info))
                       (cons 'battery (punch-battery-info))
                       (cons 'time time-str)))
             (visible-sections (cl-remove-if (lambda (section) 
                                               (or (not (cdr section))
                                                   (string-empty-p (cdr section))))
                                             sections))
             ;; Reverse the visible sections for right-side tinting
             (reversed-sections (reverse visible-sections)))
        (concat
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-line-music-info) 'music reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'music reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-system-monitor-info) 'system-monitor reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'system-monitor reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-line-col) 'column reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'column reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-buffer-position) 'position reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'position reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-copilot-info) 'copilot reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'copilot reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-term-info) 'term reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'term reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-misc-info) 'misc reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'misc reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-git-info) 'git reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'git reversed-sections nil))
         (punch-line-add-separator 
          :str (punch-line-wrap-with-background-visible (punch-weather-info) 'weather reversed-sections nil) 
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'weather reversed-sections nil))
         (punch-line-add-separator
          :str (punch-line-wrap-with-background-visible (punch-battery-info) 'battery reversed-sections nil)
          :separator punch-line-right-separator :leftside t
          :background (punch-line-get-section-background 'battery reversed-sections nil))
         (punch-line-wrap-with-background-visible time-str 'time reversed-sections nil)))
    ;; No backgrounds - use original simple format
    (concat
     (punch-line-add-separator :str (punch-line-music-info) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-system-monitor-info) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-line-col) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-buffer-position) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-copilot-info) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-term-info) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-misc-info) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-git-info) :separator punch-line-right-separator :leftside t)
     (punch-line-add-separator :str (punch-weather-info) :separator punch-line-right-separator :leftside t)
     (punch-battery-info)
     (punch-time-info))))

(defun punch-line-format-inactive ()
  "Inactive format with buffer name in gray, matching active mode-line height."
  (let* ((file-name (buffer-file-name))
         (height-adjust (max 1 (/ (punch-line-modal-height) 2)))
         (bg-color (or (face-background 'mode-line-inactive nil t)
                       (face-background 'mode-line nil t)
                       (face-background 'default nil t)
                       "#000000"))
         (base-face `(:inherit punch-line-inactive-face
                               :box (:line-width ,height-adjust :color ,bg-color)))
         (icon (when file-name
                 (propertize (nerd-icons-icon-for-file file-name)
                             'face base-face)))
         (buffer-name (file-name-sans-extension
                       (substring-no-properties (format-mode-line "%b")))))
    (if icon
        (concat " " icon " " (propertize buffer-name 'face base-face) " ")
      (propertize (concat " " buffer-name " ") 'face base-face))))

(defun punch-line-format ()
  "Generate the mode-line format."
  (if (punch-line-window-active-p)
      (list (punch-line-format-left)
            (punch-line-get-fill)
            (punch-line-format-right))
    (punch-line-format-inactive)))

(defun punch-line-update (&optional force)
  "Update mode-line for all windows.
If FORCE is non-nil, bypass the update interval check."
  (let ((current-time (float-time)))
    (when (or force
              (> (- current-time punch-line--last-update)
                 punch-line-min-update-interval))
      (when punch-line--update-timer
        (cancel-timer punch-line--update-timer))
      (setq punch-line--update-timer
            (run-with-idle-timer
             0.05 nil
             (lambda ()
               (setq punch-line--last-update current-time
                     punch-line-active-window (selected-window))
               (force-mode-line-update t)))))))

(defun punch-line-set-mode-line ()
  "Set the mode-line format for punch-line."
  (setq-default mode-line-format '(:eval (punch-line-format))))

(defun punch-line-register-hooks ()
  "Register hooks to update the mode-line."
  (add-hook 'post-command-hook #'punch-line-update)
  (add-hook 'window-configuration-change-hook #'punch-line-update)
  (add-hook 'focus-in-hook #'punch-line-update)
  (add-hook 'focus-out-hook #'punch-line-update)
  (add-hook 'window-buffer-change-functions #'punch-line-update)
  (add-hook 'window-state-change-hook #'punch-line-update)
  (add-hook 'window-size-change-functions (lambda (_) (punch-line-invalidate-fill-cache)))
  (add-hook 'after-load-theme-hook #'punch-line-update-inactive-face)
  (add-hook 'after-save-hook #'punch-git-invalidate-cache)
  (add-hook 'vc-mode-line-hook #'punch-git-invalidate-cache)
  ;; Invalidate git cache on branch switch
  (when (fboundp 'magit-post-checkout-hook)
    (add-hook 'magit-post-checkout-hook #'punch-git-invalidate-cache))
  (add-hook 'find-file-hook #'punch-git-invalidate-cache))

(defun punch-line-remove-hooks ()
  "Remove hooks to update the mode-line."
  (remove-hook 'post-command-hook #'punch-line-update)
  (remove-hook 'window-configuration-change-hook #'punch-line-update)
  (remove-hook 'focus-in-hook #'punch-line-update)
  (remove-hook 'focus-out-hook #'punch-line-update)
  (remove-hook 'window-buffer-change-functions #'punch-line-update)
  (remove-hook 'window-state-change-hook #'punch-line-update)
  (remove-hook 'window-size-change-functions (lambda (_) (punch-line-invalidate-fill-cache)))
  (remove-hook 'after-load-theme-hook #'punch-line-update-inactive-face)
  (remove-hook 'after-save-hook #'punch-git-invalidate-cache)
  (remove-hook 'vc-mode-line-hook #'punch-git-invalidate-cache)
  ;; Remove branch switch hooks
  (when (fboundp 'magit-post-checkout-hook)
    (remove-hook 'magit-post-checkout-hook #'punch-git-invalidate-cache))
  (remove-hook 'find-file-hook #'punch-git-invalidate-cache))

(define-minor-mode punch-line-mode
  "Activate Punch Line mode."
  :group 'punch-line
  :global t
  :lighter nil
  (if punch-line-mode
      (progn
        (punch-line-set-mode-line)
        (punch-line-register-hooks)
        (punch-line-update-inactive-face)
        (punch-line-update))
    (setq-default mode-line-format (default-value 'mode-line-format))
    (punch-line-remove-hooks)
    (force-mode-line-update t)))

(defun punch-line-calculate-fill (right-section)
  "Calculate the fill space needed to right-align the RIGHT-SECTION."
  (let ((right-width (string-width (or right-section ""))))
    (setq punch-line--cached-right-width right-width)
    (propertize " " 'display
                `((space :align-to (- right ,(- right-width 1)))))))

(defun punch-line-get-fill ()
  "Get the fill space needed to right-align content with caching."
  (let* ((right-section (punch-line-format-right))
         (current-width (string-width (or right-section ""))))
    (if (and punch-line--cached-fill
             punch-line--cached-right-width
             (= current-width punch-line--cached-right-width))
        punch-line--cached-fill
      (setq punch-line--cached-fill
            (punch-line-calculate-fill right-section)))))

(defun punch-line-invalidate-fill-cache ()
  "Invalidate the fill cache."
  (setq punch-line--cached-fill nil
        punch-line--cached-right-width nil))

(defun punch-line-invalidate-caches ()
  "Invalidate all caches."
  (punch-line-invalidate-fill-cache)
  (setq-local punch-git-info-cache nil
              punch-git-info-cache-time 0
              punch-git-file-name nil)
  (punch-git-invalidate-repo-cache))

(provide 'punch-line)
;;; punch-line.el ends here
