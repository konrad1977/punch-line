;;; punch-line-what-am-i-doing.el --- A customized mode-line for Emacs with task list management -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with task list management,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'cl-lib)  ; Add required dependency
(require 'nerd-icons)  ; Add required dependency
(require 'punch-line-colors)  ; Add required dependency

(defcustom punch-show-what-am-i-doing-info t
  "If set to t, show what-am-i-doing information."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-line-task-file (expand-file-name "~/.emacs.d/task-list.el")
  "File to store the task list."
  :type 'file
  :group 'punch-line)

(defvar punch-line-task-list nil
  "List of tasks.")

(defvar punch-line-current-task-index 0
  "Index of the current task in the task list.")

(defvar punch-what-am-i-doing-info-cache nil
  "Cache for what-am-i-doing information.")

(defvar punch-what-am-i-doing-info-cache-valid nil
  "Flag indicating if the cache is valid.")

;;;###autoload
(defun punch-load-tasks ()
  "Load the saved tasks explicitly."
  (interactive)
  (if (file-exists-p punch-line-task-file)
      (condition-case err
          (progn
            (with-temp-buffer
              (insert-file-contents punch-line-task-file)
              (setq punch-line-task-list (read (current-buffer)))
              (setq punch-line-current-task-index
                    (min (max 0 punch-line-current-task-index)
                         (max 0 (1- (length punch-line-task-list)))))))
        (error
         (message "Error loading tasks: %s" (error-message-string err))
         (setq punch-line-task-list nil
               punch-line-current-task-index 0)))
    (message "No task file found at %s" punch-line-task-file)))

(defun punch-save-tasks ()
  "Save the current tasks to file."
  (with-temp-file punch-line-task-file
    (prin1 punch-line-task-list (current-buffer)))
  (message "Saved %d tasks" (length punch-line-task-list)))

(defun punch-line-what-am-i-doing-next (task)
  "Add a new task to the end of the list."
  (interactive "sWhat's your next task? ")
  (if (string-empty-p (string-trim task))
      (message "Task cannot be empty.")
    (progn
      (setq punch-line-task-list (append punch-line-task-list (list task)))
      (punch-invalidate-what-am-i-doing-cache)
      (punch-save-tasks))))

(defun punch-line-what-am-i-doing-done ()
  "Mark the current task as done and remove it from the list."
  (interactive)
  (when punch-line-task-list
    (let ((completed-task (nth punch-line-current-task-index punch-line-task-list)))
      (setq punch-line-task-list (delete completed-task punch-line-task-list))
      (when (>= punch-line-current-task-index (length punch-line-task-list))
        (setq punch-line-current-task-index (max 0 (1- (length punch-line-task-list)))))
      (punch-invalidate-what-am-i-doing-cache)
      (punch-save-tasks)
      (message "Task completed: %s" completed-task))))

(defun punch-line-what-am-i-doing-show-all ()
  "Display all tasks in the task list."
  (interactive)
  (if punch-line-task-list
      (message "All tasks:\n%s"
               (mapconcat 'identity
                          (cl-loop for task in punch-line-task-list
                                   for i from 0
                                   collect (format "%d. %s%s"
                                                   (1+ i)
                                                   task
                                                   (if (= i punch-line-current-task-index) " (current)" "")))
                          "\n"))
    (message "No tasks found.")))

(defun punch-line-what-am-i-doing-next-task ()
  "Move to the next task in the list."
  (interactive)
  (when punch-line-task-list
    (setq punch-line-current-task-index
          (mod (1+ punch-line-current-task-index) (length punch-line-task-list)))

    (punch-invalidate-what-am-i-doing-cache)
    (message "Current task: %s" (nth punch-line-current-task-index punch-line-task-list))))

(defun punch-line-what-am-i-doing-previous-task ()
  "Move to the previous task in the list."
  (interactive)
  (when punch-line-task-list
    (setq punch-line-current-task-index
          (mod (1- punch-line-current-task-index) (length punch-line-task-list)))

    (punch-invalidate-what-am-i-doing-cache)
    (message "Current task: %s" (nth punch-line-current-task-index punch-line-task-list))))

(defun punch-create-what-am-i-doing-info ()
  "Create the what-am-i-doing information string."
  (when (and punch-show-what-am-i-doing-info
             punch-line-task-list
             (> (length punch-line-task-list) 0))
    (let* ((current-task (nth punch-line-current-task-index punch-line-task-list))
           (task-count (length punch-line-task-list))
           (count-info (if (> task-count 1)
                          (propertize (format " (%d/%d)"
                                            (1+ punch-line-current-task-index)
                                            task-count)
                                    'face 'punch-line-what-am-i-doing-count-face)
                        "")))
      (concat (propertize (nerd-icons-faicon "nf-fa-gripfire") 'face 'mode-line-highlight)
          " "
          (propertize current-task 'face 'punch-line-what-am-i-doing-face)
          count-info))))

(defun punch-what-am-i-doing-info ()
  "Show what-am-i-doing information with custom face and caching."
  (if (and punch-what-am-i-doing-info-cache-valid
           punch-what-am-i-doing-info-cache)
      punch-what-am-i-doing-info-cache
    (setq punch-what-am-i-doing-info-cache (punch-create-what-am-i-doing-info)
          punch-what-am-i-doing-info-cache-valid t)
    punch-what-am-i-doing-info-cache))


(defun punch-invalidate-what-am-i-doing-cache ()
  "Invalidate the what-am-i-doing information cache."
  (setq punch-what-am-i-doing-info-cache-valid nil
        punch-what-am-i-doing-info-cache nil))


(provide 'punch-line-what-am-i-doing)
;;; punch-line-what-am-i-doing.el ends here
