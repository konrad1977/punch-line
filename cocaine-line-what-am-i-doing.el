;;; cocaine-line-what-am-i-doing.el --- A customized mode-line for Emacs with task list management -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs with task list management,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'cl-lib)  ; Add required dependency
(require 'nerd-icons)  ; Add required dependency
(require 'cocaine-line-colors)  ; Add required dependency

(defcustom cocaine-show-what-am-i-doing-info t
  "If set to t, show what-am-i-doing information."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-line-task-file (expand-file-name "~/.emacs.d/task-list.el")
  "File to store the task list."
  :type 'file
  :group 'cocaine-line)

(defvar cocaine-line-task-list nil
  "List of tasks.")

(defvar cocaine-line-current-task-index 0
  "Index of the current task in the task list.")

(defvar cocaine-what-am-i-doing-info-cache nil
  "Cache for what-am-i-doing information.")

(defvar cocaine-what-am-i-doing-info-cache-valid nil
  "Flag indicating if the cache is valid.")

;;;###autoload
(defun cocaine-load-tasks ()
  "Load the saved tasks explicitly."
  (interactive)
  (if (file-exists-p cocaine-line-task-file)
      (condition-case err
          (progn
            (with-temp-buffer
              (insert-file-contents cocaine-line-task-file)
              (setq cocaine-line-task-list (read (current-buffer)))
              (setq cocaine-line-current-task-index
                    (min (max 0 cocaine-line-current-task-index)
                         (max 0 (1- (length cocaine-line-task-list)))))))
        (error
         (message "Error loading tasks: %s" (error-message-string err))
         (setq cocaine-line-task-list nil
               cocaine-line-current-task-index 0)))
    (message "No task file found at %s" cocaine-line-task-file)))

(defun cocaine-save-tasks ()
  "Save the current tasks to file."
  (with-temp-file cocaine-line-task-file
    (prin1 cocaine-line-task-list (current-buffer)))
  (message "Saved %d tasks" (length cocaine-line-task-list)))

(defun cocaine-line-what-am-i-doing-next (task)
  "Add a new task to the end of the list."
  (interactive "sWhat's your next task? ")
  (if (string-empty-p (string-trim task))
      (message "Task cannot be empty.")
    (progn
      (setq cocaine-line-task-list (append cocaine-line-task-list (list task)))
      (cocaine-invalidate-what-am-i-doing-cache)
      (cocaine-save-tasks))))

(defun cocaine-line-what-am-i-doing-done ()
  "Mark the current task as done and remove it from the list."
  (interactive)
  (when cocaine-line-task-list
    (let ((completed-task (nth cocaine-line-current-task-index cocaine-line-task-list)))
      (setq cocaine-line-task-list (delete completed-task cocaine-line-task-list))
      (when (>= cocaine-line-current-task-index (length cocaine-line-task-list))
        (setq cocaine-line-current-task-index (max 0 (1- (length cocaine-line-task-list)))))
      (cocaine-invalidate-what-am-i-doing-cache)
      (cocaine-save-tasks)
      (message "Task completed: %s" completed-task))))

(defun cocaine-line-what-am-i-doing-show-all ()
  "Display all tasks in the task list."
  (interactive)
  (if cocaine-line-task-list
      (message "All tasks:\n%s"
               (mapconcat 'identity
                          (cl-loop for task in cocaine-line-task-list
                                   for i from 0
                                   collect (format "%d. %s%s"
                                                   (1+ i)
                                                   task
                                                   (if (= i cocaine-line-current-task-index) " (current)" "")))
                          "\n"))
    (message "No tasks found.")))

(defun cocaine-line-what-am-i-doing-next-task ()
  "Move to the next task in the list."
  (interactive)
  (when cocaine-line-task-list
    (setq cocaine-line-current-task-index
          (mod (1+ cocaine-line-current-task-index) (length cocaine-line-task-list)))

    (cocaine-invalidate-what-am-i-doing-cache)
    (message "Current task: %s" (nth cocaine-line-current-task-index cocaine-line-task-list))))

(defun cocaine-line-what-am-i-doing-previous-task ()
  "Move to the previous task in the list."
  (interactive)
  (when cocaine-line-task-list
    (setq cocaine-line-current-task-index
          (mod (1- cocaine-line-current-task-index) (length cocaine-line-task-list)))

    (cocaine-invalidate-what-am-i-doing-cache)
    (message "Current task: %s" (nth cocaine-line-current-task-index cocaine-line-task-list))))

(defun cocaine-create-what-am-i-doing-info ()
  "Create the what-am-i-doing information string."
  (when (and cocaine-show-what-am-i-doing-info
             cocaine-line-task-list
             (> (length cocaine-line-task-list) 0))
    (let* ((current-task (nth cocaine-line-current-task-index cocaine-line-task-list))
           (task-count (length cocaine-line-task-list))
           (count-info (if (> task-count 1)
                          (propertize (format " (%d/%d)"
                                            (1+ cocaine-line-current-task-index)
                                            task-count)
                                    'face 'cocaine-line-what-am-i-doing-count-face)
                        "")))
      (concat (propertize (nerd-icons-faicon "nf-fa-gripfire") 'face 'mode-line-highlight)
          " "
          (propertize current-task 'face 'cocaine-line-what-am-i-doing-face)
          count-info))))

(defun cocaine-what-am-i-doing-info ()
  "Show what-am-i-doing information with custom face and caching."
  (if (and cocaine-what-am-i-doing-info-cache-valid
           cocaine-what-am-i-doing-info-cache)
      cocaine-what-am-i-doing-info-cache
    (setq cocaine-what-am-i-doing-info-cache (cocaine-create-what-am-i-doing-info)
          cocaine-what-am-i-doing-info-cache-valid t)
    cocaine-what-am-i-doing-info-cache))


(defun cocaine-invalidate-what-am-i-doing-cache ()
  "Invalidate the what-am-i-doing information cache."
  (setq cocaine-what-am-i-doing-info-cache-valid nil
        cocaine-what-am-i-doing-info-cache nil))


(provide 'cocaine-line-what-am-i-doing)
;;; cocaine-line-what-am-i-doing.el ends here 