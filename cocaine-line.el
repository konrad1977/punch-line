;;; cocaine-line.el --- En anpassad mode-line för Emacs med Evil-status och avancerade anpassningar

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0.0"))

;;; Commentary:
;; Detta paket erbjuder en anpassad mode-line för Emacs med Evil-status,
;; konfigurerbara färger, och möjlighet att anpassa visad information.

;;; Code:

(require 'cl-lib)
(require 'evil)
(require 'nerd-icons)
(require 'all-the-icons)
(require 'vc)
(require 'project)
(require 'mode-line-hud)

(defvar-local cocaine-line-is-active nil
  "Indikerar om det aktuella fönstret är aktivt.")

(defgroup cocaine-line nil
  "Anpassningar för cocaine-line."
  :group 'mode-line)

(defcustom cocaine-right-padding 0
  "Antal mellanslag att lägga till efter tiden."
  :type 'integer
  :group 'cocaine-line)

(defcustom cocaine-show-column-info nil
  "Om satt till t, visa kolumninformation."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-buffer-position nil
  "Om satt till t, visa kolumninformation."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-use-nerd-icons t
  "Om satt till t, visa filikoner med nerdicons."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-processes-info t
  "Om satt till t, visa aktiva processer."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-misc-info nil
  "Om satt till t, visa misc processer."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-git-info t
  "Om satt till t, visa Git-branch och status."
  :type 'boolean
  :group 'cocaine-line)

(defcustom cocaine-show-copilot-info t
  "Om satt till t, visa copilot ikon."
  :type 'boolean
  :group 'cocaine-line)

(defface cocaine-line-inactive-face
  '((t :inherit mode-line-inactive))
  "Face for inactive mode-line elements."
  :group 'cocaine-line)

(defface cocaine-line-buffer-name-face
  '((t :foreground "#a0a0ae" :weight bold))
  "Face för buffertnamn."
  :group 'cocaine-line)

(defface cocaine-line-major-mode-face
  '((t :foreground "#888899"))
  "Face för major mode."
  :group 'cocaine-line)

(defface cocaine-line-position-face
  '((t :foreground "#FFA07A"))
  "Face för buffertposition."
  :group 'cocaine-line)

(defface cocaine-line-time-face
  '((t :foreground "#888899":weight light))
  "Face för tidsvisning."
  :group 'cocaine-line)

(defface cocaine-line-git-face
  '((t :foreground "#a0a0ae"))
  "Standard face för Git-information."
  :group 'cocaine-line)

(defface cocaine-line-project-face
  '((t :foreground "#FFA066" :weight bold))
  "Standard face för project-information."
  :group 'cocaine-line)

(defface cocaine-line-eglot-icon-face
  '((t :inherit success))
  "Standard face för project-information."
  :group 'cocaine-line)

;; Uppdatera Git-färger till faces
(defcustom cocaine-git-faces
  '((edited . cocaine-line-git-edited-face)
    (added . cocaine-line-git-added-face)
    (removed . cocaine-line-git-removed-face)
    (conflict . cocaine-line-git-conflict-face)
    (default . cocaine-line-git-face))
  "Faces för olika Git-tillstånd."
  :type '(alist :key-type symbol :value-type face)
  :group 'cocaine-line)

(defface cocaine-line-git-edited-face
  '((t :foreground "#F1FA8C"))
  "Face för redigerade Git-filer."
  :group 'cocaine-line)

(defface cocaine-line-git-added-face
  '((t :foreground "#50FA7B"))
  "Face för tillagda Git-filer."
  :group 'cocaine-line)

(defface cocaine-line-git-removed-face
  '((t :foreground "#FF5555"))
  "Face för borttagna Git-filer."
  :group 'cocaine-line)

(defface cocaine-line-git-conflict-face
  '((t :foreground "#FF79C6"))
  "Face för Git-konflikter."
  :group 'cocaine-line)

;; Uppdatera Evil-färger till faces
(defcustom cocaine-evil-faces
  '((normal . cocaine-line-evil-normal-face)
    (insert . cocaine-line-evil-insert-face)
    (visual . cocaine-line-evil-visual-face)
    (replace . cocaine-line-evil-replace-face)
    (emacs . cocaine-line-evil-emacs-face))
  "Faces för olika Evil-lägen."
  :type '(alist :key-type symbol :value-type face)
  :group 'cocaine-line)

(defface cocaine-line-evil-normal-face
  '((t :foreground "#FFFFFF" :background "#2D4F67" :weight bold
       :box (:line-width 8 :color "#2D4F67")))
  "Face för Evil normal-läge."
  :group 'cocaine-line)

(defface cocaine-line-evil-insert-face
  '((t :foreground "#333333" :background "#E6C384" :weight bold
       :box (:line-width 8 :color "#E6C384")))
  "Face för Evil insert-läge."
  :group 'cocaine-line)

(defface cocaine-line-evil-visual-face
  '((t :foreground "#333333" :background "#D27E99" :weight bold
       :box (:line-width 8 :color "#D27E99")))
  "Face för Evil visual-läge."
  :group 'cocaine-line)

(defface cocaine-line-evil-replace-face
  '((t :foreground "#333333" :background "#FF5D62" :weight bold
       :box (:line-width 8 :color "#FF5D62")))
  "Face för Evil replace-läge."
  :group 'cocaine-line)

(defface cocaine-line-evil-emacs-face
  '((t :foreground "#333333" :background "#B0C4DE" :weight bold
       :box (:line-width 8 :color "#B0C4DE")))
  "Face för Emacs-läge."
  :group 'cocaine-line)

(defcustom cocaine-line-separator " | "
  "Separator som används mellan sektioner i mode-line."
  :type 'string
  :group 'cocaine-line)

(defcustom cocaine-line-separator-face 'cocaine-line-separator-face
  "Face för separatorn mellan sektioner."
  :type 'face
  :group 'cocaine-line)

(defface cocaine-line-separator-face
  '((t :foreground "#54536D" :weight bold :height 0.8))
  "Face för separatorn mellan sektioner i cocaine-line."
  :group 'cocaine-line)

(defun cocaine-get-mode-line-inactive-bg ()
  "Get the background color of the mode-line-inactive face."
  (face-background 'mode-line-inactive nil t))

(defun cocaine-update-inactive-face ()
  "Update the cocaine-line-inactive-face with the current mode-line-inactive background color."
  (let ((bg-color (cocaine-get-mode-line-inactive-bg)))
    (set-face-attribute 'cocaine-line-inactive-face nil
                        :box `(:line-width 8 :color ,bg-color))))

(cl-defun cocaine-add-separator (&key str separator leftside (last nil) (face 'cocaine-line-separator-face))
  "Lägg till en separator efter STR om det inte är tomt eller sist.
    LAST indikerar om detta är det sista elementet.
    FACE anger vilket face som ska användas för separatorn."
  (if (and str (not (string-empty-p str)) (not last))
      (if leftside
          (progn
            (if separator
                (concat str (propertize separator 'face face))
              (concat str (propertize cocaine-line-separator 'face face))))
        (progn
          (if separator
              (concat (propertize separator 'face face) str)
            (concat (propertize cocaine-line-separator 'face face) str))
          ))
    str))

;; Evil-status funktion
(defun cocaine-evil-status ()
  "Visa Evil-status med anpassad face och korrekt vertikal utsträckning."
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
  "Custom flycheck mode-line with icons and counts."
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
                     'face '(:inherit info)))
       (when (> warning 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-warning") warning)
                     'face '(:inherit warning)))
       (when (> error 0)
         (propertize (format " %s %d" (nerd-icons-codicon "nf-cod-error") error)
                     'face '(:inherit error)))))))

(defun cocaine-process-info ()
  "Visa information om aktiva processer."
  (when cocaine-show-processes-info
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string-blank-p process-info)
      (string-trim process-info)))))

(defun cocaine-misc-info ()
  "Visa information om misc info."
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
  "Visa Git-branch och status med anpassade faces."
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

(defun cocaine-project-info ()
  "Visa project information."
  (let ((project (cocaine-project-name)))
    (propertize project 'face 'cocaine-line-project-face)))

;; Anpassade funktioner för vänster sektion
(defun cocaine-buffer-name ()
  "Visa buffertnamn med anpassad face och ikon (om tillgänglig)."
  (let* ((file-name (buffer-file-name))
         (icon (when (and cocaine-show-use-nerd-icons file-name)
                 (nerd-icons-icon-for-file file-name t)))
         (buffer-name (file-name-sans-extension
                       (substring-no-properties (format-mode-line "%b ")))))
    (if icon
        (concat icon " " (propertize buffer-name 'face 'cocaine-line-buffer-name-face))
      (propertize buffer-name 'face 'cocaine-line-buffer-name-face))))

(defun cocaine-major-mode ()
  "Visa major mode med anpassad face."
  (propertize (substring-no-properties (format-mode-line mode-name))
              'face 'cocaine-line-major-mode-face))

(defun cocaine-line-col ()
  "Visa rad och kolumn med anpassad face."
  (when cocaine-show-column-info
    (propertize "[%l:%c]" 'face 'cocaine-line-position-face)))

(defun cocaine-buffer-position ()
  "Visa procentuell position med anpassad face."
  (when cocaine-show-buffer-position
    (propertize "%p%" 'face 'cocaine-line-position-face)))

(defun cocaine-copilot-info ()
  "HUD for Copilot."
  (when (bound-and-true-p copilot-mode)
    (propertize " " 'face '(:inherit success))))

(defun cocaine-line-spacer ()
  "Visa en tom sträng."
  (propertize " "))

(defun cocaine-time ()
  "Visa tid med anpassad face."
  (concat
   (propertize (format-time-string "%H:%M")
               'face 'cocaine-line-time-face)
   (make-string cocaine-right-padding ?\s)))

(defun cocaine-left-section ()
  "Skapa vänster sektion av mode-line."
  (let ((left-section (list (concat (cocaine-evil-status)
                                    (cocaine-line-spacer)
                                    (cocaine-buffer-name)
                                    (cocaine-add-separator :str (cocaine-major-mode) :separator "|")
                                    (cocaine-add-separator :str (cocaine-eglot-info))
                                    (cocaine-add-separator :str (mood-line-segment-hud))
                                    (cocaine-process-info)
                                    ))))
    left-section))

(defun cocaine-right-section ()
  "Skapa höger sektion av mode-line."
  (let ((right-section (concat
                        (cocaine-line-col)
                        (cocaine-add-separator :str (cocaine-flycheck-mode-line) :leftside t)
                        (cocaine-buffer-position)
                        (cocaine-add-separator :str (cocaine-copilot-info) :leftside t)
                        (cocaine-misc-info)
                        (cocaine-add-separator :str (cocaine-git-info) :leftside t)
                        (cocaine-time))))
    (list (propertize " " 'display `((space :align-to (- right ,(string-width right-section)))))
          right-section)))

(defun cocaine-evil-status-inactive ()
  "Show Evil status with gray face for inactive mode-line."
  (let* ((evil-state (if (and (bound-and-true-p evil-local-mode)
                              (boundp 'evil-state))
                         evil-state
                       'emacs))
         (state-name (if (eq evil-state 'emacs)
                         "EMACS"
                       (upcase (symbol-name evil-state)))))
    (propertize (format " %s " state-name)
                'face 'cocaine-line-inactive-face)))

(defun cocaine-mode-line-inactive-format ()
  "Inactive format with Evil status and buffer name in gray."
  (list (concat
         (cocaine-evil-status-inactive)
         (propertize "|" 'face 'cocaine-line-inactive-face)
         (propertize (format " %s " (buffer-name))
                     'face 'cocaine-line-inactive-face))))

(defun cocaine-mode-line-format ()
  "Generera formatet för cocaine-line mode-line."
  (let ((left (cocaine-left-section))
        (right (cocaine-right-section)))
    (if cocaine-line-is-active
        (append left right)
      (cocaine-mode-line-inactive-format))))

(defun cocaine-update-mode-line (&optional _)
  "Uppdatera mode-line för alla fönster."
  (let ((active-window (selected-window)))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (with-current-buffer (window-buffer window)
          (setq-local cocaine-line-is-active (eq window active-window))
          (force-mode-line-update window))))))

(defun cocaine-set-mode-line ()
  "Ställ in mode-line formatet för cocaine-line."
  (setq-default mode-line-format '(:eval (cocaine-mode-line-format))))

(defun cocaine-register-hooks ()
  "Registrera hooks för att uppdatera mode-line."
  (add-hook 'post-command-hook #'cocaine-update-mode-line)
  (add-hook 'window-configuration-change-hook #'cocaine-update-mode-line)
  (add-hook 'focus-in-hook #'cocaine-update-mode-line)
  (add-hook 'focus-out-hook #'cocaine-update-mode-line)
  (add-hook 'window-state-change-hook #'cocaine-update-mode-line)  ; Lägg till denna hook
  (add-hook 'after-load-theme-hook #'cocaine-update-inactive-face))

(defun cocaine-remove-hooks ()
  "Ta bort hooks för att uppdatera mode-line."
  (remove-hook 'post-command-hook #'cocaine-update-mode-line)
  (remove-hook 'window-configuration-change-hook #'cocaine-update-mode-line)
  (remove-hook 'focus-in-hook #'cocaine-update-mode-line)
  (remove-hook 'focus-out-hook #'cocaine-update-mode-line)
  (remove-hook 'window-state-change-hook #'cocaine-update-mode-line)  ; Ta bort denna hook
  (remove-hook 'after-load-theme-hook #'cocaine-update-inactive-face))

(define-minor-mode cocaine-line-mode
  "Aktivera Cocaine Line mode."
  :group 'cocaine-line
  :global t
  :lighter nil
  (if cocaine-line-mode
      (progn
        (cocaine-set-mode-line)
        (cocaine-register-hooks)
        (cocaine-update-inactive-face)
        (cocaine-update-mode-line))
    (setq-default mode-line-format (default-value 'mode-line-format))
    (cocaine-remove-hooks)
    (force-mode-line-update t)))

(provide 'cocaine-line)
;;; cocaine-line.el ends here
