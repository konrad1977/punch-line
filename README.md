# Punch line

<p align="center">
  <img src="https://raw.githubusercontent.com/konrad1977/punch-line/refs/heads/master/screenshots/punch-line.png" alt="Screenshot of Punch-line and mode line for Emacs."/>
</p>


```emacs-lisp
(use-package punch-line
  :ensure nil
  :after evil
  :custom
  (punch-weather-update) ;; Use weather service
  :config
  (setq
   punch-line-separator "  "
   punch-show-project-info t					;; Show project info
   punch-show-git-info t						;; Show git info
   punch-show-lsp-info t						;; Show eglot info
   punch-show-copilot-info t					;; Show copilot
   punch-show-battery-info t					;; Show battery status
   punch-show-weather-info t					;; Weather info
   punch-weather-latitude "56.7365"				;; Weather latitude
   punch-weather-longitude "16.2981"			;; Weather longitude
   punch-line-music-info '(:service apple))		;; Music service
  (punch-line-mode 1))

```
