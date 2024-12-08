# Punch line

<p align="center">
  <img src="https://raw.githubusercontent.com/konrad1977/punch-line/refs/heads/master/screenshots/punch-line.png" alt="Screenshot of Punch-line and mode line for Emacs."/>
</p>


```emacs-lisp
(use-package punch-line
  :ensure nil
  :after ((after-init . punch-line-mode)        ;; Load punch-line
          (after-init . punch-weather-update)   ;; Load weather
          (after-init . punch-load-tasks))      ;; Load saved current 
  :config
  (setq
   punch-line-left-separator "  "
   punch-line-right-separator "  "
   punch-show-flycheck-info t               ;; Show flycheck info
   punch-show-project-info t                ;; Show project info
   punch-show-git-info t                    ;; Show git info
   punch-show-lsp-info t                    ;; Show eglot info
   punch-show-copilot-info t                ;; Show copilot
   punch-show-battery-info t                ;; Show battery status
   punch-show-weather-info t                ;; Weather info
   punch-weather-latitude "56.7365"         ;; Weather latitude
   punch-weather-longitude "16.2981"        ;; Weather longitude
   punch-line-music-max-length 80           ;; Max length of artist and song
   punch-line-music-info '(:service apple)) ;; Music service
  )

```

## What am I doing plugin
```shell
M-x punch-line-what-am-i-doing-next             ### my next tasks
M-x punch-line-what-am-i-doing-done             ### mark task as done
M-x punch-line-what-am-i-doing-show-all         ### show all tasks
M-x punch-line-what-am-i-doing-next-task        ### show all tasks
M-x punch-line-what-am-i-doing-previous-task    ### show all tasks
```

<p align="center">
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/get-shit-done.png" 
  alt="Screenshot of a what I am currently working on."/>
</p>

<p align="center">
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/what-am-i-all.png" 
  alt="All my tasks in my list"/>
</p>

