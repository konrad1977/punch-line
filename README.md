# Punch Line

A modern, feature-rich mode line for Emacs

❤️ [Please sponsor me if you like this package](https://github.com/sponsors/konrad1977)

<p align="center">
  <img src="https://raw.githubusercontent.com/konrad1977/punch-line/refs/heads/master/screenshots/punch-line.png" alt="Screenshot of Punch-line and mode line for Emacs."/>
</p>

## Features

- 🎨 Highly customizable appearance
- 👿 Evil/Meow mode integration
- 🎵 Music player integration (Apple Music/Spotify)
- 🌡️ Weather information
- 📊 Git status and branch information
- 🔋 Battery status
- ⌚ Time display
- 🤖 Copilot integration
- ✅ Flycheck integration
- 📝 LSP/Eglot support
- 📋 Task management system
- 🎯 Project awareness

## Installation

```elisp
(use-package punch-line
  :ensure nil
  :after ((after-init . punch-line-mode)        ;; Load punch-line
          (after-init . punch-weather-update)   ;; Load weather
          (after-init . punch-load-tasks))      ;; Load saved current tasks
  :config
  (setq
   punch-line-left-separator "  "
   punch-line-right-separator "  "
   punch-line-music-info '(:service apple)      ;; Music service configuration
   punch-line-music-max-length 80))             ;; Max length of artist and song
```

## Feature Toggles

| Feature Toggle                    | Default | Description                                    |
|----------------------------------|---------|------------------------------------------------|
| `punch-line-show-evil-modes`     | t       | Show Evil/Meow mode states                     |
| `punch-line-modal-use-fancy-icon`| t       | Use fancy icons for modal states               |
| `punch-show-git-info`            | t       | Display Git branch and status                  |
| `punch-show-project-info`        | t       | Show current project information               |
| `punch-show-lsp-info`            | t       | Display LSP/Eglot status                       |
| `punch-show-copilot-info`        | t       | Show Copilot status                           |
| `punch-show-battery-info`        | t       | Display battery status                         |
| `punch-show-weather-info`        | t       | Show weather information                       |
| `punch-show-flycheck-info`       | t       | Display Flycheck status                        |
| `punch-show-processes-info`      | t       | Show active processes                          |
| `punch-show-org-info`            | t       | Display Org-mode information                   |
| `punch-show-misc-info`           | nil     | Show miscellaneous information                 |
| `punch-line-show-time-info`      | t       | Display current time                           |
| `punch-show-column-info`         | nil     | Show column number                             |
| `punch-show-buffer-position`     | nil     | Display buffer position                        |
| `punch-show-what-am-i-doing-info`| t       | Show current task information                  |
| `punch-battery-show-percentage`  | t       | Display battery percentage                     |

## Task Management System

The built-in task management system helps you keep track of your current activities:

```shell
M-x punch-line-what-am-i-doing-next          # View next tasks
M-x punch-line-what-am-i-doing-done          # Mark current task as completed
M-x punch-line-what-am-i-doing-show-all      # Display all tasks
M-x punch-line-what-am-i-doing-next-task     # Switch to next task
M-x punch-line-what-am-i-doing-previous-task # Switch to previous task
```
<p align="center">
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/what_am_i_doing.png" 
  alt="Screenshot of a what I am currently working on."/>
</p>

<p align="center">
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/get-shit-done.png" 
  alt="Screenshot of a what I am currently working on."/>
</p>

<p align="center">
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/what-am-i-all.png" 
  alt="All tasks."/>
</p>

## Customization

### Appearance

```elisp
;; Mode line size
(setq punch-line-modal-size 'small)  ; Options: 'small, 'medium, 'large

;; Divider style
(setq punch-line-modal-divider-style 'circle)  ; Options: 'arrow, 'flame, 'ice, 'circle

;; Separators
(setq punch-line-left-separator "  ")
(setq punch-line-right-separator "  ")
```

<p align="left">
    Arrow
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/modal_arrow.png" alt="Arrow"/>
</p>

<p align="left">
    Circle
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/modal_circle.png" alt="Circle"/>
</p>

<p align="left">
    Flame
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/modal_flame.png" alt="Flame"/>
</p>

<p align="left">
    Ice
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/modal_ice.png" alt="Ice"/>
</p>

<p align="left">
    None
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/modal_none.png" alt="None"/>
</p>


### Weather Configuration

```elisp
(setq punch-show-weather-info t
      punch-weather-latitude "56.7365"
      punch-weather-longitude "16.2981")
```

### Music Player Integration

```elisp
(setq punch-line-music-info '(:service apple)  ; or 'spotify
      punch-line-music-max-length 80)
```

<p align="center">
  <img src="https://github.com/konrad1977/punch-line/blob/master/screenshots/music.png" 
  alt="None"/>
</p>

## Cache Settings

```elisp
;; Update intervals in seconds
(setq punch-git-cache-update-interval 5
      punch-battery-cache-update-interval 60
      punch-flycheck-cache-interval 3)
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
