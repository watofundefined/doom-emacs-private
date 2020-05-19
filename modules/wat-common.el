;;; wat-common.el -*- lexical-binding: t; -*-

;; Auto-reload all buffers when they change on the disk
;; It doesn't revert unsaved changes like the name might suggest
(setq global-auto-revert-mode t)

(setq projectile-project-search-path '("~/src"))

(setq magit-repository-directories '(("~/src" . 3))
      magit-save-repository-buffers nil)

(use-package! deft
  :config
  (setq deft-directory org-directory
        deft-default-extension "org"
        deft-recursive t))

(remove-hook 'text-mode-hook #'auto-fill-mode) ; Disable hard wrapping
(add-hook 'text-mode-hook #'visual-line-mode)  ; Enable soft wrapping

(setq dired-listing-switches "-la") ; Show all files in dired

;; I'll ask for help when I need it (`C-Spc` by default)
(setq company-idle-delay nil)

(setq evil-split-window-below t ; Switch to the new window after splitting
      evil-vsplit-window-right t)

(display-time-mode 1) ; Enable time in the mode-line

(provide 'wat-common)
