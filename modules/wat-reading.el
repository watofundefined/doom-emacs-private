;;; wat-reading.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Comic Code Ligatures" :height 180)
  (buffer-face-mode)
  (setq nov-text-width 80
        line-spacing 6
        header-line-format " "
        nov-text-width t
        visual-fill-column-center-text t)
        (auto-fill-mode nil)
        (visual-line-mode 1)
        ;; (visual-fill-column-mode 1)
        (writeroom-mode 1))

(add-hook! nov-mode #'my-nov-font-setup)

(provide 'wat-reading)
