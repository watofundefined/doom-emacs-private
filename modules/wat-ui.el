;;; wat-ui.el -*- lexical-binding: t; -*-

(setq doom-theme 'eigengrau)
;(setq doom-theme 'doom-one-light)

(if (member "Comic Code Ligatures" (font-family-list))
    (setq doom-font (font-spec :family "Comic Code Ligatures" :size 20 :weight 'semi-light)
          doom-variable-pitch-font (font-spec :family "Comic Code Ligatures" :size 26 :weight 'semi-light)
          doom-big-font nil
          doom-big-font-increment 6
          doom-serif-font (font-spec :family "Comic Code Ligatures" :size 26 :weight 'semi-light)))

(setq display-line-numbers-type nil)

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " | %s" project-name))))))

(provide 'wat-ui)
