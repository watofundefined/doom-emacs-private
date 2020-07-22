;;; wat-ui.el -*- lexical-binding: t; -*-

(setq w-ui--font "JetBrains Mono")
;; (setq w-ui--font "Comic Code Ligatures")
(setq w-ui--font-size 20)
(setq w-ui--font-size-large 26)

;; Themes
;;
;; Doom
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-one-light) ;; --- #1 light
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-acario-dark) ;; --- changes font size
;; (setq doom-theme 'doom-acario-light) ;; --- changes font size
;; (setq doom-theme 'doom-city-lights)
;; (setq doom-theme 'doom-challenger-deep)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-ephemeral)
;; (setq doom-theme 'doom-fairy-floss)
(setq doom-theme 'doom-gruvbox) ;; --- #1 dark
(setq doom-gruvbox-dark-variant "hard") ;; "hard" "medium" "soft"
(setq doom-gruvbox-brighter-comments nil)
(setq doom-gruvbox-padded-modeline 3)
;; (setq doom-theme 'doom-gruvbox-light) ;; ---
;; (setq doom-theme 'doom-henna)
;; (setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-Iosvkem)
;; (setq doom-theme 'doom-laserwave)
;; (setq doom-theme 'doom-material)
;; (setq doom-theme 'doom-manegarm)
;; (setq doom-theme 'doom-molokai)
;; (setq doom-theme 'doom-monokai-classic)
;; (setq doom-theme 'doom-monokai-pro)
;; (setq doom-theme 'doom-moonlight)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-nord-light)
;; (setq doom-theme 'doom-nova)
;; (setq doom-theme 'doom-oceanic-next)
;; (setq doom-theme 'doom-opera)
;; (setq doom-theme 'doom-opera-light) ;; ---
;; (setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-palenight)
;; (setq doom-theme 'doom-peacock)
;; (setq doom-theme 'doom-rouge)
;; (setq doom-theme 'doom-snazzy)
;; (setq doom-theme 'doom-solarized-dark)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-sourcerer)
;; (setq doom-theme 'doom-spacegrey)
;; (setq doom-theme 'doom-tomorrow-day) ;; ---
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-wilmersdorf)
;; (setq doom-theme 'doom-mono-dark)
;; (setq doom-theme 'doom-mono-light)
;; (setq doom-theme 'doom-tron)
;;
;; Solarized Family
;; https://github.com/bbatsov/solarized-emacs
;; Variants:
;; (setq doom-theme 'solarized-light)
;; (setq doom-theme 'solarized-gruvbox-dark)
;; (setq doom-theme 'solarized-light-high-contrast)
;; (setq doom-theme 'solarized-dark)
;; (setq doom-theme 'solarized-dark-high-contrast)
;; (setq doom-theme 'solarized-gruvbox-light)
;; (setq doom-theme 'solarized-wombat-dark)
;; (setq doom-theme 'solarized-zenburn)
;; Config:
;; (setq solarized-use-variable-pitch nil)
;; (setq solarized-height-minus-1 1.0)
;; (setq solarized-height-plus-1 1.0)
;; (setq solarized-height-plus-2 1.0)
;; (setq solarized-height-plus-3 1.0)
;; (setq solarized-height-plus-4 1.0)
;;
;; Other - Dark
;; (setq doom-theme 'eigengrau)
;; (setq doom-theme 'gruber-darker)
;;
;; Other - Light
;; (setq doom-theme 'doneburn)
;;
;; Other (Not installed):
;; https://github.com/owainlewis/emacs-color-themes
;; https://github.com/humanoid-colors/emacs-humanoid-themes
;; https://gitlab.com/protesilaos/modus-themes
;; https://github.com/bbatsov/zenburn-emacs
;; https://github.com/gchp/flatland-emacs
;; https://github.com/habamax/habamax-theme/


(if (member w-ui--font (font-family-list))
    (setq doom-font (font-spec :family w-ui--font :size w-ui--font-size :weight 'semi-light)
          doom-variable-pitch-font (font-spec :family w-ui--font :size w-ui--font-size-large :weight 'semi-light)
          doom-big-font nil
          doom-big-font-increment 6
          doom-serif-font (font-spec :family w-ui--font :size w-ui--font-size-large :weight 'semi-light)))

(setq display-line-numbers-type nil)


(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " | %s" project-name))))))

;; Centering logic taken from https://bzg.fr/en/emacs-strip-tease.html/
(defun w-ui--center ()
  (set-fringe-mode
   (/ (- (frame-pixel-width) (* 90 (frame-char-width)))
      2))
  (custom-set-faces
   '(fringe ((t (:background nil))))))

(defun w-ui--uncenter ()
  (set-fringe-mode 0))

(defun w-ui-toggle-center ()
  (interactive)
  "Toggle Centered View"
  (if (< fringe-mode 10)
      (w-ui--center)
    (w-ui--uncenter)))

(add-hook 'window-configuration-change-hook
          (lambda ()
            (if (delq nil
                      (let ((fw (frame-width)))
                        (mapcar (lambda(w) (< (window-width w) (/ fw 2)))
                                (window-list))))
                (w-ui--uncenter))))

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Center view"  "c" #'w-ui-toggle-center))


(provide 'wat-ui)
