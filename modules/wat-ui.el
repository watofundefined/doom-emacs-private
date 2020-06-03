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
  (set-fringe-mode 0)
  (custom-set-faces
   ;; FIXME Remove hard-coded theme-specific color
   '(fringe ((t (:background "#292929"))))))

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
