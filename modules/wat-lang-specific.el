;;; wat-lang-specific.el -*- lexical-binding: t; -*-

(require 'prettier-js)

(defun turn-on-prettier ()
  (unless (bound-and-true-p prettier-js-mode)
    (prettier-js-mode)))

(add-hook! 'js2-mode-hook #'turn-on-prettier)
(add-hook! 'web-mode-hook #'turn-on-prettier)
(add-hook! '+web-react-mode-hook #'turn-on-prettier)
(add-hook! before-save-hook #'refmt-before-save nil t)

(provide 'wat-lang-specific)
