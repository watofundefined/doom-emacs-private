;;; wat-lang-specific.el -*- lexical-binding: t; -*-

(require 'prettier-js)

(add-hook! 'js2-mode-hook 'prettier-js-mode)
(add-hook! 'web-mode-hook 'prettier-js-mode)
(add-hook! before-save-hook #'refmt-before-save nil t)

(provide 'wat-lang-specific)
