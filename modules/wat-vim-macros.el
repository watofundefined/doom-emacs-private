;;; wat-vim-macros.el -*- lexical-binding: t; -*-

;; How to record a new macro:
;; https://stackoverflow.com/questions/22817120/how-can-i-save-evil-mode-vim-style-macros-to-my-init-el
;;
;; Triggered by @<char>,
;; where <char> is the first argument passed to 'evil-set-register

;; JavaScript
;; Convert line with CommonJS 'require' to ESM 'import'
(evil-set-register ?r [?^ ?v ?e ?c ?i ?m ?p ?o ?r ?t escape ?f ?= ?v ?c ?f ?r ?o ?m escape ?f ?r ?v ?e ?d ?d ?s ?\(])
;; Convert line with ESM 'import' to CommonJS require
(evil-set-register ?i [?^ ?v ?e ?c ?c ?o ?n ?s ?t escape ?f ?\' ?b ?v ?e ?c ?= ?  delete ?r ?e ?q ?u ?i ?r ?e escape ?l ?v ?a ?\' ?S ?\) escape])

(provide 'wat-vim-macros)
