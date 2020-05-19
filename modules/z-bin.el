;;; z-bin.el -*- lexical-binding: t; -*-
;;;
;;; Sort of OS Trash
;;; Stuff I'll leave here to marinate for a while before recovering/deleting
;;; Maybe it shouldn't exist and be just in git history but ¯\_(ツ)_/¯

;; source: https://orgmode.org/worg/org-hacks.html
(run-at-time 60 300 'w-org--agenda-redo-in-other-window)
(defun w-org--agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
