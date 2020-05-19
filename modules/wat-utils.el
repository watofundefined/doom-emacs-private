;;; wat-utils.el -*- lexical-binding: t; -*-
;; All of these surely exist in *some* util packages
;; But I don't know the ecosystem well enough

(defun w-utils-online-p ()
  "Check if computer is connected to the internets"
  (= 0 (call-process "curl" nil nil nil "https://1.1.1.1")))

(defun w-utils-elapsed-time (past)
  "Returns time (in seconds) that passed since `past`"
  (float-time (time-subtract nil past)))

(defun w-utils-log (buffer &rest ARGS)
  "Log to provided buffer, accepts more messages to be logged (each delimeted with '\n'"
  (get-buffer-create buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
    (apply #'insert (cons "\n" ARGS))))

(defun w-utils-seq-contains (seq item)
  (if (fboundp 'seq-contains-p)
      (seq-contains-p seq item)
    (seq-contains seq item)))

(defun w-utils-windows-p ()
  (string-equal system-type "windows-nt"))


(provide 'wat-utils)
