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

(defun w-utils-prompt-commit-type ()
  "Prompt user to pick a commit type.
Taken from Angular contributing guide:
https://github.com/angular/angular/blob/master/CONTRIBUTING.md"
  (interactive)
  (let ((options '("1) 'build'    Changes that affect the build system or external dependencies
              (example scopes: gulp, broccoli, npm)"
                   "2) 'ci'       Changes to CI configuration files and scripts
              (example scopes: Travis, Circle, BrowserStack, SauceLabs)"
                   "3) 'docs'     Documentation only changes"
                   "4) 'feat'     A new feature"
                   "5) 'fix'      A bug fix"
                   "6) 'perf'     A code change that improves performance"
                   "7) 'refactor' A code change that neither fixes a bug nor adds a feature"
                   "8) 'style'    Changes that do not affect the meaning of the code
              (white-space, formatting, missing semi-colons, etc)"
                   "9) 'test'     Adding missing tests or correcting existing tests")))
    (let ((choice (ivy-read "Commit type: " options)))
      (let ((result (concat (first (s-match "\\w+" (first (s-match "'.*'" choice))))
                            ": ")))
      (insert result)))))

(map! :after evil-magit
      :map magit-mode-map
      :n "?" #'w-utils-prompt-commit-type)

(provide 'wat-utils)
