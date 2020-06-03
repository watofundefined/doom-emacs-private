;;; wat-org.el -*- lexical-binding: t; -*-

(require 'org-protocol-capture-html)


(setq w-org--gtd-main-file "BMO.org")
(setq w-org--gtd-dir (concat org-directory "gtd/"))
(setq w-org--templates-dir (concat doom-private-dir "templates/"))
(setq w-org--agenda-files `(,w-org--gtd-main-file "tickler.org"))

(setq w-org--refile-targets '((nil :maxlevel . 3) ; refile withing current file
                              (w-org--agenda-files :maxlevel . 5)
                              (("someday.org" "inbox.org") :maxlevel . 7)))

(setq w-org--capture-templates
      `(("t" "Todo" entry (file ,(concat w-org--gtd-dir "inbox.org"))
         "* TODO %?" :kill-buffer t)
        ("n" "Note" entry (file ,(concat w-org--gtd-dir "inbox.org"))
         "* %?" :kill-buffer t)
        ;; Triggered by JS bookmarklet from within the browser.
        ;; Can be used to bookmark a page or store it for offline access.
        ;; See: https://github.com/alphapapa/org-protocol-capture-html
        ("w" "Web site" entry (file ,(concat w-org--gtd-dir "inbox.org"))
         "* %a :website:\n%U %?\n\n%i" :immediate-finish t)
        ))

;; Tags should help me decide what what to pick next
;; depending on where I am and how much time/energy I have
(setq w-org--tags '(
                    ;; Context
                    ("@home" . ?h) ("@office" . ?o) ("@computer" . ?c)
                    ("@anywhere" . ?a) ("@market" . ?m) ("@town" . ?t) (:newline)
                    ;; Depth
                    ("immersive" . ?i) ("process" . ?p) (:newline)
                    ;; Time
                    ("15min" . ?<) ("30min" . ?=) ("1h" . ?>) (:newline)
                    ;; Energy
                    ("#easy" . ?1) ("#average" . ?2) ("#challenge" . ?3) (:newline)
                    ;; Other
                    ("Project" . ?x)))


;;
;; Configure Org Packages
(use-package org
  :config
  (setq org-todo-keywords '("TODO(t)" "NEXT(n)" "INPROGRESS(i)" "WAITING(w)"
                            "|" "DONE(d)" "CANCELLED(c)")
        org-capture-templates w-org--capture-templates
        org-log-done 'time
        org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")
        org-ellipsis " ▿"
        org-tags-column -80
        org-refile-targets w-org--refile-targets
        org-refile-allow-creating-parent-nodes 'confirm
        org-indent-mode nil
        org-agenda-files w-org--agenda-files
        org-agenda-inhibit-startup nil
        org-agenda-show-future-repeats nil
        org-agenda-show-current-time-in-grid t
        org-agenda-use-time-grid t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-agenda-start-on-weekday t
        org-agenda-show-all-dates t
        org-agenda-start-day "+0d"
        org-agenda-span 'day
        org-agenda-current-time-string "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈ now"
        org-deadline-warning-days 14
        org-archive-location (concat w-org--gtd-dir ".archive/%s::")
        org-tag-alist w-org--tags))


;; See https://github.com/alphapapa/org-super-agenda for more grouping options
(after! org-agenda
  (require 'evil-org-agenda)
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)
        org-super-agenda-groups
        '((:name "Today" :time-grid t :scheduled today)
          (:name "Due today" :deadline today)
          (:name "Important" :priority "A")
          (:name "Overdue" :deadline past :scheduled past)
          (:name "Due soon" :deadline future)
          (:name "Waiting" :todo "WAITING"))))

(use-package! org-journal
  :config
  (setq
         org-journal-dir (concat org-directory "journal")
         org-journal-file-type 'yearly
         org-journal-file-format "Journal_%Y.org"
         org-journal-date-format "%Y-%m-%d %A"
         ;; FIXME Figure out how to configure the pgp stuff first.
         ;; org-journal-encrypt-journal t
         org-journal-carryover-items nil)
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t))
  (add-hook
   'org-journal-after-entry-create-hook
   (lambda ()
     (setq auto-fill-mode t)
     (insert "\n")
     (evil-insert-state))))

(use-package! org-roam
  :config
  (require 'org-roam-protocol)
  (setq org-roam-directory (concat org-directory "roam")
        org-roam-graph-exclude-matcher "private"
        org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
                 :head "#+TITLE: ${title}\n"
                 :unnarrowed t)
          ("b" "blog" plain (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
                 :head "#+TITLE: ${title}
#+HUGO_SECTION: posts
#+HUGO_SLUG: ${slug}
#+HUGO_DATE: %(format-time-string \"%Y-%m-%dT%H-%M-%S+03:00\" (current-time) t)
#+HUGO_DRAFT: true
#+HUGO_TAGS:
#+HUGO_CUSTOM_FRONT_MATTER: :subheading
#+HUGO_CUSTOM_FRONT_MATTER: :subheadingForArticle
#+HUGO_CUSTOM_FRONT_MATTER: :summary
#+HUGO_CUSTOM_FRONT_MATTER: :summaryForArticle
#+HUGO_CUSTOM_FRONT_MATTER: :customCss /posts/${slug}.css
#+HUGO_CUSTOM_FRONT_MATTER: :customJs /posts/${slug}.js
#+SETUPFILE:./hugo_setup.org\n\n
* Footnotes
​* COMMENT Local Variables                          :ARCHIVE:
# Local Variables:
# eval: (org-hugo-auto-export-mode)
# End:"
                 :unnarrowed t))))


;;
;; Private Functions
(defun w-org--find-any-agenda-file ()
  (find-file (concat w-org--gtd-dir (first org-agenda-files))))

(defun w-org--try-disable-sync ()
  (if (featurep 'wat-org-sync)
      (w-org-sync-disable)))

(defun w-org--create-review (name template-filename)
  (w-org--try-disable-sync)
  (let ((org-capture-templates
         `(("x" ,name
                 entry
                 (file+olp+datetree "/tmp/reviews.org")
                 (file ,(concat w-org--templates-dir template-filename))))))
    (org-capture nil "x")
    (org-capture-finalize t)
    (org-speed-move-safe #'outline-up-heading)
    (org-narrow-to-subtree)
    (org-clock-in)))

(defun w-org--open-loops (back forth)
  (let ((org-agenda-start-with-log-mode t)
        (org-agenda-use-time-grid nil)
        (start-date (org-read-date nil nil (concat "-" (number-to-string back) "d")))
        (period (+ back forth)))
    (w-org--find-any-agenda-file)
    (org-agenda-list nil start-date period)))

(defun w-org--make-org-ql-search-result-title (title)
  "I don't want to see the org-ql query displayed in header, it's distracting me.
This wraps the title with spaces on both sides, so that it's centered on the screen.
Which pushes the query outside of window. It assumes that the the target
window width will be the same as the width of the current window.
The magic number 5 is 5 characters in 'View:' ¯\\_(ツ)_/¯ "
  (let ((number-of-whitespaces-on-sides
         (round (/ (- (window-total-width) 5 (length title)) 2))))
    (concat (make-string number-of-whitespaces-on-sides ?\s)
            title
            (make-string number-of-whitespaces-on-sides ?\s))))

(defun w-org--save-buffer-delayed ()
  "Save current buffer in 0.5 sec"
  (run-with-timer 0.5 nil #'save-buffer))


;;
;; Public Functions
;; Not all are interactive, some are just meant to be called from GTD templates
(defun w-org-daily-review ()
  (interactive)
  (w-org--create-review "Daily Review" "daily-review.org"))

(defun w-org-weekly-review ()
  (interactive)
  (w-org--create-review "Weekly Review" "weekly-review.org"))

(defun w-org-monthly-review ()
  (interactive)
  (w-org--create-review "Monthly Review" "monthly-review.org"))

(defun w-org-show-todos ()
  (interactive)
  (w-org--find-any-agenda-file)
  (org-agenda "TODO" "T"))

(defun w-org-find-main-gtd-file ()
  (interactive)
  (find-file (concat w-org--gtd-dir w-org--gtd-main-file)))

(defun w-org-find-someday-file ()
  (interactive)
  (find-file (concat w-org--gtd-dir "someday.org")))

(defun w-org-todays-agenda ()
  (interactive)
  (w-org--find-any-agenda-file)
  (org-agenda-list))

(defun w-org-todays-agenda-and-todos ()
  (interactive)
  (w-org--find-any-agenda-file)
  (org-agenda "" "n"))

(defun w-org-recent-open-loops ()
  (interactive)
  (w-org--open-loops 2 3))

(defun w-org-longer-open-loops ()
  (interactive)
  (w-org--open-loops 14 14))

(defun w-org-stuck-projects ()
  "Show stuck projects and un-scheduled one-off todos.
Project is any todo which contains todos."
  (interactive)
  (w-org-find-main-gtd-file)
  (org-ql-search (org-agenda-files)
    '(and (todo)
          (or (and (ancestors (todo))
                   (ancestors (and (not (descendants (todo "NEXT")))
                                   (not (descendants (scheduled))))))
              (and (ancestors "One-off") (not (scheduled)))))
    :super-groups '((:auto-parent t))
    :title (w-org--make-org-ql-search-result-title "Stuck Projects")))

(defun w-org-waiting-projects ()
  "Show projects with waiting todos. Project is any todo which contains todos."
  (interactive)
  (w-org-find-main-gtd-file)
  (org-ql-search (org-agenda-files)
    '(and (todo)
          (and (or (ancestors (todo)) (ancestors "One-off")))
               (ancestors (and (descendants (todo "WAITING")))))
    :super-groups '((:auto-parent t))
    :title (w-org--make-org-ql-search-result-title "Waiting Projects")))

(defun w-org-open-emails ()
  "Open my emails inside browser"
  (browse-url "https://mail.google.com/mail/u/0/#inbox")
  (browse-url "https://mail.google.com/mail/u/1/#inbox")
  (browse-url "https://mail.google.com/mail/u/2/#inbox")
  (browse-url "https://beta.protonmail.com/inbox"))

;; A shameless copy-paste (yank?) of Doom's 'doom/find-file-in-private-config
(defun w-org-find-file-in-org-dir ()
  "Search for a file in `org-directory'."
  (interactive)
  (doom-project-find-file org-directory))


;; Doom(?) remaps org-set-tags-command to a different (counsel) implementation.
;; I prefer the original which offers 1-letter shortcuts for tagging
;; See org-tag-alist, ("process" . ?p) allows to add "process" tag by pressing 'p'
(after! counsel
  (define-key!
    [remap org-set-tags-command]  #'org-set-tags-command))


;;
;; Hooks
;;
;; I want to start typing straight away
(add-hook 'org-capture-mode-hook #'evil-insert-state)
;;
;; Both 'org-after-refile-insert-hook and 'org-archive-hook are executed
;; *after* the destination buffer is written to and *before* the source buffer
;; is modified so the source buffer (i.e. curent buffer) needs to be saved again
;; with a small delay.
(dolist (hook '(org-after-refile-insert-hook org-archive-hook))
  (add-hook hook 'org-save-all-org-buffers)
  (add-hook hook 'w-org--save-buffer-delayed))

(map! :leader
      (:prefix-map ("n" . "notes")
       :desc "Org agenda"  "A"  #'org-agenda
       :desc "Org agenda today"  "a"  #'w-org-todays-agenda
       (:prefix ("g" . "gtd reviews")
        :desc "Daily Review"  "d"  #'w-org-daily-review
        :desc "Weekly Review"  "w"  #'w-org-weekly-review
        :desc "Monthly Review"  "m"  #'w-org-monthly-review))
      (:prefix-map ("o" . "open")
       (:prefix ("a" . "org agenda")
        :desc "Org agenda today"  "a"  #'w-org-todays-agenda)))

(map! :leader
      (:prefix-map ("f" . "file")
       :desc "Find file in Org dir"  "o" #'w-org-find-file-in-org-dir))


(provide 'wat-org)
