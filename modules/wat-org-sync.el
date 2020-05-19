;;; wat-org-sync.el -*- lexical-binding: t; -*-
;;
;; I use cloud sync only for the files that I want on my phone.
;;
;; Abbreviations:
;; - 'r-2-l' = remote to local sync
;; - 'l-2-r' = local to remote sync
;;

(require 'wat-org)


;;
;; Config
;; I use default org-directory so my synced files are:
;; ~/org/gtd/BMO.org and ~/org/gtd/inbox-phone.org
(setq w-org-sync--files '("BMO.org" "inbox-phone.org"))
(setq w-org-sync--dir (concat org-directory "gtd/"))

(setq w-org-sync--log-buffer "*Org Cloud-Sync Log*")

(setq w-org-sync--l-2-r-cmd (concat doom-private-dir "scripts/" "synclocaltoremote"))
(setq w-org-sync--r-2-l-cmd (concat doom-private-dir "scripts/" "syncremotetolocal"))

;; Debounce sync to remote after file is saved
(setq w-org-sync--l-2-r-debounce-time 15) ;; 15 secs

;; Try fetching data from remote shortly after Emacs starts
(setq w-org-sync--r-2-l-delay-after-start 30) ;; 30 secs
(setq w-org-sync--r-2-l-cooldown (* 30 60)) ;; 30 mins
(setq w-org-sync--r-2-l-interval (* 60 60)) ;; 1 hr

;; Warn if using Windows and no Cygwin is installed
(if (w-utils-windows-p)
    (unless (or (file-exists-p "C:/cygwin64/bin/bash.exe")
                (file-exists-p "C:/cygwin/bin/bash.exe"))
      (message-box "You'll need to install Cygwin to get the sync running correctly.
 If you don't want sync, then you can stop this whole module from loading
 by commenting out the (require 'wat-org-sync) under config.el")))


;;
;; State
(setq w-org-sync--inprogress nil)
(setq w-org-sync--r-2-l-ran-at nil)
(setq w-org-sync--l-2-r-debounce-timer nil)
(setq w-org-sync--enabled t)


;;
;; Private Functions
(defun w-org-sync--synced-file-p (path)
  (require 'wat-utils)
  (w-utils-seq-contains w-org-sync--files (f-filename path)))

(defun w-org-sync--on-sync-finished (_ event)
  (if (string= event "finished\n")
      (setq w-org-sync--inprogress nil)))

(defun w-org-sync--offline-warning ()
 (message-box "Can't sync, you're offline"))

(defun w-org-sync--sync-already-in-progress-warning (requested-sync-type)
 (message-box
  (format "Can't perform '%s' sync because sync is already in progress.
Verify your phone-synced files have correct content once the sync has completed.
(You can see the status of the sync in the buffer '%s')
If the files are Ok, then re-run by 'M-x w-org-sync-l-2-r' or 'M-x w-org-sync-r-2-l'"
          requested-sync-type
          w-org-sync--log-buffer)))

(defun w-org-sync--r-2-l-ran-recently-p ()
  (and w-org-sync--r-2-l-ran-at ;; not nil
       (> w-org-sync--r-2-l-cooldown
          (w-utils-elapsed-time w-org-sync--r-2-l-ran-at))))

(defun w-org-sync--try-r-2-l ()
  (when (and w-org-sync--enabled
             (not (w-org-sync--r-2-l-ran-recently-p))
             (not w-org-sync--inprogress)
             (w-utils-online-p))
    (w-org-sync--r-2-l)))

(defun w-org-sync--r-2-l ()
  (setq w-org-sync--r-2-l-ran-at (current-time))
  (setq w-org-sync--inprogress t)
  (set-process-sentinel
   (apply #'start-process
          (append `("r-2-l"
                    ,w-org-sync--log-buffer)
                  (if (w-utils-windows-p) '("sh") '()) ;; Using Cygwin sh on Windows
                  `(,(expand-file-name w-org-sync--r-2-l-cmd)
                    ,(expand-file-name w-org-sync--dir))
                  w-org-sync--files))
   #'w-org-sync--on-sync-finished))

(defun w-org-sync--try-l-2-r ()
  (if w-org-sync--enabled
      (if w-org-sync--inprogress
          (w-org-sync--sync-already-in-progress-warning "l-2-r")
        (w-org-sync--debounced-l-2-r))))

(defun w-org-sync--debounced-l-2-r ()
  (and w-org-sync--l-2-r-debounce-timer
       (cancel-timer w-org-sync--l-2-r-debounce-timer))
  ;; Set inprgress flag already at this point to prevent r-2-l run during debounce period
  (setq w-org-sync--inprogress t)
  (setq w-org-sync--l-2-r-debounce-timer
        (run-with-timer w-org-sync--l-2-r-debounce-time nil #'w-org-sync--l-2-r)))

(defun w-org-sync--file-names-to-sync ()
  (mapcar (lambda (f) (expand-file-name (concat w-org-sync--dir f)))
          w-org-sync--files))

(defun w-org-sync--l-2-r ()
  (setq remote-sync-inprogress t)
  (set-process-sentinel
   (apply #'start-process
          (append `("l-2-r"
                    ,w-org-sync--log-buffer)
                  (if (w-utils-windows-p) '("sh") '()) ;; Using Cygwin sh on Windows
                  `(,(expand-file-name w-org-sync--l-2-r-cmd))
                  (w-org-sync--file-names-to-sync)))
   #'w-org-sync--on-sync-finished))

(defun w-org-sync--after-save-hook-handler ()
  "Trigger sync *to* remote if saving one of the phone-synced files
(w-org-sync--files)"
  (if (and buffer-file-name (w-org-sync--synced-file-p buffer-file-name))
      (w-org-sync--try-l-2-r)))

(defun w-org-sync--doom-switch-buffer-hook-handler ()
  "Trigger r-2-l sync if switching to one of the phone-synced files"
  (if (and buffer-file-name (w-org-sync--synced-file-p buffer-file-name))
      (w-org-sync--try-r-2-l)))


;;
;; Public Functions
(defun w-org-sync-enable ()
  (interactive)
  (setq w-org-sync--enabled t))

(defun w-org-sync-disable ()
  (interactive)
  (setq w-org-sync--enabled nil))

(defun w-org-sync-r-2-l ()
  "Initiate r-2-l sync if no sync is in progress, ignores w-org-sync--enabled flag.
Warns if you're offline or if sync is already running."
  (interactive)
  (if (not (w-utils-online-p))
      (w-org-sync--offline-warning)
    (if w-org-sync--inprogress
        (w-org-sync--sync-already-in-progress-warning "r-2-l")
      (w-org-sync--r-2-l))))

(defun w-org-sync-l-2-r ()
  "Trigger l-2-r sync even if auto-syncing is disabled (w-org-sync--enabled nil)
Handy for daily/weekly reviews during which I disable the syncing, do a bunch of
changes locally and want to be sure that the l-2-r runs first.
Warns if you're offline or if sync is already running."
  (interactive)
  (if (not (w-utils-online-p))
      (w-org-sync--offline-warning)
    (if w-org-sync--inprogress
        (w-org-sync--sync-already-in-progress-warning)
      (w-org-sync--l-2-r))))


;;
;; Hooks
(add-hook 'after-save-hook 'w-org-sync--after-save-hook-handler)
(add-hook 'doom-switch-buffer-hook 'w-org-sync--doom-switch-buffer-hook-handler)


;;
;; Tasks
(run-with-timer w-org-sync--r-2-l-delay-after-start
                w-org-sync--r-2-l-interval
                #'w-org-sync--try-r-2-l)


(provide 'wat-org-sync)
