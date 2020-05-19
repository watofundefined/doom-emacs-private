;;; wat-keybindings.el -*- lexical-binding: t; -*-
;;;
;;; Warning: I might be using an un-used letter today but I could be overriding
;;; Doom's maps tomorrow when:
;;; - Doom modifies the default map
;;; - I enable a sub-module feature which extends the map

;;
;; Org stuff
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


(provide 'wat-keybindings)
