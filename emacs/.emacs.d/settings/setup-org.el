(require 'org)

(setq org-log-done 'time)

(setq org-agenda-files (list "~/Dropbox/org/work.org"
                             "~/Dropbox/org/home.org"))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Persist clock history
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Default dir for notes.
(setq org-default-notes-file "~/Dropbox/org/notes.org")

(provide 'setup-org)
