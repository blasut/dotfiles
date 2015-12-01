(defun open-notes () 
  "Opens the file notes.txt"
  (interactive)
  (switch-to-buffer (find-file-noselect "~/Dropbox/org/notes.org")))

