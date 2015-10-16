; This is to be able to execute commands without reaching for the alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Clever newlines
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<M-return>") 'open-line-above)

;; Finding files more often than using transpose-words...
(global-set-key (kbd "C-t") 'helm-projectile-find-file)

;; Easier access for commonly used command
(global-set-key (kbd "M-t") 'helm-projectile-grep) ; search in project 

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))


;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(global-set-key (kbd "C-x m") 'copy-sexp-to-repl)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'helm-mini
  "k" 'kill-buffer
  "p" 'helm-projectile-switch-project)

(provide 'key-bindings)
