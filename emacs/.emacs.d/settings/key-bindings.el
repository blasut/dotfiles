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

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(provide 'key-bindings)
