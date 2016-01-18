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

(define-key evil-normal-state-map (kbd ",") 'evil-ex)

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "f"  'helm-projectile-find-file
  "b"  'helm-mini
  "k"  'kill-buffer
  "p"  'helm-projectile-switch-project
  "x"  'helm-M-x
  "m"  'magit-status
  "n"  'open-notes
  "at"  'org-todo-list
  "s"  'helm-projectile-grep)

;; nnoremap <leader>f :call SelectaFile(".")<cr>
;; nnoremap <leader>gv :call SelectaFile("app/views")<cr>
;; nnoremap <leader>gc :call SelectaFile("app/controllers")<cr>
;; nnoremap <leader>gm :call SelectaFile("app/models")<cr>
;; nnoremap <leader>gh :call SelectaFile("app/helpers")<cr>
;; nnoremap <leader>gl :call SelectaFile("lib")<cr>
;; nnoremap <leader>gp :call SelectaFile("public")<cr>
;; nnoremap <leader>gs :call SelectaFile("public/stylesheets")<cr>
;; nnoremap <leader>gf :call SelectaFile("features")<cr>

(evil-leader/set-key-for-mode 'elixir-mode
  "t"  'alchemist-mix-test
  "gf" 'alchemist-project-toggle-file-and-tests ; TODO: new key
  "gv" 'alchemist-phoenix-find-views
  "gr" 'alchemist-phoenix-router
  "gl" 'alchemist-phoenix-find-channels
  "gm" 'alchemist-phoenix-find-models
  "gc" 'alchemist-phoenix-find-controllers
  "gt" 'alchemist-phoenix-find-templates)

(provide 'key-bindings)
