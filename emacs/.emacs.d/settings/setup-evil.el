(require 'evil)
(require 'evil-leader)
(require 'evil-matchit)

(global-evil-matchit-mode 1)

(global-evil-leader-mode)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'helm-mini
  "k" 'kill-buffer)

(evil-mode 1)

(add-hook 'Emacs-Lisp #'evil-cleverparens-mode)
(add-hook 'Lisp #'evil-cleverparens-mode)
(add-hook 'Clojure #'evil-cleverparens-mode)

(provide 'setup-evil)

