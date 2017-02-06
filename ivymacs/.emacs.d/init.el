 (setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
; Set up load path
(add-to-list 'load-path settings-dir)

(require 'defaults)

(require 'use-package)

(use-package color-theme-solarized :ensure t
  :disabled t
  :init
  ;; to make the byte compiler happy.
  ;; emacs25 has no color-themes variable
  (setq color-themes '())
  :config
  ;; load the theme, don't ask for confirmation
  (load-theme 'solarized t)

  (defun solarized-switch-to-dark ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'dark)
    (enable-theme 'solarized)
    (set-cursor-color "#d33682"))
  (defun solarized-switch-to-light ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'light)
    (enable-theme 'solarized)
    (set-cursor-color "#d33682"))

  (solarized-switch-to-dark))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; More configuration goes here
  )

(use-package magit :ensure t
  :config
  (global-git-commit-mode))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :commands (ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :config
  (ivy-mode 1))

(use-package counsel :ensure t
  :bind*
  (("M-x"     . counsel-M-x))
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package general :ensure t
  :config
  (general-evil-setup t)

  (general-define-key
   :states '(normal)
   :prefix "C-SPC"
   :non-normal-prefix "C-SPC"
   )

  (general-define-key
   :states '(normal motion emacs)
   :prefix "SPC"
   "g"  '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "git status")
   )
  )



(require 'functions)
(require 'setup-org)
