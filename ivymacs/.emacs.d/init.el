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

(use-package which-key :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.5
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.5
        which-key-min-display-lines 7))

(use-package ace-window :ensure t
  :commands
  ace-window
  :config
  (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
  (setq aw-ignore-current t))

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
   ;; Special keys
   "SPC" '(counsel-M-x)

   ;; B
   "b"  '(:ignore t :which-key "Buffer")
   "bb" '(ivy-switch-buffer :which-key "Change buffer")

   ;; E
   "e"  '(:ignore t :which-key "Eval")
   "eb" '(eval-buffer :which-key "Eval Buffer")

   ;; F
   "f"  '(:ignore t :which-key "File")
   "ff" '(counsel-find-file :which-key "Find file")  
   "fs" '(save-buffer :which-key "Save")  

   ;; G
   "g"  '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "git status")

   ;; H
   "h"  '(:ignore t :which-key "Help")
   "hdf" '(counsel-describe-function :which-key "Describe function")
   "hdv" '(counsel-describe-variable :which-key "Describe variable")
   "hdk" '(describe-key :which-key "Describe key")

   ;; P
   "p"  '(:ignore t :which-key "Projectile")

   ;; W
   "w"  '(:ignore t :which-key "Window")
   "ww" '(other-window :which-key "Switch window")
   "wd" '(delete-window :which-key "Delete window")
   "wD" '(delete-other-windows :which-key "Delete other windows")
   "wa" '(ace-window :which-key "Ace window")
   "w-" '(split-window-below :which-key "Split window below")
   "w/" '(split-window-right :which-key "Split window right")
   )
  )

(require 'functions)
(require 'setup-org)
