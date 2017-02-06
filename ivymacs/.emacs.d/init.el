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

(use-package avy :ensure t
  :commands (avy-goto-word-or-subword-1
             avy-goto-word-1
             avy-goto-char-in-line
             avy-goto-line)
  :config
  (setq avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n ?m))
  (setq avy-styles-alist
        '((avy-goto-char-in-line . post)
          (avy-goto-word-or-subword-1 . post))))

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
   ;; Flat keys
   "/" '(counsel-git-grep :which-key "Find in files")
   "TAB" '(next-buffer :which-key "Next buffer")
   
   ;; Special keys
   "SPC" '(counsel-M-x)

   ;; B
   "b"  '(:ignore t :which-key "Buffer")
   "bb" '(ivy-switch-buffer :which-key "Change buffer")
   "bd" '(ace-delete-window :which-key "Delete buffer")
   "bn" '(next-buffer :which-key "Next buffer")
   "bp" '(previous-buffer :which-key "Previous buffer")
   "bR" '(revert-buffer :which-key "Revert buffer")

   ;; E
   "e"  '(:ignore t :which-key "Eval")
   "eb" '(eval-buffer :which-key "Eval Buffer")

   ;; F
   "f"  '(:ignore t :which-key "File")
   "ff" '(counsel-find-file :which-key "Find file")  
   "fs" '(save-buffer :which-key "Save")  
   "fl" '(counsel-locate :which-key "Locate")  

   ;; G
   "g"  '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "git status")

   ;; H
   "h"  '(:ignore t :which-key "Help")
   "hi"  '(ivy-help :which-key "Ivy")
   "hdf" '(counsel-describe-function :which-key "Describe function")
   "hdv" '(counsel-describe-variable :which-key "Describe variable")
   "hdk" '(describe-key :which-key "Describe key")

   ;; P
   "p"  '(:ignore t :which-key "Projects")
   "pf" '(counsel-git :which-key "Projects")

   
   "w"  '(:ignore t :which-key "Window")
   "ww" '(other-window :which-key "Switch window")
   "wd" '(ace-delete-window :which-key "Delete window")
   "wD" '(delete-other-windows :which-key "Delete other windows")
   "wa" '(ace-window :which-key "Ace window")
   "w-" '(split-window-below :which-key "Split window below")
   "w/" '(split-window-right :which-key "Split window right")
   )
  )

(require 'functions)
(require 'setup-org)
