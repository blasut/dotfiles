(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
                                        ; Set up load path
(add-to-list 'load-path settings-dir)

(require 'defaults)
(require 'functions)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(require 'use-package)

(use-package color-theme-solarized :ensure t
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
  (progn
    (evil-mode 1)
    (use-package evil-escape :ensure t
      :config
      (progn
        (evil-escape-mode 1)
        (setq-default evil-escape-key-sequence "fj")))))

(use-package magit :ensure t
  :commands magit-status
  :config
  (progn
    (global-git-commit-mode)
    (setq magit-completing-read-function 'ivy-completing-read)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :commands (ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :bind*
  (("C-s"     . swiper))
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
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom)
    ;; simple then alphabetic order.
    (setq which-key-sort-order 'which-key-prefix-then-key-order)
    (setq which-key-popup-type 'side-window
          which-key-side-window-max-height 0.5
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.5
          which-key-min-display-lines 7)))

(use-package ace-window :ensure t
  :commands
  ace-window
  :config
  (progn
    (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
    (setq aw-ignore-current t)))

(use-package avy :ensure t
  :commands (avy-goto-word-or-subword-1
             avy-goto-word-1
             avy-goto-char-in-line
             avy-goto-line)
  :config
  (progn
    (setq avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n ?m))
    (setq avy-styles-alist
          '((avy-goto-char-in-line . post)
            (avy-goto-word-or-subword-1 . post)))))

(use-package projectile
  :ensure t
  :init
  (progn
    (setq projectile-mode-line nil)
    (projectile-global-mode)
    (setq projectile-project-root-files-bottom-up
          '(".git" ".projectile"))
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching nil)
    (setq projectile-verbose nil))
  :config
  (use-package counsel-projectile :ensure t
    :config
    (counsel-projectile-on))
  )

(use-package erc
  :config
  (progn
    (setq erc-hide-list '("PART" "QUIT" "JOIN"))
    (setq erc-autojoin-channels-alist '(("freenode.net"
                                         "#org-mode"
                                         "#hacklabto"
                                         "#emacs"
                                         "#emacs-beginners"
                                         "#emacs-ops"
                                         "#lisp"))
          erc-server "irc.freenode.net"
          erc-nick "blasut")))

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
   "/" '(swiper)
   )

  (general-define-key
   :states '(normal motion emacs)
   :prefix "SPC"
   ;; Flat keys
   "/"    '(counsel-git-grep :which-key "Find in files")
   "TAB"  '(projectile-project-buffers-other-buffer :which-key "Next buffer")

   ;; Special keys
   "SPC"  '(counsel-M-x)

   ;; B
   "b"    '(:ignore t :which-key "Buffer")
   "bb"   '(ivy-switch-buffer :which-key "Change buffer")
   "bd"   '(ace-delete-window :which-key "Delete buffer")
   "bn"   '(next-buffer :which-key "Next buffer")
   "bp"   '(previous-buffer :which-key "Previous buffer")
   "bR"   '(revert-buffer :which-key "Revert buffer")

   ;; E
   "e"    '(:ignore t :which-key "Eval")
   "eb"   '(eval-buffer :which-key "Eval Buffer")

   ;; F
   "f"    '(:ignore t :which-key "File")
   "fc"   '(open-config :which-key "Open init.el file")
   "ff"   '(counsel-find-file :which-key "Find file")
   "fl"   '(counsel-locate :which-key "Locate")
   "fs"   '(save-buffer :which-key "Save")
   "fr"   '(counsel-recentf :which-key "Recent files")
   "fR"   '(rename-current-buffer-file :which-key "Rename file")

   ;; G
   "g"    '(:ignore t :which-key "Git")
   "gs"   '(magit-status :which-key "git status")

   ;; H
   "h"    '(:ignore t :which-key "Help")
   "hi"   '(info :which-key "Info")
   "hv"   '(ivy-help :which-key "Ivy")
   "hdb"  '(counsel-descbinds :which-key "Describe bindings")
   "hdf"  '(counsel-describe-function :which-key "Describe function")
   "hdk"  '(describe-key :which-key "Describe key")
   "hdv"  '(counsel-describe-variable :which-key "Describe variable")
   "hdm"  '(describe-mode :which-key "Describe mode")

   ;; J
   "j"    '(:ignore t :which-key "Jump")
   "jj"   '(avy-goto-char :which-key "Char")
   "jl"   '(avy-goto-line :which-key "Line")
   "jw"   '(avy-goto-word-0 :which-key "Word")

   ;; P
   "p"    '(:ignore t :which-key "Projects")
   "pb"   '(counsel-projectile-switch-to-buffer :which-key "Switch buffer")
   "pd"   '(counsel-projectile-find-dir :which-key "Find dir")
   "pf"   '(counsel-projectile-find-file :which-key "Find file")
   "pF"   '(projectile-find-file-in-known-projects :which-key "Find file in all projects")
   "pp"   '(counsel-projectile-switch-project :which-key "Switch project")
   "pr"   '(projectile-recentf :which-key "Recent")
   "p/"   '(counsel-git-grep :which-key "Search")
   "ps"   '(counsel-git-grep :which-key "Search")

   ;; S
   "s"    '(:ignore t :which-key "Search")
   "ss"   '(swiper :which-key "Search in file")
   "sS"   '(swiper-all :which-key "Search in all buffers")
   "sp"   '(counsel-git-grep :which-key "Grep in project")
   "sj"   '(counsel-imenu :which-key "Imenu")

   ;; W
   "w"    '(:ignore t :which-key "Window")
   "ww"   '(other-window :which-key "Switch window")
   "wd"   '(ace-delete-window :which-key "Delete window")
   "wD"   '(delete-other-windows :which-key "Delete other windows")
   "wa"   '(ace-window :which-key "Ace window")
   "ws"   '(split-window-below :which-key "Split window below")
   "w-"   '(split-window-below :which-key "Split window below")
   "wS"   '(split-window-right :which-key "Split window right")
   "w/"   '(split-window-right :which-key "Split window right")
   "wh"   '(windmove-left :which-key "Window left")
   "wj"   '(windmove-down :which-key "Window down")
   "wk"   '(windmove-up :which-key "Window up")
   "wl"   '(windmove-right :which-key "Window right")

   ;; X
   "x"    '(:ignore t        :which-key "Text")
   "xd"   '(delete-trailing-whitespace :which-key "Delete trailing whitespace")
   "xs"   '(sort-lines :which-key "Sort lines")
   "xu"   '(lower-case :which-key "Lower case")
   "xU"   '(upper-case :which-key "Upper case")
   "xc"   '(count-words :which-key "Count words")
   ;; XA
   "xa"   '(:ignore t       :which-key "Align")
   "xa'"  '(align-by-single  :which-key "'")
   "xa="  '(align-by-=       :which-key "=")
   "xa("  '(align-by-lparen  :which-key "(")
   "xa)"  '(align-by-lparen  :which-key ")")
   "xa:"  '(align-by-:       :which-key ":")
   )
  )

(require 'setup-org)
