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

(use-package ace-window
  :ensure t
  :defer 1
  :config
  (defhydra hydra-window (:color red
                                 :hint nil)
    "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)
    ("|" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)))
    ("_" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)))
    ("v" split-window-right)
    ("x" split-window-below)
                                        ;("t" transpose-frame "'")
    ;; winner-mode must be enabled
    ("u" winner-undo)
    ("r" winner-redo) ;;Fixme, not working?
    ("o" delete-other-windows :exit t)
    ("a" ace-window :exit t)
    ("f" new-frame :exit t)
    ("s" ace-swap-window)
    ("da" ace-delete-window)
    ("dw" delete-window)
    ("db" kill-this-buffer)
    ("df" delete-frame :exit t)
    ("q" nil)
                                        ;("i" ace-maximize-window "ace-one" :color blue)
                                        ;("b" ido-switch-buffer "buf")
    ("m" headlong-bookmark-jump))
  )


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
   "b"  '(hydra-window/body)
   )
  )



(require 'functions)
(require 'setup-org)
