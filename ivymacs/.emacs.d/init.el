 (setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
; Set up load path
(add-to-list 'load-path settings-dir)

(require 'defaults)

(require 'use-package)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; More configuration goes here
  )

(use-package general :ensure t
  :config
  (general-evil-setup t)

  (general-define-key
   :states '(normal insert emacs)
   :prefix "C-SPC"
   :non-normal-prefix "C-SPC"
   )

  (general-define-key
   :states '(normal motion insert emacs)
   :prefix "SPC"
   "g"  '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "git status")
   )
  )



