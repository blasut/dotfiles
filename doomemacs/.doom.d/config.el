;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Mono" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/notes/work")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

; TODO: use these AND SPC
(setq ;doom-leader-key ","
      doom-localleader-key "\\")

; org-narrow-to-subtree
; widen
;   (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)
(after! org
  (map! :map org-mode-map
        :localleader
        "N" #'org-store-link
        "n" #'org-narrow-to-subtree
        "w" #'widen)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d!)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
  (setq org-log-into-drawer t)
  )

;; (winum-select-window-1 &optional ARG)
(map!
  (:when (featurep! :ui workspaces)
    :n "s-1"   #'winum-select-window-1
    :n "s-2"   #'winum-select-window-2
    :n "s-3"   #'winum-select-window-3
    :n "s-4"   #'winum-select-window-4
    :n "s-5"   #'winum-select-window-5
    :n "s-6"   #'winum-select-window-6
    :n "s-7"   #'winum-select-window-7
    :n "s-8"   #'winum-select-window-7
    :n "s-9"   #'winum-select-window-8

    :g "M-1"   #'+workspace/switch-to-0
    :g "M-2"   #'+workspace/switch-to-1
    :g "M-3"   #'+workspace/switch-to-2
    :g "M-4"   #'+workspace/switch-to-3
    :g "M-5"   #'+workspace/switch-to-4
    :g "M-6"   #'+workspace/switch-to-5
    :g "M-7"   #'+workspace/switch-to-6
    :g "M-8"   #'+workspace/switch-to-7
    :g "M-9"   #'+workspace/switch-to-8
    :g "M-0"   #'+workspace/switch-to-final
    ))

;; Remap the regular search to use swiper
(map! :n "/" #'swiper-isearch)

;; Remap leader . to project search instead
(map! :leader
        "." #'+ivy/projectile-find-file
        )

;; Remap SPC SPC to M-x like spacemacs
(map! :leader
        "SPC" #'counsel-M-x
        )

(map!
 :g "C-k" 'sp-kill-hybrid-sexp
 :g "M-k" 'sp-backward-kill-sexp
 )

;; Allow me to mash the keys in any order yes good
(setq evil-escape-unordered-key-sequence t)

;; C-o to open the hydra
(setq ivy-read-action-function #'ivy-hydra-read-action)



;; To compare the init.example.el and my current init.el
(defun blasut/compare-init ()
  "Compares the init.example.el file with the current init.el"
  (interactive)
  (ediff "~/dotfiles/doomemacs/.doom.d/init.el" "~/dotfiles/doomemacs/.emacs.d/init.example.el"))

;; setup lsp clangd defaults
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))


(after! lsp
  (setq lsp-treemacs-sync-mode 1)
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace)
  )

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(defun blasut/org-present-hide-cursor ()
  "Hide the cursor for current window.

  Stolen from the org-present library"
  (interactive)
  (internal-show-cursor (selected-window) nil))

(defun blasut/org-present-show-cursor ()
  "Show the cursor for current window."
  (interactive)
  (internal-show-cursor (selected-window) t))

(defun blasut/presentation-setup ()
  (doom-big-font-mode)
  (org-display-inline-images))

(defun blasut/presentation-end ()
  (doom-big-font-mode -1))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . blasut/presentation-setup)
         (org-tree-slide-stop . blasut/presentation-end))

  :custom
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  ;; make sure images don't take up too much space
  (org-image-actual-width nil)

  :bind (("<f5>" . org-tree-slide-mode)
         (:map org-tree-slide-mode-map
          ("<mouse-3>" . org-tree-slide-move-previous-tree)
          ("<mouse-4>" . org-tree-slide-move-next-tree)))

  )


(use-package run-command
  :bind (("C-c c" . run-command)
         ("C-c C-c" . run-command)
         ("C-c d" . recompile)
         ("C-c C-d" . recompile)
         )
  :config
  (setq compilation-scroll-output t)
  (setq scroll-conservatively 101)
  )

(defun run-command-recipe-local ()
    (list
     (list :command-name "say-hello"
           :command-line "echo Hello, World!")

     )
  )

(defun run-command-recipe-package-json--get-scripts (package-json-file)
  "Extract NPM scripts from `package-json-file'."
  (with-temp-buffer
    (insert-file-contents package-json-file)
    (let* ((json-data (json-parse-buffer))
           (script-hash (gethash "scripts" json-data))
           (scripts '()))
      (maphash (lambda (key _value) (push key scripts)) script-hash)
      scripts)))

(defun run-command-recipe-package-json ()
  (when-let* ((find-package-json
               (locate-dominating-file default-directory "package.json"))
              (project-dir
               (concat (projectile-project-root) "/" "web" "/"))
              (scripts
               (run-command-recipe-package-json--get-scripts (concat project-dir "package.json")))
              (script-runner
               (if (file-exists-p (concat project-dir "yarn.lock")) "yarn" "npm")))
    (mapcar (lambda (script)
              (list :command-name script
                    :command-line (concat "docker-compose exec web " script-runner " run " script)
                    :display script
                    :working-dir project-dir))
            scripts)))

(defun run-command-recipe-mix ()
  (when-let* ((project-dir
               ;; check if there is a mix file somwewhere
               (locate-dominating-file default-directory "mix.exs"))
              ;; then we'll use the projectile project root for monorepos. Should work with non-mono repos aswell
              (working-dir (projectile-project-root)))
    (list
     ;; make commands for docker project
     (list :command-name "format project"
           :command-line "docker-compose exec backend mix format"
           :working-dir working-dir
           )

     (list :command-name "mix tests"
           :command-line "docker-compose exec backend mix test"
           :working-dir working-dir
           )

     (list :command-name "mix tests.watch failed"
           :command-line "docker-compose exec backend mix test.watch --failed"
           :working-dir working-dir
           )

     )    )
  )


;; Let's try tabnine
                                        ;(use-package! company-tabnine
                                        ;  :after company
                                        ;  :config
                                        ;  (setq company-backends '(company-tabnine)))
                                        ;  ;(cl-pushnew 'company-tabnine (default-value 'company-backends)))
                                        ;
                                        ;(add-hook! prog-mode-hook
                                        ;   (setq company-backends '(company-tabnine)))

;; (add-hook! lsp-after-open (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)))

;; (set-company-backend! 'conf-mode
;;                    'company-tabnine 'company-capf 'company-dabbrev-code 'company-yasnippet)
;;                  (set-company-backend! 'prog-mode
;;                    'company-tabnine 'company-capf 'company-yasnippet)))


