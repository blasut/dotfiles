;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/notes")

;; Try using babel org config with doom
(org-babel-load-file
 (expand-file-name "~/.doom.d/settings.org"))


;; C-o to open the hydra
(setq ivy-read-action-function #'ivy-hydra-read-action)

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
              (project-dir (projectile-project-root))
              (scripts
               (run-command-recipe-package-json--get-scripts (concat project-dir "package.json")))
              (script-runner
               (if (file-exists-p (concat project-dir "yarn.lock")) "yarn" "npm")))
    (mapcar (lambda (script)
              (list :command-name script
                    :command-line (concat script-runner " run " script)
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


