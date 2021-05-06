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

;; Try using babel org config with doom
(org-babel-load-file
   (expand-file-name "~/.doom.d/settings.org"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/notes/work")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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


