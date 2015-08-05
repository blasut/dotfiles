(require 'projectile)

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)

(add-to-list 'projectile-globally-ignored-directories "node_modules")
;(add-to-list 'grep-find-ignored-directories "node_modules")

(projectile-global-mode)

(require 'helm-projectile)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

(provide 'setup-projectile)
