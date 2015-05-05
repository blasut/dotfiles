(require 'projectile)

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'git)

(projectile-global-mode)

(require 'helm-projectile)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

(provide 'setup-projectile)
