(require 'yasnippet)
(require 'common-lisp-snippets)

(yas-global-mode 1)

;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)


(provide 'setup-yasnippet)
