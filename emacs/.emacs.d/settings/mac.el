(setq x-select-enable-clipboard t)
      
(setq mac-command-modifier 'meta)  ; set command to meta
(setq mac-option-modifier 'super)  ; set option to super
(setq ns-function-modifier 'hyper) ; set FN to hyper modifier

;; Swedish mac-keyboard alt-keys
(define-key key-translation-map (kbd "s-8") (kbd "["))
(define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "s-9") (kbd "]"))
(define-key key-translation-map (kbd "s-)") (kbd "}"))
(define-key key-translation-map (kbd "s-7") (kbd "|"))
(define-key key-translation-map (kbd "s-/") (kbd "\\"))

;; Open files
(defun mac-open-current-file ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(provide 'mac)
