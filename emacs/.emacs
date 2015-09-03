;; Set path to dependencies
(setq site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(paredit
     markdown-mode
     yasnippet
     smartparens
     elisp-slime-nav
     slime
     ac-slime
     auto-complete
     clojure-mode
     cider
     coffee-mode
     haskell-mode
     php-mode
     pretty-lambdada
     rainbow-delimiters
     rspec-mode
     slime
     smartparens
     undo-tree
     helm
     projectile
     helm-projectile
     rainbow-identifiers
     jade-mode
     slim-mode
     common-lisp-snippets
     simplezen
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'defaults)
(require 'misc)
(require 'setup-paredit)
(require 'setup-lisp)
(require 'key-bindings)
(require 'setup-helm)
(require 'setup-projectile)
(require 'setup-scheme)
(require 'setup-simplezen)
(require 'setup-yasnippet)
(require 'mode-mappings)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
(when is-mac (require 'mac))


;;; Old config below

(require 'pretty-lambdada)
(pretty-lambda-for-modes)

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(smartparens-global-mode t)
(sp-pair "'" nil :actions :rem) ; Remove pairing for qoutes.
(sp-pair "`" nil :actions :rem) ; Remove pairing for backticks.

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; C-stuff
(setq c-default-style "linux"
          c-basic-offset 4)

(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1))) ; Autoindent

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(menu-bar-mode t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#657b83" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "unknown" :family "Monospace")))))
