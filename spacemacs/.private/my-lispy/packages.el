;;; packages.el --- my-lispy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-lispy-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-lispy/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-lispy/pre-init-PACKAGE' and/or
;;   `my-lispy/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-lispy-packages
  '(evil-lispy)
  "The list of Lisp packages required by the my-lispy layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun my-lispy/init-evil-lispy ()
  (use-package evil-lispy
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
      (add-hook 'clojure-mode-hook #'evil-lispy-mode))
    :config
    (progn
      ;; Bind back the original bindings
      (define-key lispy-mode-map "o" 'special-lispy-other-mode)
      (define-key lispy-mode-map "d" 'special-lispy-different)
      (define-key lispy-mode-map "i" 'special-lispy-tab)
      (define-key lispy-mode-map "f" 'special-lispy-flow)

      ;; My custom bindings
      (define-key evil-normal-state-map (kbd "u") 'lispy-undo)
      (define-key evil-normal-state-map (kbd "?") 'evil-lispy-show-help)
      (define-key evil-lispy-state-map (kbd "?") 'evil-lispy-show-help)

      (define-key lispy-mode-map (kbd "C-u") 'lispy-undo))))
;;; packages.el ends here
