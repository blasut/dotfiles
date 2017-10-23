;;; packages.el --- my-lispyville layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: jite <jite@jites-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
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
;; added to `my-lispyville-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-lispyville/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-lispyville/pre-init-PACKAGE' and/or
;;   `my-lispyville/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-lispyville-packages
  '(lispyville
    lispy)
  "The list of Lisp packages required by the my-lispyville layer.

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

(defun my-lispyville/init-lispyville ()
  (use-package lispyville
    :defer t
    :init
    (my-lispyville//add-hooks-for-lispyville)
    :config
    (progn (my-lispyville//set-lispyville-keytheme)
           (spacemacs|diminish evil-lispy-mode " Ⓛ" " L"))
    ))

(defun my-lispyville//add-hooks-for-lispyville ()
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'ielm-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispyville-mode 1)))
  ;; (add-hook 'spacemacs-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'scheme-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda () (lispyville-mode 1))))

(defun my-lispyville//set-lispyville-keytheme ()
  (with-eval-after-load 'lispyville
    (lispyville-set-key-theme '(operators
                                additional-movement
                                slurp/barf-lispy
                                additional
                                mark))
    (spacemacs/set-leader-keys "o." 'lispyville-mode)))

(defun my-lispyville/pre-init-smartparens ()
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil))

(defun my-lispyville/post-init-smartparens ()
  (show-smartparens-global-mode -1))

(defun my-lispyville/post-init-evil ()
  (setq evil-move-beyond-eol t))

;;; packages.el ends here
