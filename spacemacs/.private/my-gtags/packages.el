;;; packages.el --- my-gtags layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Christopher <box@Sup.fritz.box>
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
;; added to `my-gtags-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-gtags/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-gtags/pre-init-PACKAGE' and/or
;;   `my-gtags/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-gtags-packages
  '(counsel-gtags)
  "The list of Lisp packages required by the my-gtags layer.

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

(defun my-gtags/init-counsel-gtags ()
  (use-package counsel-gtags
    :init
    (add-hook 'prog-mode-hook 'counsel-gtags-mode)
    :config
    (progn
      (spacemacs|diminish counsel-gtags-mode " Ⓖ" " G")
      (add-hook 'counsel-gtags-mode-hook #'my-gtags/setup-global-bindings-for-tags)
      (my-gtags/define-keys-for-mode 'js2-mode)
      (my-gtags/define-keys-for-mode 'react-mode))))

(defun my-gtags/setup-global-bindings-for-tags ()
  (define-key evil-normal-state-map (kbd "M-.") 'counsel-gtags-dwim)
  (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
  (define-key counsel-gtags-mode-map (kbd "M-*") 'counsel-gtags-go-backward)
  ;; (define-key ggtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
  )

(defun my-gtags/define-keys-for-mode (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    "gc" 'counsel-gtags-create-tags
    "gf" 'counsel-gtags-find-file
    "gd" 'counsel-gtags-find-definition
    "gg" 'counsel-gtags-dwim
    "gr" 'counsel-gtags-find-reference
    "gn" 'counsel-gtags-go-forward
    "gp" 'counsel-gtags-go-backward
    "gs" 'counsel-gtags-find-symbol
    "gG" nil))

;;; packages.el ends here
