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

(defun my-gtags/post-init-counsel-gtags ()
  ;; this is getting ugly...
  (message "Post init counsel gtags")
  ;; (define-key evil-normal-state-map (kbd "H-.") nil)

  (defun kbd-mac-command (keys)
    "Wraps `kbd' function with Mac OSX compatible Command-key (⌘).
KEYS should be a string such as \"f\" which will be turned into values
such as \"H-f\", \"s-f\", or \"A-f\" depending on the value of
`mac-commmand-modifier' which could be `hyper', `super', or `alt'.
KEYS with a string of \"C-f\" are also valid and will be turned into
values such as \"H-C-f\".
Returns nil if `mac-command-modifier' is set to `none' or something
other than the three sane values listed above."
    (let ((found (assoc mac-command-modifier
                        '((hyper . "H-")
                          (super . "s-")
                          (alt   . "A-")))))
      (when found (kbd (concat (cdr found) keys)))))

  (add-hook 'web-mode-hook
            (lambda ()
              (define-key web-mode-map (kbd-mac-command ".") 'counsel-gtags-dwim)
              (define-key web-mode-map (kbd-mac-command ",") 'counsel-gtags-go-backward)))

  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook
              (define-key evil-normal-state-map (kbd-mac-command ".") 'counsel-gtags-dwim)
              (define-key evil-normal-state-map (kbd-mac-command ",") 'counsel-gtags-go-backward)))

  ;; (with-eval-after-load 'react-mode
  ;;   (define-key evil-normal-state-map (kbd "M-.") 'counsel-gtags-dwim)
  ;;   (define-key evil-normal-state-map (kbd "M-,") 'counsel-gtags-go-backward))

  ;; (with-eval-after-load 'elixir-mode
  ;;   (define-key evil-normal-state-map (kbd "M-.") 'counsel-gtags-dwim)
  ;;   (define-key evil-normal-state-map (kbd "M-,") 'counsel-gtags-go-backward))

  ;; (with-eval-after-load 'ruby-mode
  ;;   (define-key evil-normal-state-map (kbd "M-.") 'counsel-gtags-dwim)
  ;;   (define-key evil-normal-state-map (kbd "M-,") 'counsel-gtags-go-backward))

  ;; (with-eval-after-load 'slime-mode
  ;;   (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition)
  ;;   (define-key evil-normal-state-map (kbd "M-,") 'slime-pop-find-definition-stack))

  ;; I don't want this to override when another layer already defines a reasonable M-.
  ;; (define-key evil-normal-state-map (kbd "M-.") 'counsel-gtags-dwim)

  ;; (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim)
  ;; (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
  ;; (define-key counsel-gtags-mode-map (kbd "M-*") 'counsel-gtags-go-backward)
  ;; (define-key ggtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
  )

;;; packages.el ends here
