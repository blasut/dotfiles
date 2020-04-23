;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/notes/work")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

; TODO: use these AND SPC
(setq ;doom-leader-key ","
      doom-localleader-key "\\")

; org-narrow-to-subtree
; widen
;   (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)
(after! org
  (map! :map org-mode-map
        :localleader
        "N" #'org-store-link
        "n" #'org-narrow-to-subtree
        "w" #'widen))

;; (winum-select-window-1 &optional ARG)
(map!
  (:when (featurep! :ui workspaces)
    :g "M-1"   #'winum-select-window-1
    :g "M-2"   #'winum-select-window-2
    :g "M-3"   #'winum-select-window-3
    :g "M-4"   #'winum-select-window-4
    :g "M-5"   #'winum-select-window-5
    :g "M-6"   #'winum-select-window-6
    :g "M-7"   #'winum-select-window-7
    :g "M-8"   #'winum-select-window-7
    :g "M-9"   #'winum-select-window-8
    ))

;; Remap the regular search to use swiper
(map! :n "/" #'swiper-isearch)

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
