(setq make-backup-files nil)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(setenv "LANG" "en_US.UTF-8")

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

(setq show-paren-style 'parenthesis) ; highlight entire bracket expression
(show-paren-mode 1)

; Bind esc to newline and indent instead of just newline.
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(define-key global-map (kbd "RET") 'newline-and-indent)

(defadvice yes-or-no-p (around prevent-dialog activate)
             "Prevent yes-or-no-p from activating a dialog"
               (let ((use-dialog-box nil))
                     ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
             "Prevent y-or-n-p from activating a dialog"
               (let ((use-dialog-box nil))
                     ad-do-it))

(setq ring-bell-function 'ignore) ; turn off stupid bell

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(setq inhibit-startup-message t)

(provide 'defaults)
