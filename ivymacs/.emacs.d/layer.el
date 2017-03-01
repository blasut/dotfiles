(defun layer-name (name)
  (intern (concat "my-layer/" (symbol-name name))))

(defmacro define-layer (name &rest args)
  (let ((vars (append (plist-get args :vars) '((disabled nil))))
        (config (plist-get args :config))
        (func-name (layer-name name)))
    `(cl-defun ,func-name (&key ,@vars)
       (unless disabled
         (progn ,config)))))

(define-layer git 
  :vars ((git-completion-mode 'ivy-completing-read)
         (global-git-mode t))
  :config (use-package magit :ensure t
            :commands magit-status
            :config
            (progn
              (use-package evil-magit
                :ensure t      
                :after evil)
              (when global-git-mode (global-git-commit-mode))
              (setq magit-completing-read-function git-completion-mode))))


(defmacro use-layer (name &rest args)
  (let ((vars (plist-get args :vars))
        (func-name (layer-name name)))
    `(,func-name ,@vars)))


(use-layer git)

(use-layer git :vars (git-completion-mode 'ivy-completing-read
                      disabled t))
(define-layer git 
  :vars ((git-completion-mode 'ivy-completing-read)
         (global-git-mode t))
  :config (use-package magit :ensure t
            :commands magit-status
            :config
            (progn
              (use-package evil-magit
                :ensure t      
                :after evil)
              (when global-git-mode (global-git-commit-mode))
              (setq magit-completing-read-function git-completion-mode))))

(cl-defun my-layer/git
    (&key
     (git-completion-mode 'ivy-completing-read)
     (global-git-mode t)
     (disabled nil))
  (unless disabled
    (progn
      (use-package magit :ensure t :commands magit-status :config
        (progn
          (use-package evil-magit :ensure t :after evil)
          (when global-git-mode
            (global-git-commit-mode))
          (setq magit-completing-read-function git-completion-mode))))))


(defmacro use-layer (name &rest args)
  (let ((vars (plist-get args :vars))
        (func-name (layer-name name)))
    (print vars)
    `(,func-name ,@vars)))

use-layer

use-layer

use-layer



(use-layer git :vars (git-completion-mode 'ivy-completing-read
                      disabled t))


(git-completion-mode (quote ivy-completing-read) disabled t)


((git-completion-mode (quote ivy-completing-read)) (disabled t))



((lambda ()
   (let ((vars '((git-completion-mode (quote ivy-completing-read)) (disabled t))))
     ;; should be
     ;; :car cdr :car cdr
     ;; totally flat list?
     (print (apply #'append vars))
     (print (mapcan
             (lambda (var)
               (print (type-of var))
               (print (type-of (car var)))
               (list (make-symbol (car var))) )
             vars)))))


(git-completion-mode (quote ivy-completing-read) disabled t)

cons

symbol


(define-layer git 
  :config (use-package magit :ensure t
            :commands magit-status
            :config
            (progn
              (use-package evil-magit
                :ensure t      
                :after evil)
              (when global-git-mode (global-git-commit-mode))
              (setq magit-completing-read-function git-completion-mode))))
