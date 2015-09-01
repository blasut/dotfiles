(setq initial-scratch-message ";; This but a scratch
")

(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

(fset 'copy-sexp-to-repl
   "\C-xM\367\C-xO\C-y")

(provide 'misc)

