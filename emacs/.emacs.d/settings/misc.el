(setq initial-scratch-message ";; This but a scratch
")

(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

(fset 'copy-sexp-to-repl
   [?\C-  ?\M-x ?m ?a ?r ?k ?s backspace ?- ?s ?e ?x ?c ?p backspace backspace ?p return ?\M-w ?\C-x ?o ?\C-y])


(provide 'misc)

