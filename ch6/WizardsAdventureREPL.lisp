(load "../ch5/WizardsAdventure.lisp")

(defun game-repl ()
  (loop (print (eval (read)))))
