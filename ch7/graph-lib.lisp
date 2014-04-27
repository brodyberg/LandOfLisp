(defparameter *max-label-length* 30)

(defun dot-label (expression)
  (if expression
    (let ((s (write-to-string expression :pretty nil)))
      (if (> (length s) *max-label-length*)
	(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	s))
    ""))

(defun dot-name (expression)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string expression)))

(defparameter *label-start* "[label=\"")
(defparameter *label-end* "\"];")

(defun print-label (text)
  (princ (concatenate 'string *label-start* text *label-end*)))
  
(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (print-label (dot-label node)))
	nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (print-label (dot-label (cdr edge))))
		(cdr node)))
	edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		    fname
		    :direction :output
		    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))


