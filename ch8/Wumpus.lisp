; (load "../ch7/GraphUtil.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		       collect (edge-pair (random-node) (random-node)))))

(defun print-list2 (lst)
  (princ (car lst))
  (if (eq lst ())
    ()
    (print-list2 (cdr lst))))

(defun print-list3 (lst)
  (unless (eq lst ())
    (princ (car lst))
    (print-list3 (cdr lst))))

(defun print-list (lst)
  (loop for a in lst do (print a)))

; Go through edge-list. Remove all edges whose terminus
; is not node. 
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
		       (unless (member node visited)
			 (push node visited)
			 (mapc (lambda edge)
			       (traverse (cdr edge)))
			 (direct-edges node edge-list)))))
    (traverse node))
  visited)

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
			   (let* ((connected (get-connected (car nodes) edge-list))
				  (unconnected (set-difference nodes connected)))
			     (push connected islands)
			     (when unconnected
			       find-island unconnected)))))
    (find-island nodes))
  islands)

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))
