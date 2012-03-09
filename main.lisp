(in-package #:return-values-hints)

(defvar *return-values-hints* (make-hash-table :test 'eq))

(deftype %setf-function-name ()
  '(cons (eql setf) (cons symbol null)))

(defun return-values-hint (operator &optional (default nil defaultp))
  (etypecase operator
    (symbol (gethash operator
                     *return-values-hints*
                     (if defaultp default '(&rest values))))
    (%setf-function-name '(new-value))))

(defun (setf return-values-hint) (new-hint operator)
  (check-type new-hint (or list (member :implementation-dependent
                                        :non-local-exit)))
  (check-type operator symbol)
  (setf (gethash operator *return-values-hints*) new-hint))

(defmacro define-return-values-hint (operator hint)
  (flet ((expand (operator hint)
           `(setf (return-values-hint ',operator) ',hint)))
    (if (typep operator '(cons (eql or)))
        `(progn ,@(mapcar (lambda (operator)
                            (expand operator hint))
                          (cdr operator)))
        (expand operator hint))))

(defmacro define-return-values-hints (&body operator-hint-pairs)
  `(progn ,@(mapcar
             (lambda (operator-hint-pair)
               (destructuring-bind (operator hint) operator-hint-pair
                 `(define-return-values-hint ,operator ,hint)))
             operator-hint-pairs)))
