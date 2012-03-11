(in-package #:return-values-hints)

(defclass retval-hints-table () ())

(defvar *retval-hints-tables* (make-hash-table :test 'eq))

(defun find-retval-hints (hints-table-designator &key (errorp t) (recursivep t))
  (symbol-macrolet ((it hints-table-designator))
    (etypecase it
      (retval-hints-table it)
      (symbol
       (let ((found (or (gethash it *retval-hints-tables*)
                        (when errorp
                          (error "No ~S named ~S."
                                 'retval-hints-table it)))))
         (if recursivep
             (find-retval-hints found)
             found))))))

(defun (setf find-retval-hints) (hints-table name)
  (check-type hints-table (or retval-hints-table symbol))
  (check-type name (and symbol (not null)))
  (setf (gethash table-name *retval-hints-tables*) hints-table))

(deftype %setf-function-name ()
  '(cons (eql setf) (cons symbol null)))

(defgeneric retval-hints-name (hints-table)
  (:method ((hints-table retval-hints-table))
    nil))

(defclass retval-hints-name-mixin ()
  ((name :initarg :name
         :accessor retval-hints-name)))

(defgeneric (setf retval-hints-name) (new-name hints-table)
  (:method (new-name (hints-table retval-hints-table))
    (cerror "Keep the old name and continue."
            "Hints table ~S doesn't support ~S."
            hints-table
            '(setf retval-hints-name))
    new-name))

(defgeneric retval-hint-using-table (table operator &key)
  (:method ((table retval-hints-table) (operator cons) &key)
    (if (typep operator '%setf-function-name)
        '(new-value)
        (error "Unexpected non-setf cons operator ~S."
               operator))))

(defgeneric (setf retval-hint-using-table) (new-hint table operator &key))

(defgeneric valid-retval-operator-p (hints-table operator)
  (:method ((table retval-hints-table) operator)
    nil)
  (:method ((table retval-hints-table) (operator symbol))
    t))

(defgeneric valid-retval-hint-p (hints-table hint)
  (:method ((table retval-hints-table) hint)
    (typep hint '(or list (member
                           :implementation-dependent
                           :non-local-exit)))))

(defgeneric retval-hints-count (table)
  (:method ((table retval-hints-table))
    (values nil nil)))

(defun %format-hints-count (destination count knownp)
  (multiple-value-call #'format
    destination
    (if knownp
        (values "(~A~D entr~:@P)"
                (ecase knownp
                  (t "")
                  (min ">=")
                  (max "<=")
                  (~ "~"))
                count)
        "(? entries)")))

(defmethod print-object ((table retval-hints-table) stream)
  (multiple-value-bind (count knownp) (retval-hints-count table)
    (if knownp
        (print-unreadable-object (table stream :type t)
          (%format-hints-count stream count knownp))
        (call-next-method))))

(defclass retval-hints-hash-table-mixin ()
  ((hash :reader %hash
         :initform (make-hash-table :test 'equal))))

(defmethod retval-hint-using-table ((table retval-hints-hash-table-mixin)
                                    (operator symbol)
                                    &key)
  (gethash operator (%hash table)))

(defmethod (setf retval-hint-using-table) (new-hint
                                           (table retval-hints-hash-table-mixin)
                                           (operator symbol)
                                           &key)
  (setf (gethash operator (%hash table)) new-hint))

(defmethod retval-hints-count ((hints-table retval-hints-hash-table-mixin))
  (values (hash-table-count (%hash hints-table)) t))


(defclass standard-retval-hints-table (retval-hints-hash-table-mixin
                                       retval-hints-table)
  ())

(defvar *retval-hints-table* (make-instance 'standard-retval-hints-table))

(defun retval-hint (operator &rest keys
                    &key (table *retval-hints-table*)
                    errorp &allow-other-keys)
  (flet ((lookup ()
           (apply #'retval-hint-using-table table operator
                  :allow-other-keys t :table nil :errorp nil keys)))
    (if errorp
        (multiple-value-call
            (lambda (hint hintp &rest keys)
              (if hintp
                  (apply #'values hint hintp keys)
                  (error "No retval-hint found in ~S for ~S and errorp."
                         table operator)))
          (lookup)))))

(defun (setf retval-hint) (new-hint operator &rest keys
                           &key (table *retval-hints-table*)
                           &allow-other-keys)
  (setf (apply #'retval-hint-using-table table operator
               :allow-other-keys t :table nil keys)
        new-hint))

(defmacro define-retval-hint (operator hint)
  (flet ((expand (operator hint)
           `(setf (retval-hint ',operator) ',hint)))
    (if (typep operator '(cons (eql or)))
        `(progn ,@(mapcar (lambda (operator)
                            (expand operator hint))
                          (cdr operator)))
        (expand operator hint))))

(defmacro define-retval-hints (&body operator-hint-pairs)
  `(progn ,@(mapcar
             (lambda (operator-hint-pair)
               (destructuring-bind (operator hint) operator-hint-pair
                 `(define-retval-hint ,operator ,hint)))
             operator-hint-pairs)))

(defmacro define-retval-hints-table (name ))
