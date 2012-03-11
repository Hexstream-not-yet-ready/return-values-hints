(cl:defpackage #:return-values-hints
  (:use #:cl)
  (:nicknames #:retval-hints)
  (:export #:*retval-hints-table*
           #:retval-hints-table
           #:retval-hints-name
           #:return-values-hint ; and setf
           #:define-return-values-hint
           #:define-return-values-hints

           #:retval-hint-using-table ; and setf
           #:valid-retval-operator-p
           #:valid-retval-hint-p
           #:retval-hints-count
           #:retval-hints-hash-table-mixin
           #:standard-retval-hints-table))
