(in-package #:return-values-hints)

(define-return-values-hints
  (* (product))
  (+ (sum))
  (- (difference)) ; with one arg: (- (negation)) ; notable?
  (/ (quotient))   ; with one arg: (/ (reciprocal)) ; notable?
  (/= (none-equal-p)) ; notable?
  (1+ (successor))
  (1- (predecessor))
  (< (generalized-boolean))  ; (strictly-increasing-order-p)
  (<= (generalized-boolean)) ; (increasing-order-p)
  (= (generalized-boolean))  ; (all-equal-p)
  (> (generalized-boolean))  ; (strictly-decreasing-order-p)
  (>= (generalized-boolean)) ; (decreasing-order-p)
  ((or abort
       muffle-warning)
   :non-local-exit)
  (abs (absolute-value))
  (acons (extended-alist))
  (acos (arc-cosine))
  (acosh (hyperbolic-cosine))
  ((or add-method
       remove-method)
   (generic-function-given))
  (adjoin (extended-list))
  (adjust-array (adjusted-array))
  (adjustable-array-p (adjustablep))
  (allocate-instance (new-uninitialized-instance))
  (alpha-char-p (generalized-boolean))
  (alphanumericp (generalized-boolean))
  (and (&rest last-form-values/nil/t-if-no-forms))
  (append (appended-lists))
  ((or apply
       funcall
       multiple-value-call)
   (&rest function-values))
  (apropos ())
  (apropos-list (symbols))
  (aref (element))
  (arithmetic-error-operands (operands))
  (arithmetic-error-operation (operation))
  (array-dimension (dimension-ignoring-fill-pointer))
  (array-dimensions (dimensions-list))
  (array-displacement (displaced-to displaced-index-offset)) ; notable?
  (array-element-type (typespec))
  (array-has-fill-pointer-p (generalized-boolean))
  (array-in-bounds-p (generalized-boolean))
  (array-rank (number-of-dimensions))
  (array-row-major-index (flat-index-ignoring-fill-pointer))
  (array-total-size (total-size))
  (arrayp (generalized-boolean))
  (ash (shifted-integer))
  (asin (arc-sine))
  (asinh (hyperbolic-arc-sine))
  (assert ((eql nil)))
  ((or assoc
       assoc-if
       assoc-if-not)
   (entry/nil))
  (atan (arc-tangent))
  (atanh (hyperbolic-arc-tangent))
  (atom (generalized-boolean))
  ((or bit
       sbit)
   (element-bit))
  ((or bit-and
       bit-andc1
       bit-andc2
       bit-eqv
       bit-ior
       bit-nand
       bit-nor
       bit-not
       bit-orc1
       bit-orc2
       bit-xor)
   ([new/modified]-bit-array)) ;notable?
  (bit-vector-p (generalized-boolean))
  (block (&rest progn-values))
  (boole (result-integer))
  ((or both-case-p
       lower-case-p
       both-case-p)
   (generalized-boolean))
  (boundp (generalized-boolean))
  (break ((eql nil)))
  (broadcast-stream-streams (output-streams))
  (butlast (truncated-list-copy))
  (byte (bytespec))
  (byte-position (position))
  (byte-size (number-of-bits))
  ((or caaaar
       caaadr
       caaar
       caadar
       caaddr
       caadr
       caar
       cadaar
       cadadr
       cadar
       caddar
       cadddr
       caddr
       cadr
       car
       cdaaar
       cdaadr
       cdaar
       cdadar
       cdaddr
       cdadr
       cdar
       cddaar
       cddadr
       cddar
       cdddar
       cddddr
       cddr
       cdr)
   (object))
  (call-method (&rest method-values))
  (call-next-method (&rest next-method-values))
  ((or case
       ccase
       ecase
       typecase
       ctypecase
       etypecase)
   (&rest matching-clause-progn-values))
  (catch (&rest [progn/thrown]-values))
  ((or ceiling
       fceiling
       ffloor
       floor
       fround
       ftruncate
       truncate)
   (quotient remainder))
  (cell-error-name (name))
  (cerror ((eql nil)))
  (change-class (instance))
  ((or char
       schar)
   (element-character))
  (char-code (implementation-dependent_code))
  (char-downcase (corresponding-character))
  ((or char-equal
       char-greaterp
       char-lessp
       char-not-equal
       char-not-greaterp
       char-not-lessp
       char-upcase
       char/=
       char<
       char<=
       char=
       char>
       char>=)
   (generalized-boolean))
  (char-int (implementation-dependent_integer))
  (char-name (implementation-dependent_name/nil))
  (character (denoted-character))
  (characterp (generalized-boolean))
  (check-type ((eql nil)))
  (cis (e^i*-radians))
  (class-name (name))
  (class-of (class))
  (clear-input ((eql nil)))
  ((or finish-output
       force-output
       clear-output)
   ((eql nil)))
  (close (t-if-was-open-else-implementation-dependent)) ;notable?
  (clrhash (hash-table))
  (code-char (character/nil))
  (coerce (coercion-result))
  (compile (function-name/compiled-function warnings-p failure-p))
  (compile-file (output-file-truename/nil warnings-p failure-p))
  (compile-file-pathname (pathname))
  (compiled-function-p (generalized-boolean))
  (compiler-macro-function (function/nil))
  (complement (complement-function))
  (complex (complex-number))
  (complexp (generalized-boolean))
  (compute-applicable-methods (applicable-methods))
  (compute-restarts (restarts-list_potentially-shared))
  (concatenate (fresh-sequence))
  (concatenated-stream-streams (input-streams))
  (cond (&rest matching-clause-progn-values))
  (conjugate (complex-conjugate))
  (cons (fresh-cons))
  (consp (generalized-boolean))
  (constantly (function))
  (constantp (generalized-boolean))
  ((or continue
       store-value
       use-value)
   ((eql nil)))
  (copy-alist (fresh-alist))
  (copy-list (fresh-list))
  (copy-pprint-dispatch (new-table))
  (copy-readtable (to-readtable-given/fresh-readtable))
  (copy-seq (fresh-sequence))
  (copy-structure (shallow-copy))
  (copy-symbol (new-symbol))
  (copy-tree (new-cons-tree))
  (cos (cosine))
  (cosh (hyperbolic-cosine))
  ((or count
       count-if
       count-if-not)
   (how-many))
  ((or decf
       incf)
   (new-value))
  (declaim :implementation-dependent)
  (decode-float (significand exponent sign))
  ((or decode-universal-time
       get-decoded-time)
   (second minute hour
           date month year
           day daylight-p zone))
  (defclass (new-class))
  (defconstant (name))
  (defgeneric ([new/redefined]-generic-function))
  (define-compiler-macro (name))
  (define-condition (name))
  (define-method-combination (name))
  (define-modify-macro (name))
  (define-setf-expander (function/macro-name))
  (define-symbol-macro (name))
  (defmacro (name))
  (defmethod (new-method))
  (defpackage (new/redefined-generic-function))
  (defparameter (name))
  (defsetf (function/macro-name))
  (defstruct (name))
  (deftype (name))
  (defun (name))
  (defvar (name))
  ((or delete
       delete-if
       delete-if-not
       delete-duplicates
       remove
       remove-if
       remove-if-not
       remove-duplicates)
   (result-sequence_potentially-shared))
  (delete-file ((eql t)))
  (delete-package (successp))
  (denominator (denominator))
  (deposit-field (result-integer))
  (describe ())
  (describe-object :implementation-dependent)
  (destructuring-bind (&rest progn-values))
  (digit-char (character))
  (digit-char-p (weight/nil))
  (directory (fresh-list-of-truenames))
  ((or namestring
       file-namestring
       directory-namestring
       host-namestring
       enough-namestring)
   (namestring))
  (disassemble ((eql nil)))
  ((or do
       do*)
   (&rest result-form-progn-values))
  ((or do-symbols
       do-external-symbols
       do-all-symbols)
   (&rest result-form-values))
  (documentation (docstring/nil))
  ((or dolist
       dotimes)
   (&rest result-form-values))
  (dpb (result-integer))
  (dribble :implementation-dependent)
  (echo-stream-input-stream (input-stream))
  (echo-stream-output-stream (output-stream))
  (ed (implementation-dependent))
  ((or first
       second
       third
       fourth
       fifth
       sixth
       seventh
       eighth
       ninth
       tenth)
   (object))
  (elt (sequence-element))
  (encode-universal-time (universal-time))
  (endp (generalized-boolean))
  (ensure-directories-exist (pathspec-given created))
  (ensure-generic-function (generic-function))
  ((or eq
       eql
       equal
       equalp)
   (generalized-boolean))
  (error :non-local-exit)
  (eval (&rest values))
  (eval-when (progn-values/nil))
  ((or evenp
       oddp)
   (generalized-boolean))
  ((or every
       notevery
       notany)
   (generalized-boolean))
  (exp (e^power))
  (export ((eql t)))
  (expt (base^power))
  (fboundp (generalized-boolean))
  (fdefinition (global-definition))
  (file-author (string/nil))
  (file-error-pathname (pathspec))
  (file-length (length/nil))
  (file-position (position//success-p))
  (file-string-length (object-serialization-length/nil))
  (file-write-date (universal-time/nil))
  (fill (sequence-given))
  (fill-pointer (fill-pointer))
  ((or find
       find-if
       find-if-not)
   (found/nil))
  (find-all-symbols (symbol-list))
  (find-class (class/nil))
  (find-method (method/nil))
  (find-package (package/nil))
  (find-restart (restart/nil))
  (find-symbol (symbol/nil status)) ; notable?
  ((or flet
       labels
       macrolet)
   (&rest progn-values))
  (float (float))
  (float-digits (representation-digits-count))
  (float-precision (significant-representation-digits-count))
  (float-radix (radix))
  (float-sign (float))
  (floatp (generalized-boolean))
  (fmakunbound (name-given))
  (format (string/nil))
  (formatter (function))
  (fresh-line (outputted-newline-p))
  (function (function))
  (function-keywords (keys allow-other-keys-p))
  (function-lambda-expression
   (lambda-expression/nil closure-p-unreliable debug-name-unreliable/nil))
  (functionp (generalized-boolean))
  (gcd (greatest-common-denominator))
  (gensym (fresh-uninterned-symbol))
  (gentemp (fresh-interned-symbol))
  (get (symbol-plist-value/default-given))
  (get-dispatch-macro-character (function-designator/nil))
  (get-internal-real-time (internal-time))
  (get-internal-run-time (internal-time))
  (get-macro-character (two-args-function-designator/nil non-terminating-p))
  (get-output-stream-string (accumulated-string))
  (get-properties (indicator value tail))
  (get-setf-expansion (vars vals store-vars writer-form reader-form))
  (get-universal-time (current-universal-time))
  (getf (plist-values/default))
  (gethash (values present-p))
  (go :non-local-exit)
  (graphic-char-p (generalized-boolean))
  (handler-bind (&rest progn-values))
  (handler-case (&rest form-values/matching-clause-progn-values))
  (hash-table-count (entries-count))
  (hash-table-p (generalized-boolean))
  (hash-table-rehash-size (integer-addend/float-factor))
  (hash-table-rehash-threshold (rehash-threshold))
  (hash-table-size (size))
  (hash-table-test (function-designator))
  (identity (object-given))
  (if (&rest [then/else]-form-values))
  (ignore-errors (&rest progn-values/nil+condition))
  (imagpart (imaginary-part))
  (import ((eql t)))
  (in-package (package))
  (initialize-instance (instance-given))
  ((or input-stream-p
       output-stream-p)
   (generalized-boolean))
  (inspect :implementation-dependent)
  (integer-decode-float (significand exponent integer-sign))
  (integer-length (number-of-bits))
  (integerp (generalized-boolean))
  (interactive-stream-p (generalized-boolean))
  (intern (symbol status))
  (intersection (list_shared))
  (invalid-method-error :implementation-dependent)
  (invoke-debugger :non-local-exit)
  (invoke-restart (&rest restart-values))
  (invoke-restart-interactively (&rest restart-values))
  (isqrt (natural-square-root))
  (keywordp (generalized-boolean))
  (lambda (function))
  (last (tail))
  (lcm (least-common-multiple))
  (ldb (integer-byte))
  (ldb-test (any-bit-set-p))
  (ldiff (copy-up-to-object-given))
  (length (length))
  ((or let
       let*)
   (&rest progn-values))
  ((or lisp-implementation-type
       lisp-implementation-version
       long-site-name
       short-site-name
       machine-instance
       machine-type
       machine-version
       software-type
       software-version)
   (description-string/nil))
  (list (fresh-list))
  (list* (list))
  (list-all-packages (fresh-list-of-all-registered-packages))
  (list-length (length/nil-if-circular))
  (list (character-immediately-available-p))
  (listp (generalized-boolean))
  (load (successp))
  (load-logical-pathname-translations (just-loaded-p))
  (locally (&rest progn-values))
  (log (logarithm))
  ((or logand
       logandc1
       logandc2
       logeqv
       logior
       lognand
       lognor
       lognot
       logorc1
       logorc2
       logxor)
   (integer))
  (logbitp (nth-bit-set-p))
  (logcount (number-of-set-bits))
  (logical-pathname (logical-pathname))
  (logical-pathname-translations (from-wildcard+to-wildcard_list))
  (logtest (not-zerop-logand))
  (loop (&rest values))
  (loop-finish :non-local-exit)
  (macro-function (macro-function/nil))
  ((or macroexpand
       macroexpand-1)
   (expansion/form-given expanded-p))
  (make-array (new-array))
  (make-broadcast-stream (new-broadcast-stream))
  (make-concatenated-stream (new-concatenated-stream))
  (make-condition (new-condition))
  (make-dispatch-macro-character ((eql t)))
  (make-echo-stream (new-echo-stream))
  (make-hash-table (new-hash-table))
  (make-instance (new-initialized-instance))
  (make-instances-obsolete (class-given))
  (make-list (new-list))
  (make-load-form (creation-form &optional initialization-form))
  (make-load-from-saving-slots (creation-form initialization-form))
  (make-method (method-object))
  (make-package (new-package))
  (make-pathname (new-pathname))
  (make-random-state (new-random-state))
  (make-sequence (new-sequence))
  (make-string (new-simple-string))
  (make-string-input-stream (new-string-input-stream))
  (make-string-output-stream (new-string-output-stream))
  (make-symbol (fresh-uninterned-symbol))
  (make-synonym-stream (new-synonym-stream))
  (make-two-way-stream (new-two-way-stream))
  (makunbound (symbol-given))
  (map (sequence/nil))
  (map-into (result-sequence-given))
  ((or mapc
       mapl)
   (first-list-given))
  ((or mapcan
       mapcon)
   (nconced-lists))
  ((or mapcar
       maplist)
   (fresh-list))
  (maphash ((eql nil)))
  (mask-field (masked-integer))
  (max (max-real))
  ((or member
       member-if
       member-if-not)
   (tail))
  (merge (sequence_potentially-sharing-with-both-sequence-args))
  (merge-pathnames (pathname))
  (method-combination-error :implementation-dependent)
  (method-qualifiers (qualifiers))
  (min (min-real))
  (minusp (negativep))
  (mismatch (mismatch-position))
  (mod (modulus))
  (multiple-value-bind (&rest progn-values))
  (multiple-value-list (form-values-as-list))
  (multiple-value-prog1 (&rest first-form-values))
  (multiple-value-setq (form-primary-value))
  (name-char (name/nil))
  (nbutlast (truncated-list_potentially-sharing))
  (nconc (destructively-appended-list))
  (next-method-p (generalized-boolean))
  (nintersection
   (list_potentially-sharing-with-list-1))
  ((or no-applicable-methods
       no-next-method)
   (&rest values))
  (not (boolean))
  ((or nreconc
       revappend)
   (list))
  (nreverse (reversed-sequence))
  ((or nset-exclusive-or
       nset-difference
       nunion
       set-exclusive-or
       set-difference
       union)
   (list)) ; Could be more explicit, but I'm tired right now.
  ((or nstring-capitalize
       nstring-downcase
       nstring-upcase)
   (string-given))
  (nsublis (cons-tree-given-with-substitutions))
  ((or nsubst
       nsubst-if
       nsubst-if-not)
   (cons-tree-given-with-new-substituted))
  ((or nsubstitute
       nsubstitute-if
       nsubstitute-if-not)
   (sequence-given))
  (nth (nth-element-of-list-given/nil-if-past-end))
  (nth-value (nth-multiple-value-of-form/nil-if-not-enough-values))
  (nthcdr (tail))
  ((or null
       not)
   (boolean))
  (numberp (generalized-boolean))
  (numerator (numerator))
  (open (stream/nil))
  (open-stream-p (generalized-boolean))
  (or (&rest first-true-primary-value/last-form-values/nil-if-no-forms))
  (output-stream-p (generalized-boolean))
  (package-error-package (package-designator))
  (package-name (string/nil))
  (package-nicknames (strings-list))
  (package-shadowing-symbols (symbols-list))
  (package-use-list (list-of-packages-package-given-uses))
  (package-used-by-list (list-of-packages-that-use-package-given))
  (packagep (generalized-boolean))
  (pairlis (extended-alist-with-prepended-elements-[forward/backwards]))
  (parse-integer (integer/nil position-after-integer-if-junk-allowed-else-string-length))
  (parse-namestring (pathname/nil position-after-pathname-if-junk-allowed-else-string-length))
  (pathname (designated-pathname))
  (pathname-device (string/nil/unspecific/implementation-defined))
  (pathname-directory (string/strings-list/nil/wild/unspecific/implementation-defined))
  (pathname-host (string/strings-list/unspecific/logical-host))
  (pathname-match-p (generalized-boolean_implementation-dependent-matching-rules))
  (pathname-name (string/nil/wild/unspecific/implementation-defined))
  (pathname-type (string/nil/wild/unspecific))
  (pathname-version (non-negative-integer/wild/newest/unspecific/nil_semi-standard>oldest/previous/installed))
  (pathnamep (generalized-boolean))
  (peek-char (character/eof-value))
  (phase (polar-angle-radians))
  (plusp (positive-and->0-p))
  (pop (former-car))
  ((or position
       position-if
       position-if-not)
   (index/nil))
  (pprint ())
  (pprint-dispatch (found-function/print-object-proxy-function found-p))
  (pprint-exit-if-list-exhausted ((eql nil)))
  ((or pprint-fill
       pprint-linear
       pprint-tabular)
   ((eql nil)))
  (pprint-indent ((eql nil)))
  (pprint-logical-block ((eql nil)))
  (pprint-newline ((eql nil)))
  (pprint-pop (list-element-of-lexically-current-logical-block/nil))
  (pprint-tab ((eql nil)))
  ((or prin1
       princ
       print
       write)
   (object-given))
  ((or prin1-to-string
       princ-to-string
       write-to-string)
   (string))
  (print-not-readable-object (object-that-cannot-be-printed))
  (print-object (object-given))
  (print-unreadable-object ((eql nil)))
  (probe-file (truename/nil))
  (proclaim :implementation-dependent)
  ((or prog
       prog*)
   ((eql nil))) ; I don't count an explicit return as normal return values.
  (prog1 (first-form-primary-value))
  (prog2 (second-form-primary-value))
  ((or progn
       progv)
   (&rest progn-values))
  (provide :implementation-dependent)
  ((or psetf
       psetq)
   ((eql nil)))
  ((or push
       pushnew)
   (extended-list))
  (quote (unevaluated-object-given))
  (random (random-number-<limit-and-of-same-type))
  (random-state-p (generalized-boolean))
  ((or rassoc
       rassoc-if
       rassoc-if-not)
   (entry/nil))
  ((or rational
       rationalize)
   (rational))
  (rationalp (generalized-boolean))
  ((or read
       read-preserving-whitespace)
   (parsed-object/eof-value))
  (read-byte (integer/eof-value))
  (read-char (character/eof-value))
  (read-char-no-hang (immediately-available-character/nil))
  (read-delimited-list (parsed-objects-list))
  (read-from-string (parsed-object/eof-value position-after-parsed-object))
  (read-line (line/eof-value missing-newline-p))
  (read-sequence (position-first-element-not-updated))
  (readtable-case (upcase/downcase/preserve/invert))
  (readtablep (generalized-boolean))
  (realp (generalized-boolean))
  (realpart (realpart))
  (reduce (result))
  (reinitialize-instance (instance-given))
  (rem (remainder))
  ((or remf
       remhash
       remprop)
   (was-present-p))
  (rename-file (defaulted-new-pathname old-truename new-truename))
  (rename-package (package-object))
  (replace (modified-sequence-given))
  (require :implementation-dependent)
  (rest (tail))
  ((or restart-bind
       restart-case)
   (&rest progn-values))
  (restart-name (symbol/nil))
  ((or return
       return-from)
   :non-local-exit)
  (reverse (fresh-reversed-sequence))
  (room :implementation-dependent)
  (rotatef ((eql nil)))
  (row-major-aref (element))
  ((or rplaca
       rplacd)
   (modified-cons))
  (scale-float (scaled-float))
  (search (leftmost-position-first-match/nil))
  (set (value-given))
  (set-dispatch-macro-character ((eql t)))
  (set-macro-character ((eql t)))
  (set-pprint-dispatch ((eql nil)))
  (set-syntax-from-char ((eql t)))
  (setf (&rest values-storing-form-last-place/nil-if-no-pairs))
  (setq (primary-value-last-form/nil-if-no-pairs))
  (shadow ((eql t)))
  (shadowing-import ((eql t)))
  (shared-initialize (instance-given))
  (shiftf (former-value-of-first-place))
  (signal ((eql nil)))
  (signum ([-1/0/1]-of-same-type))
  (simple-bit-vector-p (generalized-boolean))
  (simple-condition-format-arguments (format-arguments))
  (simple-condition-format-controls (format-control))
  (simple-string-p (generalized-boolean))
  (simple-vector-p (generalized-boolean))
  (sin (sine))
  (sinh (hyperbolic-sine))
  (sleep ((eql nil)))
  ((or slot-boundp
       slot-exists-p)
   (generalized-boolean))
  (slot-makunbound (instance-given))
  (slot-missing (&rest values))
  (slot-unbound (object &rest ignored-values))
  (slot-value (value))
  (some (first-satisfactory-element))
  ((or sort
       stable-sort)
   (destructively-sorted-sequence-given))
  (special-operator-p (one-of-25-standard-special-operators-p))
  (sqrt (principal-square-root))
  (standard-char-p (generalized-boolean))
  (step (&rest progn-results))
  (stream-element-type (typespec))
  (stream-error-stream (stream))
  (stream-external-format (external-file-format-designator))
  (streamp (generalized-boolean))
  (string (denoted-string))
  ((or string-capitalize
       string-downcase
       string-upcase)
   (fresh-string))
  (string-equal (generalized-boolean))
  ((or string/=
       string<
       string>
       string<=
       string>=
       string-not-equal
       string-lessp
       string-greaterp
       string-not-greaterp
       string-not-lessp)
   (mismatch-index))
  ((or string-trim
       string-left-trim
       string-right-trim)
   (fresh-string/string-given))
  (stringp (generalized-boolean))
  (sublis (cons-tree))
  (subseq (fresh-subsequence))
  (subsetp (generalized-boolean))
  ((or subst
       subst-if
       subst-if-not)
   (cons-tree))
  ((or substitute
       substitute-if
       substitute-if-not)
   (sequence_potentially-sharing))
  (subtypep (type1-recognizable-subtype-of-type2-p definite-answer-p))
  (svref (element))
  (sxhash (hash-code_non-negative-fixnum))
  (symbol-function (function/implementation-defined-[macro/special-operator]-proxy))
  (symbol-macrolet (&rest progn-values))
  (symbol-name (string))
  (symbol-package (home-package/nil))
  (symbol-plist (property-list))
  (symbol-value (global-value))
  (symbolp (generalized-boolean))
  (synonym-stream-symbol (symbol))
  (tagbody ((eql nil)))
  (tailp (object-is-a-cdr-of-list-p))
  (tan (tangent))
  (tanh (hyperbolic-tangent))
  (terpri ((eql nil)))
  (the (&rest form-values))
  (throw :non-local-exit)
  (time (&rest form-values))
  (trace (function-names-if-zero-args/implementation-dependent))
  (translate-logical-pathname (physical-pathname))
  (translate-pathname (translated-pathname))
  (tree-equal (generalized-boolean))
  (truename (truename))
  (two-way-stream-input-stream (input-stream))
  (two-way-stream-input-stream (output-stream))
  (type-error-datum (offending-datum))
  (type-error-expected-type (expected-type-of-datum))
  (type-of (typespec))
  (typep (generalized-boolean))
  (unbound-slot-instance (instance))
  (unexport ((eql t)))
  (unintern (was-present-p))
  ((or when
       unless)
   (&rest progn-values))
  (unread-char ((eql nil)))
  (untrace :implementation-dependent)
  (unuse-package ((eql t)))
  (unwind-protect (&rest protected-form-values))
  (update-instance-for-different-class :implementation-dependent)
  (update-instance-for-redefined-class (&rest ignored-values))
  (upgraded-array-element-type (upgraded-typespec))
  (upgraded-complex-part-type (upgraded-typespec))
  (use-package ((eql t)))
  (user-homedir-pathname (pathname/nil))
  (values (&rest values))
  (values-list (&rest list-elements))
  (vector (fresh-simple-general-vector))
  (vector-pop (last-element-according-to-fill-pointer))
  (vector-push (index-of-new-element/nil-if-full))
  (vector-push-extend (index-of-new-element))
  (vectorp (generalized-boolean))
  (warn ((eql nil)))
  (wild-pathname-p (generalized-boolean))
  ((or with-accessors
       with-compilation-unit
       with-condition-restarts
       with-hash-table-iterator
       with-input-from-string
       with-open-file
       with-open-stream
       with-package-iterator
       with-slots
       with-standard-io-syntax)
   (&rest progn-values))
  (with-output-to-string (string/progn-values))
  (with-simple-restart (progn-values/nil+t))
  (write-byte (byte-given))
  (write-char (character-given))
  (write-line (string-given))
  (write-sequence (sequence-given))
  (write-string (string-given))
  (y-or-n-p (user-entered-y-p))
  (yes-or-no-p (user-entered-yes-p))
  (zerop (generalized-boolean)))
