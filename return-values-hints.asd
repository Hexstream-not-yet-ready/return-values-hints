(asdf:defsystem #:return-values-hints

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Allows querying of return values hints for all standard operators, as well as registering and querying of such hints for user-defined operators."

  :version "0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "main")
               (:file "standard")))
