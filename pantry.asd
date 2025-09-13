(defsystem "pantry"
  :description "Common Lisp client for Pantry JSON storage service: https://getpantry.cloud"
  :author "Aleksandar Simic a@repl.ist"
  :license "BSD"
  :version "0.1.0"
  :depends-on (:dexador :jonathan)
  :components ((:file "pantry")))
