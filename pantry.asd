(defsystem "pantry"
  :description "Common Lisp client for Pantry JSON storage service: https://getpantry.cloud"
  :author "Aleksandar Simic <a@repl.ist>"
  :license "BSD"
  :version "0.0.3"
  :depends-on (:dexador :com.inuoe.jzon)
  :components ((:file "pantry")))
