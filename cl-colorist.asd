(asdf:defsystem #:cl-colorist
  :description "Small, capability-aware ANSI text styling."
  :author "Lukáš Hozda"
  :version "0.1.0"
  :serial t
  :components ((:module "source"
                :serial t
                :components ((:file "package")
                             (:file "colorist"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-colorist/tests))))

(asdf:defsystem #:cl-colorist/tests
  :description "Tests for cl-colorist."
  :depends-on (#:cl-colorist)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:cl-colorist/tests '#:run-tests)))
