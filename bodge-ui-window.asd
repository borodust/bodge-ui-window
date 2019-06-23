(asdf:defsystem :bodge-ui-window
  :description "UI window"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (bodge-utilities bodge-concurrency bodge-math bodge-host bodge-ui
                               bodge-canvas bodge-canvas-ui
                               glad-blob bodge-glad)
  :serial t
  :components ((:file "ui-window")))


(asdf:defsystem :bodge-ui-window/examples
  :description "UI window"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (bodge-ui-window)
  :pathname "examples"
  :serial t
  :components ((:file "basic")
               (:file "styling")))
