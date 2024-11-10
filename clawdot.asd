(asdf:defsystem :clawdot/wrapper
  :description "godot bindings"
  :author "Jason Chandler"
  :license "MIT"
  :version "1.0"
  :depends-on (:cffi :claw :claw-utils :cl-resect :cl-ppcre :uiop)
  :pathname "src/"
  :serial t
  :components (
               (:module :godot-includes :pathname "godot-cpp/include/")
               (:module :godot-gen-includes :pathname "godot-cpp/gen/include/")))


(asdf:defsystem :clawdot
  :description "godot bindings"
  :author "Jason Chandler"
  :license "MIT"
  :version "1.0"
  :depends-on (:cffi :claw :claw-utils :cl-resect :cl-ppcre :uiop)
  :pathname "src/"
  :serial t
  :components ((:file "clawdot")))
