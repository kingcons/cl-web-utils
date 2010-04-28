(defsystem :cl-web-utils
  :name "cl-web-utils"
  :description "A set of utilities for wrapping website APIs"
  :version "0.0.1"
  :author "Will Halliburton"
  :maintainer "Brit Butler <redline6561@gmail.com>"
  :license "LLGPL"
  :depends-on (:drakma :cxml :cl-json :md5 :split-sequence)
  :serial t
  :components ((:file "packages")
               (:file "helpers")
               (:file "simple-http-request")
               (:file "simple-xml")))
