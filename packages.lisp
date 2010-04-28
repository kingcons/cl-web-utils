(defpackage :cl-web-utils
  (:use :common-lisp)
  (:import-from :drakma :http-request
                        :url-encode)
  (:import-from :split-sequence :split-sequence)
  (:export #:define-xml-request
           #:define-json-request
           #:define-http-request
           #:nor
           #:nand
           #:concat
           #:last1
           #:symb
           #:ksymb
           #:random-nth
           #:parse-float
           #:md5))
