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
           #:md5
;           #:awhen
;           #:aif
;           #:byte-array-to-ascii-string
;           #:to-json
;           #:from-json
;           #:anaphoric
;           #:save-binary-file
;           #:string-starts-with
;           #:eval-always
;           #:with-new-thread
;           #:str-join
;           #:safe-read-from-string
;           #:simple-run
;           #:mkstr
;           #:tail
;           #:file-info
;           #:file-mime
;           #:remove-keyword
;           #:remove-keywords
;           #:time-difference
;           #:readable-character-code-p
           ))
