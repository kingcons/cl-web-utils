;; simple-xml.lisp

(in-package :cl-web-utils)

(defun xml-url-raw (url &optional stream)
  (cxml:parse-stream (or stream (http-request url :want-stream t))
		     (cxml-xmls:make-xmls-builder)))

(defun xml-url-dom (url &optional stream)
  (cxml:parse-stream (or stream (http-request url :want-stream t) )
		     (cxml-dom:make-dom-builder)))

(defun print-dom (obj &optional (indent 0))
  (indent indent)
  (format t "~a~%" obj)
  (awhen (slot-value obj 'rune-dom::children)
    (loop for child across it
	  do (print-dom child (1+ indent)))))

(defun xml-name-to-keyword (name)
  (ksymb
   (substitute #\- #\_
	       (string-upcase
		(aif (position #\: name)
                  (subseq name (1+ it))
		  name)))))

(defun parse-xml-dom-item (item fields)
  (loop for child across (dom:child-nodes item)
	for hit = (and (typep child 'dom:element)
		       (or (and (eq fields :all) :all)
			   (member (dom:tag-name child) fields
                                   :test #'equalp :key #'car)))
	when hit
	  collect (cons
		    (or (and (eq hit :all) (xml-name-to-keyword (dom:tag-name child)))
			(cdar hit))
		    (awhen (dom:first-child child) (dom:node-value it)))))

(defun parse-xml-dom-list (name feed fields)
  (loop for item across (dom:get-elements-by-tag-name feed name)
	collect (parse-xml-dom-item
		      item
		      (typecase fields
			(list
			   (loop for field in fields
				 collect (cons field (xml-name-to-keyword field))))
			(t fields)))))

(defun simple-xml (url-or-fn fields &optional (item-name "item"))
  (parse-xml-dom-list item-name
		      (typecase url-or-fn
			(function (funcall url-or-fn))
			(t (xml-url-dom url-or-fn)))
		      fields))
