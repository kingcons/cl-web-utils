;; helper.lisp
;; Helpers

(in-package :cl-web-utils)

(export '(nand nor with-new-thread str-join concat safe-read-from-string
	  last1 simple-run mkstr symb ksymb parse-float tail file-info file-mime
	  remove-keyword remove-keywords random-nth time-difference md5 helpers
	  readable-character-code-p byte-array-to-ascii-string to-json from-json
	  eval-always string-starts-with awhen aif))

(defmacro nor (&rest forms)
  `(not (or ,@forms)))

(defmacro nand (&rest forms)
  `(not (and ,@forms)))

;;; FIXME dont use eval.
(defmacro str-join (string-list &key (sep "") key)
  (if key
    (eval ``(str-join ,(mapcar ,key ',string-list) :sep ,,sep) )
    `(concatenate 'string ,@(cdr (loop for s in string-list append (list sep s))))))

(defmacro with-new-thread (&body forms)
  `(make-thread (lambda () ,@forms)))

(defmacro concat (&body body)
  `(concatenate 'string ,@body))

(defmacro last1 (list)
  `(car (last ,list)))

(defun simple-run (prog args &key filename output error (element-type 'character) split-lines-p (input t) (env (sb-ext:posix-environ)))
  (let ((raw
	 (if (or filename output)
           (progn
             (sb-ext:run-program
              prog args :output (if output output filename)
              :if-output-exists :supersede :environment env :error error
              :input input)
             (if output output filename))
           (with-output-to-string (str nil :element-type element-type)
             (sb-ext:run-program
              prog args :output str :environment env :error error
              :input input)))))
    (if split-lines-p
      (split-sequence #\Newline (string-right-trim '(#\Newline) raw))
      raw)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun ksymb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun parse-float (s)
  (let ((*read-eval* nil))
    (let ((val (read-from-string s)))
      (cond
	((typep val 'float) val)
	((typep val 'fixnum) (coerce val 'float))))))

(defmethod tail (filename &optional (lines 30))
  (with-output-to-string (str)
    (sb-ext:run-program "/usr/bin/tail" (list "-n" (princ-to-string lines) filename) :output str)))

(defun file-info (filename)
  (string-trim '(#\Return #\Newline #\Linefeed)
	       (with-output-to-string (str)
		 (sb-ext:run-program "/usr/bin/file" (list "-b" filename) :output str))))

(defun file-mime (filename)
  (string-trim '(#\Return #\Newline #\Linefeed)
	       (with-output-to-string (str)
		 (sb-ext:run-program "/usr/bin/file" (list "-i" "-b" filename) :output str))))


(defun remove-keyword (list keyword)
  (let (remove)
    (loop for i in list
          when (eql i keyword)
            do (setf remove t)
          else when remove
                 do (setf remove nil)
          else collect i)))

(defun remove-keywords (list &rest keywords)
  (loop for argument in keywords
        with i = list
        do (setf i (remove-keyword i argument))
        finally (return i)))

(defun random-nth (list)
  (nth (random (length list)) list))

(defun readable-character-code-p (code &optional allow-return)
  (or (and allow-return (= code 13))
      (< 31 code 127)))

(defun byte-array-to-ascii-string (arr &optional readable-only strip-high-bit
                                       null-terminated allow-return)
  (let* ((max-len (if null-terminated
                    (or (position 0 arr) (length arr))
                    (length arr)))
	 (rtn (make-string max-len)))
    (loop for el across arr
	  for els = (if strip-high-bit (logand #x7f el) el)
	  for c = (code-char els)
	  for x = 0 then (1+ x)
	  while (< x max-len)
	  do (setf (aref rtn x)
		   (if readable-only
                     (if (readable-character-code-p els allow-return)
                       (or (and allow-return (= els 13) #\Newline) c)
                       #\Space)
                     c)))
    rtn))

(defmacro to-json (item)
  `(json:encode-json-to-string ,item))

(defmacro from-json (string)
  `(json:decode-json-from-string ,string))

(defun md5 (string)
  (string-downcase
   (with-output-to-string (str)
     (loop for el across (md5:md5sum-sequence string)
           do (format str "~2,'0x" el)))))

(defun safe-read-from-string (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun save-binary-file (name data)
  (with-open-file (out name :element-type '(unsigned-byte 8) :direction :output)
    (write-sequence data out))
  name)

(defmacro eval-always (&body body)
  "Expands into an eval-when with all the fixings. It's nothing but a shorthand."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun string-starts-with (string prefix &key test)
  "Returns true if `string` starts with `prefix`.

Use the keyword argument `test` (which defaults to `char=`) to check
each character."
  (setf test (or (and test (ensure-function test)) #'char=))
  (let ((mismatch (mismatch prefix string :test test)))
    (or (not mismatch) (= mismatch (length prefix)))))

(defmacro anaphoric (op test &body body)  
  ;; Note: multiple values discarded. Handling them would be nice, but also
  ;; requires consing up a values-list, which seems a bit harsh for something
  ;; that is supposed to be "simple syntactic sugar".
  `(let ((it ,test))
     (,op it ,@body)))

(defmacro awhen (test &body body)
  "Like WHEN, except binds the result of the test to IT (via LET) for the scope
of the body."
  `(anaphoric when ,test ,@body))

(defmacro aif (test then &optional else)
  "Like IF, except binds the result of the test to IT (via LET) for
the scope of the then and else expressions."  
  `(anaphoric if ,test ,then ,else))
