(ql:quickload :cl-csv)
(ql:quickload :cl-interpol)

(cl-interpol:enable-interpol-syntax)

(setf cl-csv::*always-quote* t)
(setf cl-csv::*newline* #?"\n")
(defparameter *_DIR_* (directory-namestring *load-truename*))
(defparameter *_DIR_* "~/src/dissertation/stackExchange/analysis/model/")

(require :cocoa)

(load (format nil "~a~a" *_DIR_* "actr6/load-act-r-6.lisp"))

(defparameter *csv*
  (let ((input-csv "title-chunks.csv"))
    (with-open-file (strm (format nil "~a~a" *_DIR_* input-csv) :direction :input)
      (cl-csv:read-csv strm :skip-first-p t))))

(defparameter *csv-sub* (subseq *csv* 0 10))

*csv-sub*
(clear-all)

(define-model fdm0
  (chunk-type dm-assoc arg1 arg2)
  (chunk-type context word)
  (chunk-type tag word)
  (add-dm-fct
    (loop for chunk in (remove-duplicates
                         (mapcar #'first *csv-sub*)
                         :test #'string-equal)
          collect (list
                    (intern (string-upcase chunk))
                    'isa 'context
                    'word chunk)))
  (add-dm-fct
    (loop for chunk in (remove-duplicates
                         (mapcar #'third *csv-sub*)
                         :test #'string-equal)
          collect (list
                    (intern (string-upcase chunk))
                    'isa 'tag
                    'word chunk)))
  (add-dm-fct
    (loop for (left-chunk left-chunkhash right-chunk right-chunkhash count) in *csv-sub*
          ;declare (ignore left-chunkhash right-chunkhash)
          append (loop for i from 0 to (parse-integer count)
                       collect (list 
                                 'isa 'dm-assoc
                                 'arg1 (intern (string-upcase left-chunk))
                                 'arg2 (intern (string-upcase right-chunk)))))))





(first *csv*)
(length *csv*)


(make-instance 'window)
