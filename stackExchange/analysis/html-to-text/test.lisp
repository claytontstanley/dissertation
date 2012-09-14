(defparameter *_DIR_* (directory-namestring *load-truename*))
(defparameter *csv-name* "in.csv")

(cl-interpol:enable-interpol-syntax)

(setf cl-csv::*always-quote* t)
(setf cl-csv::*newline* #?"\n")

(defun csv->lst (csv)
  (rest 
    (cl-csv:read-csv csv)))

(defun load-csv (path)
  (file-string path))

(let ((cnt 0))
  (defun html->text (csv)
    (incf cnt)
    (when (and (not (= cnt 1))
               (<= cnt 101))
      (with-cwd *_DIR_*
        (destructuring-bind (posts_id body) csv
          (let ((tmp-file "/tmp/tmp.txt"))
            (with-open-file (strm tmp-file :direction :output :if-exists :supersede)
              (format strm "~a" body))
            (format  t "converting line ~a with id ~a~%" cnt posts_id)
            (setf body [python html-to-text.py ?tmp-file ]))
          (list posts_id body))))))

(defun write-csv (csv)
  (with-open-file (strm "out.csv" :direction :output :if-exists :append :if-does-not-exist :create)
    (awhen (html->text csv)
      (cl-csv:write-csv-row it :stream strm))))

(with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
  (cl-csv:read-csv strm :row-fn #'write-csv))

