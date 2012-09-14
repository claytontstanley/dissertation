(defparameter *_DIR_* (directory-namestring *load-truename*))

(defparameter *csv-name* "in.csv")
(defparameter *txt* nil)
(defparameter *csv* nil)
(defparameter *processed-csv* nil)

(defun csv->lst (csv)
  (cl-csv:read-csv csv))

(defun load-csv (path)
  (file-string path))

(defun html->text (csv)
  (with-cwd *_DIR_*
    (loop for (posts_id body) in csv
          for count from 1
          do (let ((tmp-file "/tmp/tmp.txt"))
               (with-open-file (strm tmp-file :direction :output :if-exists :supersede)
                 (format strm "~a" body))
               (format  t "converting line ~a~%" count)
               (setf body [python html-to-text.py ?tmp-file ]))
          collect (list posts_id body))))

(setf *txt*
      (load-csv 
        (format nil "~a~a" *_DIR_* *csv-name*)))

(setf *csv* (csv->lst *txt*))

(setf *processed-csv* (html->text *csv*))

(with-cwd *_DIR_*
  (with-open-file (strm "out.csv" :direction :output :if-exists :supersede)
    (cl-csv:write-csv *processed-csv* :stream strm)))

