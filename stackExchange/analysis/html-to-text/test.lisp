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

(defun html->text (csv)
  (with-cwd *_DIR_*
    (destructuring-bind (posts_id body) csv
      (let ((tmp-file "/tmp/tmp.txt"))
        (with-open-file (strm tmp-file :direction :output :if-exists :supersede)
          (format strm "~a" body))
        (setf body [python html-to-text.py ?tmp-file ]))
      (list posts_id body))))

(defun get-dir (posts-id)
  (format nil "~3,'0d" (mod (parse-integer posts-id) 1000)))

(let ((cnt 0))
  (defun write-html (csv)
    (incf cnt)
    (format  t "converting line ~a~%" cnt)
    (destructuring-bind (posts-id body) csv
      (when (not (= cnt 1))
        (let ((out-file (format nil "~a/~a/~a" "html" (get-dir posts-id) posts-id)))
          (with-open-file (strm out-file :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format strm "~a" body)))))))

(defun extract-html ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-html)))

(defun build-csv ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-csv)))

(let ((cnt 0))
  (defun write-csv (csv)
    (incf cnt)
    (format  t "converting line ~a~%" cnt)
    (destructuring-bind (posts-id body) csv
      (declare (ignore body))
      (let ((out-file "out.csv"))
        (with-open-file (strm out-file :direction :output :if-exists (if (= cnt 1) :supersede :append) :if-does-not-exist :create)
          (when (not (= cnt 1))
            (let ((txt (file-string (format nil "~a/~a/~a" "nohtml" (get-dir posts-id) posts-id))))
              (cl-csv:write-csv-row (list posts-id txt) :stream strm))))))))
