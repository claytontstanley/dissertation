(defparameter *_DIR_* (directory-namestring *load-truename*))
(defparameter *csv-name* "in.csv")

(cl-interpol:enable-interpol-syntax)

(setf cl-csv::*always-quote* t)
(setf cl-csv::*newline* #?"\n")

(defun get-dir (posts-id)
  (format nil "~3,'0d" (mod (parse-integer posts-id) 1000)))

(defun write-col (col dir)
  (let ((cnt 0))
    (lambda (csv)
      (incf cnt)
      (format  t "extracting ~a, line ~a~%" col cnt)
      (let ((posts-id (csv-val 'posts-id csv)))
        (let ((col-val (csv-val col csv)))
          (when (not (= cnt 1))
            (let ((out-file (format nil "~a/~a/~a" dir (get-dir posts-id) posts-id)))
              (with-open-file (strm out-file :direction :output :if-exists :supersede :if-does-not-exist :create)
                (format strm "~a" col-val)))))))))

(let ((fun-lst (list 
                 (write-col 'body "html")
                 (write-col 'title "title/raw")
                 (write-col 'tags "tags/raw"))))
  (defun write-all-cols (csv)
    (loop for fun in fun-lst
          do (funcall fun csv))))

(defun extract-all ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-all-cols)))

(defun build-csv ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-csv)))

(defun csv-val (key csv)
  (nth
    (ecase key
      (posts-id 0)
      (body 1)
      (tags 2)
      (title 3))
    csv))

(defun csv-vals (keys csv)
  (loop for key in keys
        collect (csv-val key csv)))

(let ((cnt 0))
  (defun write-csv (csv)
    (incf cnt)
    (format  t "converting line ~a~%" cnt)
    (let ((out-file "out.csv"))
      (let ((posts-id (csv-val 'posts-id csv)))
        (with-open-file (strm out-file :direction :output :if-exists (if (= cnt 1) :supersede :append) :if-does-not-exist :create)
          (when (not (= cnt 1))
            (let ((txt (file-string (format nil "~a/~a/~a" "nohtml" (get-dir posts-id) posts-id))))
              (cl-csv:write-csv-row (list posts-id txt) :stream strm))))))))
