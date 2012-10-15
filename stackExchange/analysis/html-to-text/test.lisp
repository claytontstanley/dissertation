(defparameter *_DIR_* (directory-namestring *load-truename*))
(defparameter *csv-name* "in-huge.csv")
(defparameter *processed-dirname* (if (search "huge" *csv-name*) "nlp-huge" "nlp"))

(cl-interpol:enable-interpol-syntax)

(setf cl-csv::*always-quote* t)
(setf cl-csv::*newline* #?"\n")

(defun get-dir (posts-id)
  (format nil "~3,'0d"
          (mod
            (if (stringp posts-id)
              (parse-integer posts-id)
              posts-id)
            1000)))

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

(let ((dir-name (if (search "huge" *csv-name*) "raw-huge" "raw")))
  (let ((fun-lst (list 
                   ;(write-col 'body (format nil "body/~a" dir-name))
                   ;(write-col 'title (format nil "title/~a" dir-name))
                   (write-col 'tag (format nil "tag/~a" dir-name)))))
    (defun write-all-cols (csv)
      (loop for fun in fun-lst
            do (funcall fun csv)))))

(defun extract-all ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-all-cols)))

(defun build-nohtml ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-csv)))

(defun build-chunks ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* *csv-name*) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-chunks)))

(defun get-shuffled-post-ids (start end)
  (subseq 
    (mapcar #'parse-integer
            (mapcar #'first-and-only
                    (with-open-file (strm (format nil "~a~a" *_DIR_* "post-ids.csv") :direction :input)
                      (cl-csv:read-csv strm :skip-first-p t))))
    start
    end))

(defun get-rel-paths (post-ids)
  (let ((dirs (mapcar #'get-dir post-ids)))
    (loop for dir in dirs
          for id in post-ids 
          collect
          (format nil "~a/~a/~a"
                  *processed-dirname*
                  dir
                  id))))

(defun create-all-symlinks ()
  ; FIXME: Add 'title chunk back in
  (dolist (chunk-type (list 'tag))
    (create-symlinks 000000 100000 chunk-type 1)
    (create-symlinks 100000 200000 chunk-type 2)))

(defun create-symlinks (start end chunk-type subset-num)
  (let ((rel-paths (get-rel-paths (get-shuffled-post-ids start end))))
    (let ((src-paths
            (mapcar (lambda (path)
                      (format nil "~a/~a"
                              (chunk-type->folder chunk-type)
                              path))
                    rel-paths))
          (des-paths
            (mapcar (lambda (path)
                      (format nil "~a-subset-~a/~a"
                              (chunk-type->folder chunk-type)
                              subset-num
                              path))
                    rel-paths)))
      (let ((cluster-num 1000))
        (loop for src-paths in (group src-paths cluster-num)
              for des-paths in (group des-paths cluster-num)
              do (let ((src-paths-as-string
                         (format nil "~{~a~^~%~}" src-paths)))
                   (let ((des-paths-as-string
                           (format nil "~{~a~^~%~}" des-paths)))
                     (format t "sending group of ~a~%" cluster-num)
                     (with-cwd *_DIR_*
                       (script (format nil "~a '~a' '~a'"
                                       "./create-symlinks.sh"
                                       src-paths-as-string
                                       des-paths-as-string))))))))))

(defun csv-val (key csv)
  (nth
    (ecase key
      (posts-id 0)
      (body 1)
      (tag 2)
      (title 3))
    csv))

(defun chunk-type->folder (chunk-type)
  (cond ((eq chunk-type 'title) "title")
        ((eq chunk-type 'body) "body")
        ((eq chunk-type 'tag) "tag")
        (t (error "chunk-type ~a not defined~%" chunk-type))))

(defun csv-vals (keys csv)
  (loop for key in keys
        collect (csv-val key csv)))

(defun lowercase (str)
  (format nil "~(~a~)" str))

(let ((cnt 0))
  (let ((chunk-cnt 0))
    (defun write-chunks (csv)
      (incf cnt)
      (format  t "converting line ~a~%" cnt)
      (let ((out-file "chunks.csv"))
        (let ((posts-id (csv-val 'posts-id csv)))
          (with-open-file (strm out-file :direction :output :if-exists (if (= cnt 1) :supersede :append) :if-does-not-exist :create)
            (when (not (= cnt 1))
              (dolist (chunk-type (list 'title 'body 'tag))
                (let ((txt (file-string (format nil "~a/~a/~a/~a" (chunk-type->folder chunk-type) *processed-dirname* (get-dir posts-id) posts-id))))
                  (let ((chunks (cl-ppcre:split #?"\n" txt)))
                    (dolist (chunk chunks)
                      (cl-csv:write-csv-row (list (incf chunk-cnt) posts-id chunk (lowercase chunk-type)) :stream strm))))))))))))

(let ((cnt 0))
  (defun write-csv (csv)
    (incf cnt)
    (format  t "converting line ~a~%" cnt)
    (let ((out-file "nohtml.csv"))
      (let ((posts-id (csv-val 'posts-id csv)))
        (with-open-file (strm out-file :direction :output :if-exists (if (= cnt 1) :supersede :append) :if-does-not-exist :create)
          (when (not (= cnt 1))
            (let ((txt (file-string (format nil "~a/~a/~a" "body/nohtml" (get-dir posts-id) posts-id))))
              (cl-csv:write-csv-row (list posts-id txt) :stream strm))))))))



