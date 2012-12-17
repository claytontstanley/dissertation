(cl-interpol:enable-interpol-syntax)

(defparameter *huge-p* nil)

(defun make-huge-if-huge (str)
  (if *huge-p* 
    (let ((split-str (cl-ppcre:split "(\\.)" str :with-registers-p t)))
      (format nil "~a-huge~{~a~}" (first split-str) (rest split-str)))
    str))

(defparameter *csv-name* "in.csv")
(defparameter *processed-dirname* "nlp")

(defparameter *_DIR_* (directory-namestring *load-truename*))
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

(let ((dir-name (make-huge-if-huge "raw")))
  (let ((fun-lst (list 
                   (write-col 'body (format nil "body/~a" dir-name))
                   (write-col 'title (format nil "title/~a" dir-name))
                   (write-col 'tag (format nil "tag/~a" dir-name)))))
    (defun write-all-cols (csv)
      (loop for fun in fun-lst
            do (funcall fun csv)))))

(defun extract-all ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* (make-huge-if-huge *csv-name*)) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-all-cols)))

(defun build-nohtml ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* (make-huge-if-huge *csv-name*)) :direction :input)
    (cl-csv:read-csv strm :row-fn #'write-csv)))

(defun build-chunks ()
  (with-open-file (strm (format nil "~a~a" *_DIR_* (make-huge-if-huge *csv-name*)) :direction :input)
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
                  (make-huge-if-huge *processed-dirname*)
                  dir
                  id))))

(defclass subset ()
  ((start-index :reader start-index :initarg :start-index)
   (end-index :reader end-index :initarg :end-index)
   (subset-id :reader subset-id :initarg :subset-id)))

(defparameter *subsets*
  (mapcar
    (lambda (subset)
      (destructuring-bind (start-index end-index subset-id) subset
        (make-instance 'subset
                       :start-index start-index
                       :end-index end-index
                       :subset-id subset-id)))
    (list
      ;(list  000000  100000 1) ; 100k training data
      ;(list  100000  200000 2) ; 100k test data
      ;(list  200000  201000 3) ; 1k dataset (not used)
      ;(list  000000 1000000 4) ; 1M training data
      ;(list 1000000 1100000 5) ; 100k test data for 1M training data
      (list 1000000 1001000 6) ; 1k test data for 1M training data
      (list 1001000 1002000 7)  ; 2nd 1k test data for 1M training data
      )))

(defun create-all-symlinks ()
  (let ((*huge-p* t))
    (dolist (chunk-type (list 'tag 'title))
      (dolist (subset *subsets*)
        (with-slots (start-index end-index subset-id) subset
          (create-symlinks start-index end-index chunk-type subset-id))))))

(defun last-dir (dir)
  (cl-ppcre:scan-to-strings
    "((?<=^)|(?<=/))[^/]+$"
    (string-right-trim (list #\/) dir)))

(defun create-all-post-ids-csv ()
  (dolist (chunk-type (list 'tag 'title))
    (let ((*huge-p* t)) 
      (dolist (subset (mapcar #'subset-id *subsets*)) 
        (format t "working subset ~a and chunk-type ~a~%" subset chunk-type)
        (let ((dir
                (format nil "~a-subset-~a/~a" (lowercase chunk-type) subset (make-huge-if-huge *processed-dirname*))))
          (create-post-ids-csv dir))))
    (let ((*huge-p* nil))
      (format t "working small set for chunk-type ~a~%" chunk-type)
      (let ((dir
              (format nil "~a/~a" (lowercase chunk-type) (make-huge-if-huge *processed-dirname*))))
        (create-post-ids-csv dir)))))

(defun create-post-ids-csv (dir)
  (let ((post-ids-csv (format nil "~a/~a.csv" dir (last-dir dir))))
    (with-cwd *_DIR_*
      (let ((post-ids
              (let ((res))
                (when (probe-file post-ids-csv)
                  (delete-file post-ids-csv))
                (cl-fad:walk-directory
                  dir
                  (lambda (file)
                    (push (file-namestring file) res)))
              (reverse res))))
        (with-open-file (strm post-ids-csv :direction :output :if-exists :supersede :if-does-not-exist :create)
          (format strm "~{~a~%~}" post-ids))))))

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
              for group = 0 then (1+ group)
              do (let ((src-paths-as-string
                         (format nil "~{~a~^~%~}" src-paths)))
                   (let ((des-paths-as-string
                           (format nil "~{~a~^~%~}" des-paths)))
                     (format t "sending ~a as group ~a~%" cluster-num group)
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
    (let ((out-file (make-huge-if-huge "chunks.csv")))
      (let ((strm))
        (defun write-chunks (csv)
          (incf cnt)
          (when (= cnt 1)
            (setf strm (open out-file :direction :output :if-exists :supersede :if-does-not-exist :create)))
          (format t "converting line ~a~%" cnt)
          (let ((posts-id (csv-val 'posts-id csv)))
            (when (not (= cnt 1))
              (dolist (chunk-type (list 'title 'body 'tag))
                (let ((txt (file-string (format nil "~a/~a/~a/~a"
                                                (chunk-type->folder chunk-type)
                                                (make-huge-if-huge *processed-dirname*)
                                                (get-dir posts-id)
                                                posts-id))))
                  (let ((chunks (cl-ppcre:split #?"\n" txt)))
                    (dolist (chunk chunks)
                      (cl-csv:write-csv-row (list (incf chunk-cnt) posts-id chunk (lowercase chunk-type)) :stream strm))))))))))))

(defun remove-long-lines (input-csv)
  (with-open-file (strm (format nil "~a~a" *_DIR_* input-csv) :direction :input)
    (cl-csv:read-csv strm :row-fn #'maybe-remove-long-line)))

(defun trim-chunks ()
  (remove-long-lines "chunks.csv"))

(defun trim-chunks-huge ()
  (remove-long-lines "chunks-huge.csv"))

(defun valid-string-p (csv)
  (let ((max-length 255))
    (every
      (lambda (str)
        (<= (length str) max-length))
      csv)))

(defun maybe-remove-long-line (csv)
  (when (valid-string-p csv)
    (cl-csv:write-csv-row csv :stream *standard-output*)))

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



