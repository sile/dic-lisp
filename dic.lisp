(in-package :dic)

;;;;;;
;;; special variables
(defvar *ENTRY_DELIMITER* "-=+=-=+=-=+=-=+=-=+=-")
(defvar *tmpdir* #P"/tmp/")
(defconstant +ESC-CHAR+ (code-char #o33))

;;;;;;
;;; structs
(defstruct entry 
  (key     "" :type simple-string)
  (title   "" :type simple-string)
  (summary "" :type simple-string)
  (data    "" :type (simple-array (unsigned-byte 8))))

(defstruct dic
  index
  entrys)

;;;;;;
;;; internal functions
;; build dictionary for on-memory representation
(defun collect-title (entrys)
  (let ((titles (map 'list #'entry-key entrys)))
    (unique! (sort titles #'string<) #'string=)))

(defun build-title-index (titles)
  (let ((title-list-file (merge-pathnames  
                          (make-pathname :name (symbol-name (gentemp))
                                         :type "title.list")
                          *tmpdir*))
        (title-index-file (merge-pathnames 
                           (make-pathname :name (symbol-name (gentemp))
                                          :type "title.index")
                           *tmpdir*)))

    (with-open-file (out title-list-file :direction :output)
      (dolist (title titles)
        (write-line title out)))

    (unwind-protect
        (progn
          (dawg:build :input title-list-file :output title-index-file)
          (unwind-protect
              (dawg:load title-index-file)
            (delete-file title-index-file)))
      (delete-file title-list-file))))
  
(defun parse-dictionary (source &aux entrys)
  (let (key summary title data)
    (each-file-line (line source)
      (cond ((string= line *ENTRY_DELIMITER*)
             (push (make-entry :key key
                               :title title
                               :summary summary
                               :data (creole:string-to-octets
                                      (apply #'concatenate 'string (nreverse data))))
                   entrys)
             (setf key nil
                   title nil
                   summary nil
                   data nil))
            ((null key)
             (setf key line))
            ((null title)
             (setf title line))
            ((null summary)
             (setf summary line))
            (t
             (setf data (nconc (list #.(string #\Newline) line) data)))))
    (coerce (nreverse entrys) 'vector)))

(defun make-id->entrys-map (title-idx title-count entrys)
  (let* ((da title-idx)
         (limit title-count)
         (id->entrys (make-array limit :initial-element nil)))
    (loop FOR i FROM (1- (length entrys)) DOWNTO 0
          FOR e = (aref entrys i)
          FOR key = (entry-key e)
      DO
      (push e (aref id->entrys (dawg:get-id key da))))
    id->entrys))


;; word-lookup methods 
(defun exact-lookup (word da id->ents)
  (a.when (dawg:get-id word da)
    (aref id->ents it)))

(defun prefix-lookup (word da id->ents limit &aux acc)
  (block :lookup
    (dawg:each-predictive (id) (word da)
      (let ((ents (aref id->ents id)))
        (dolist (e ents)
          (when (minusp (decf limit))
            (return-from :lookup))
          (push e acc)))))
  (nreverse acc))

(defun include-lookup (word da id->ents &aux acc)
  (dawg:each-common-prefix (id len) (word da)
    (declare (ignore len))
    (setf acc (append (aref id->ents id) acc)))
  acc)


;; format word-lookup result
(defun format-title (title)
  (format nil "~c[1;31m# ~a~c[0m" +ESC-CHAR+ title +ESC-CHAR+))

(defun fmt-body1 (body)
  (with-output-to-string (out)
    (labels ((recur (start open-p)
               (let ((p (position #\` body :start start)))
                 (cond ((null p)
                        (write-string body out :start start))
                       (open-p
                        (write-string body out :start start :end p)
                        (format out "~c[1m" +ESC-CHAR+)
                        (recur (1+ p) nil))
                       (t
                        (write-string body out :start start :end p)
                        (format out "~c[0m" +ESC-CHAR+)
                        (recur (1+ p) t))))))
      (recur 0 body))))

(defun fmt-body2 (body)
  (with-output-to-string (out)
    (labels ((recur (start)
               (let* ((beg (position #\{ body :start start))
                      (end (position #\} body :start (1+ (or beg 0)))))
                 (if (or (null beg) (null end))
                     (write-string body out :start start)
                   (progn
                     (write-string body out :start start :end beg)
                     (format out "~c[1;4;32m" +ESC-CHAR+)
                     (write-string body out :start (1+ beg) :end end)
                     (format out "~c[0m" +ESC-CHAR+)
                     (recur (1+ end)))))))
      (recur 0))))

(defun fmt-body3 (body)
  (with-output-to-string (out)
    (labels ((recur (start)
               (let* ((beg (position #\[ body :start start))
                      (end (position #\] body :start (1+ (or beg 0)))))
                 (if (or (null beg) (null end) )
                     (write-string body out :start start)
                   (progn
                     (write-string body out :start start :end beg)
                     (format out "~c[1;7;34m" +ESC-CHAR+)
                     (write-string body out :start (1+ beg) :end end)
                     (format out "~c[0m" +ESC-CHAR+)
                     (recur (1+ end)))))))
      (recur 0))))

(defun format-body (body)
  (fmt-body1 (fmt-body2 (fmt-body3 body))))


;;;;;;
;;; external functions
(defun load (source)
  (let* ((entrys (parse-dictionary source))
         (titles (collect-title entrys))
         (title-idx (build-title-index titles))
         (id->entrys (make-id->entrys-map title-idx (length titles) entrys)))
    (make-dic :index title-idx
              :entrys id->entrys)))

(defun lookup (word dic &key (type :prefix) (limit 3))
  (let ((word (string-downcase word)))
    (with-slots (index entrys) dic
      (ecase type
        (:exact (subseq@ (exact-lookup word index entrys) 0 limit))
        (:prefix (prefix-lookup word index entrys limit))
        (:include (subseq@ (include-lookup word index entrys) 0 limit))))))

(defun make-command-and-die (source-text-dic command-path &key (default-result-limit 1))
  (declare (ignorable command-path))
  #+SBCL
  (let ((dic (load source-text-dic)))
    (sb-ext:save-lisp-and-die 
     command-path 
     :executable t
     :toplevel (main-lambda (word &optional (limit default-result-limit))
                 "~A: word [show-result-limit]"
                 (when (stringp limit)
                   (setf limit (parse-integer limit)))
                 (let ((result (lookup word dic
                                       :type :prefix
                                       :limit limit)))
                   (dolist (e result)
                     (format t "~&~a~%~A~%" 
                             (format-title (entry-title e))
                             (format-body (creole:octets-to-string (entry-data e)))))))))
  #-SBCL
  (error "This function only support SBCL"))
