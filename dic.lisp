(in-package :dic)

(defvar *ENTRY_DELIMITER* "-=+=-=+=-=+=-=+=-=+=-")
(defvar *tmpdir* #P"/tmp/")

(defun default-output (source-path)
  (merge-pathnames (make-pathname :type "bin") source-path))

(defun collect-title (entrys)
  (let ((titles (map 'list #'entry-key entrys)))
    (unique! (sort titles #'string<) #'string=)))

(defun build-title-index (entrys)
  (let ((titles (collect-title entrys))
        (title-list-file (merge-pathnames 
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
              (list (dawg:load title-index-file) (length titles)) ; XXX:
            (delete-file title-index-file)))
      (delete-file title-list-file))))
  
(defun parse-dictionary (source &aux entrys)
  (let (key title data)
    (each-file-line (line source)
      (cond ((string= line *ENTRY_DELIMITER*)
             (push (make-entry :key key
                               :title title
                               :data (apply #'concatenate 'string 
                                            (nreverse data)))
                   entrys)
             (setf key nil
                   title nil
                   data nil))
            ((null key)
             (setf key line))
            ((null title)
             (setf title line))
            (t
             (setf data (nconc (list #.(string #\Newline) line) data)))))
    (coerce (nreverse entrys) 'vector)))

(defstruct entry 
  (key "" :type simple-string)
  (title "" :type simple-string)
  (data "" :type simple-string))

(defun make-id->entrys-map (title-idx entrys)
  (let* ((da (first title-idx))
         (limit (second title-idx))
         (id->entrys (make-array limit :initial-element nil)))
    (loop FOR i FROM (1- (length entrys)) DOWNTO 0
          FOR e = (aref entrys i)
          FOR key = (entry-key e)
      DO
      (push e (aref id->entrys (dawg:get-id key da))))
    id->entrys))

(defstruct dic
  index
  entrys)

;; TODO: load?
(defun build (source &key (progress t))
  (declare (ignorable progress))
  (let* ((entrys (parse-dictionary source))
         (title-idx (build-title-index entrys))
         (id->entrys (make-id->entrys-map title-idx entrys)))
    (make-dic :index (first title-idx)
              :entrys id->entrys)))

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
  
(defun lookup (word dic &key (type :prefix) (limit 3))
  (let ((word (string-downcase word)))
    (with-slots (index entrys) dic
      (ecase type
        (:exact (subseq@ (exact-lookup word index entrys) 0 limit))
        (:prefix (prefix-lookup word index entrys limit))
        (:include (subseq@ (include-lookup word index entrys) 0 limit))))))
