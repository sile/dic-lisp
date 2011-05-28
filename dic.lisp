(in-package :dic)

(defvar *ENTRY_DELIMITER* "-=+=-=+=-=+=-=+=-=+=-")
(defvar *tmpdir* #P"/tmp/")

(defun default-output (source-path)
  (merge-pathnames (make-pathname :type "bin") source-path))

(defun collect-title (source &aux titles)
  (let ((title-p t))
    (each-file-line (line source)
      (when title-p
        (push (string-downcase line) titles)
        (setf title-p nil))
      (when (string= line *ENTRY_DELIMITER*)
        (setf title-p t)))
    (unique! (sort titles #'string<) #'string=)))

(defun build-title-index (source)
  (let ((titles (collect-title source))
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
              (dawg:load title-index-file)
            (delete-file title-index-file)))
      (delete-file title-list-file))))
  
(defun build (source &key (output (default-output source)) (progress t))
  (build-title-index source))




