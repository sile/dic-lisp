(in-package :dic)

(defmacro each-file-line ((line filepath &rest keys) &body body)
  `(with-open-file (#1=#:in ,filepath ,@keys)
     (loop FOR ,line = (read-line #1# nil nil nil)
           WHILE ,line
       DO
       (locally ,@body))))

(defun unique! (list test)
  (labels ((recur (front rear)
             (when rear
               (if (funcall test (car front) (car rear))
                   (progn (setf (cdr front) (cdr rear))
                          (recur front (cdr rear)))
                 (recur (cdr front) (cdr rear))))))
    (recur list (cdr list))
    list))

(defmacro a.when (exp &body body)
  `(let ((it ,exp))
     (when it 
       ,@body)))

(defun subseq@ (seq start end)
  (if (< (length seq) end)
      seq
    (subseq seq start end)))

(defun basename (pathstring)
  (let ((path (parse-namestring pathstring)))
    (format nil "~A~@[.~A~]" (pathname-name path) (pathname-type path))))

;; '(a b c &optional c &key (d e)) -> '(a b c d)
(defun collect-varsym (args)
  (mapcar (lambda (a)
            (if (consp a) (car a) a))
          (remove-if (lambda (a)
                       (and (symbolp a) (string= "&" a :end2 1)))
                     args)))
#+SBCL
(defmacro main-lambda (args &body body)
  (let ((usage nil))
    (sb-ext:disable-debugger)

    (when (stringp (car body))
      (setf usage (car body)
	    body  (cdr body)))
    
    `(lambda ()
       ;; When failed arguments destructuring, show documentation and exit
       ,(when usage
          `(handler-case 
            (destructuring-bind ,args (cdr sb-ext:*posix-argv*) 
              (declare (ignore ,@(collect-varsym args))))
            (error ()
              (format *error-output* "~&~?~%~%" 
                      ,usage
                      (list (basename (car sb-ext:*posix-argv*))))
              (sb-ext:quit :unix-status 1))))
       
       (destructuring-bind ,args (cdr sb-ext:*posix-argv*)
         (handler-case
          (locally ,@body)
          (sb-int:simple-stream-error ()
             (sb-ext:quit :unix-status 0 :recklessly-p t))
          (condition (c)
             (format *error-output* "~&ERROR: ~A~%" c)
             (sb-ext:quit :unix-status 1)))
         (sb-ext:quit :unix-status 0)))))
