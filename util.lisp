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
