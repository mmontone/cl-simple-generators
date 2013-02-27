(defpackage simple-generators
  (:use :cl))

(in-package simple-generators)

(define-condition yield ()
  ((value :initarg :value
	  :accessor value
	  :initform (error "Yield a value"))))

(defmacro define-generator-function (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (defun ,(intern (format nil "~a*" name)) ,args
       (lambda ()
	 ,@body))))

(defun yield (value)
  (with-simple-restart (continue "Continue after yield of ~A" value)
    (error 'yield :value value))
  value)

(defun run-generator (producer consumer)
  (handler-bind
      ((yield (lambda (yield)
		(funcall consumer (value yield))
		(invoke-restart 'continue))))
    (funcall producer)))

(defmacro with-generator (producer args &body body)
  `(run-generator ,producer (lambda ,args
			      ,@body)))

(define-generator-function list-generator (list)
  (loop for x in list
       do (yield x)))

(define-generator-function or-g (gen)
  (let (result)
    (block or-g-block
      (run-generator gen
		     (lambda (x)
		       (when x
			 (setf result t)
			 (return-from or-g-block)))))
    result))

(define-generator-function compose-g (f g)
  (run-generator g
		 (lambda (x)
		   (yield (funcall f x)))))

(define-generator-function map-g (f gen)
  (run-generator gen
		 (lambda (x)
		   (yield (funcall f x)))))

(defun generator-list (gen)
  (let (result)
    (run-generator gen
		   (lambda (x)
		     (push x result)))
    (nreverse result)))

(define-generator-function fold-g (f s gen)
  (let ((r s))
    (run-generator gen
		   (lambda (x)
		     (setf r (funcall f r x))))
    r))

;; File reading, character after character
(defun file-g (filepath) 
  (with-open-file (file filepath :direction :input)
    (let ((char (read-char file nil)))
      (loop while char
	do (progn
	     (yield char)
	     (setf char (read-char file nil)))))))

;; Transform a producer of chars to a producer of lines
(defun lines-g (gen)
  (let (line)
    (with-generator gen (char)
      (if (equalp char #\Newline)
	  (progn
	    (yield (reverse line))
	    (setf line nil))
	  (push char line)))))		  
    
