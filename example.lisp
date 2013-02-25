(in-package :simple-generators)

;; Basic generator
(run-generator (list-generator* (list 1 2 3))
	       #'print)

;; Or
(or-g (list-generator* (list nil nil nil)))
(or-g (list-generator* (list nil nil t)))

;; Transducer
(run-generator (compose-g* #'1+ (list-generator* (list 1 2 3)))
	       #'print)

;; Collecting
(generator-list (list-generator* (list 1 2 3)))

;; Map
(generator-list (map-g* #'parse-integer (list-generator* (list "1" "2" "3"))))

;; Foldg
(fold-g #'+ 0 (list-generator* (list 1 2 3 4 5)))

;; List comprehension example
;; [(x,y)| x <- [1..15] | y <- x/2]

(let ((gen (lambda ()
	     (loop for i from 1 to 15
		do (yield i)))))
  (generator-list (compose-g* (lambda (x)
				(let ((y (/ x 2)))
				  (cons x y)))
			      gen)))
