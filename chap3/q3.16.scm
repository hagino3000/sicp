(use gauche.test)

(define (count-pairs x)
 (if (not (pair? x))
  0
  (+ (count-pairs (car x))
     (count-pairs (cdr x))
     1)))



(print (count-pairs (list 1 2 3)))
(print (count-pairs (list '(1) 2 3)))
(print (count-pairs (list '(1 2) '(3 4) 3)))

;(define p1 (list 1 2))
;(define p2 (list 1 p1))
;(set-cdr! p1 p2)
;(print (count-pairs (list p1 3 4)))
