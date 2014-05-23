(define (unless condition usual-value exceptional-value)
 (if condition exceptional-value usual-value))

(define (factorial n)
 (print "Call factorial")
 (print n)
 (unless (= n 1)
         (* n (factorial (- n 1)))
         1))


(print (factorial 5))
