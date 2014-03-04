(define (f x)
 (letrec ((even?
           (lambda (n)
            (if (= n 0)
             true
             (odd? (- n 1)))))
          (odd?
           (lambda (n)
            (if (= n 0)
             false
             (even? (- n 1))))))
  
   (define (even? x) (+ x 1))
   (even? x)))

