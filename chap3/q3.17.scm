(use gauche.test)

(define (count-pairs x)

 (define (walk x checked-pairs)
   (define (is-checked p)
    (define (check check-list)
     (print check-list)
     (cond ((not (pair? check-list)) #f)
           ((eq? (car check-list) p) #t)
           (else (check (cdr check-list)))))
    (check checked-pairs))

   (define (add-checked p)
    (begin (if (pair? checked-pairs)
        (append! checked-pairs p)
        (set! checked-pairs (list p)))))

   (cond ((not (pair? x)) 0)
         ((is-checked x) 0)
         (else (add-checked x) (print (car x)) (print (cdr x))
               (+ (walk (car x) checked-pairs)
                  (walk (cdr x) checked-pairs)
                  1))))
  (walk x '()))


(print (count-pairs (list 1 2 3)))
(print (count-pairs (list '(1) 2 3)))
(print (count-pairs (list '(1 2) '(3 4) 3)))

(define p1 (list 1 2))
(define p2 (list 1 p1))
(set-cdr! p1 p2) ; p1 -> (1 (1 p1))
;(print (count-pairs (list p1 3 4)))
