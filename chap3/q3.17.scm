(use gauche.test)


(define (count-pairs _x)
  (define checked-pairs '())

  (define (has-checked? p)
    (define (check check-list)
;      (print check-list)
      (cond ((not (pair? check-list)) #f)
            ((eq? (car check-list) p) #t)
            (else (check (cdr check-list)))))
    (check checked-pairs))

  (define (add-checked-pairs p)
    (if (pair? checked-pairs)
        (set! checked-pairs (cons checked-pairs p))
        (set! checked-pairs (list p)))
    checked-pairs)

  (define (walk x)
   (cond ((not (pair? x)) 0)
         ((has-checked? x) 0)
         (else  
               (add-checked-pairs x)
               ;(print "checked-pairs")
               ;(print checked-pairs)
               (+ (walk (car x))
                  (walk (cdr x))
                  1))))
  (walk _x))

(define px '(1 1))
(define py '(9 9))
(print (count-pairs (list '(1 2 3))))
(print (count-pairs (list px py 3)))
(print (count-pairs (list px px px)))


(define p1 (list 1 2))
(define p2 (list 1 p1))
(set-cdr! p1 p2) ; p1 -> (1 (1 p1))
(print (count-pairs (list p1)))
