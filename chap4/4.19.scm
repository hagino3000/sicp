; Ben
; $BAGD>$GNI$$(B
(let ((a 1))
 (define (f x)
  (define b (+ a x)) ; a = 1
  (define a 5)       ; a = 5
  (+ a 5))
 (f 10))

; Alyssa
; JavaScript$B$N(Bvar$B$NF0$-(B
(let ((a 1))
 (define (f x)
  (define b (+ a x)) ; a = undefined
  (define a 5)
  (+ a 5))
 (f 10))

; Eva
; $B<BAu$,Fq$7$$(B
(let ((a 1))
 (define (f x)
  (define b (+ a x)) ; a = 5
  (define a 5)
  (+ a 5))
 (f 10))
