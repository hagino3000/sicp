
(define (require p) (if (not p) (amb)))

(define (an-element-of items)
 (require (not (null? items)))
 (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
 (amb n (an-interger-starting-from (+ n 1))))


(define (a-pythagorean-triple-between low high)
 (let ((i (an-integer-between low high)))
  (let ((j (an-integer-between i high)))
   (let ((k (an-integer-between j high)))
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))))

; requireをつかって書ける
;(define (an-integer-between low high)
;  (require (< low high))
;  (amb low (an-integer-between (+ low 1) high)))
;
(define (an-integer-between low high)
  (if (< low high)
    (amb low (an-integer-between (+ low 1) high))
    high))
