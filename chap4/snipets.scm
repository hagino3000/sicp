
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

; Q4.30
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (for-each proc items)
 (if (null? items)
     'done
     (begin (proc (car items))
            (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
         
(define (p1 x)
 (set! x (cons x '(2)))
 x)

(define (p2 x)
 (define (p e)
  e
  x)
 (p (set! x (cons x '(2)))))
