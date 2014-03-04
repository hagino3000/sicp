; Ben
; 素直で良い
(let ((a 1))
 (define (f x)
  (define b (+ a x)) ; a = 1
  (define a 5)       ; a = 5
  (+ a 5))
 (f 10))

; Alyssa
; JavaScriptのvarの動き
(let ((a 1))
 (define (f x)
  (define b (+ a x)) ; a = undefined
  (define a 5)
  (+ a 5))
 (f 10))

; Eva
; 実装が難しい
(let ((a 1))
 (define (f x)
  (define b (+ a x)) ; a = 5
  (define a 5)
  (+ a 5))
 (f 10))
