(use gauche.test)

; 対を保持するリスト
(define pointer-list '())

; 対を保持するリストに新たに対を追加する手続
(define (append-pointer-list x)
  (if (pair? x)
    (if (pair? pointer-list)
        (set! pointer-list (cons pointer-list x))
        (set! pointer-list (list x)))))

; リスト l に x が含まれているかを返す手続
(define (in-list? x l)
 (cond ((not (pair? l)) #f)
       ((eq? x (car l)) #t)
       (else (in-list? x (cdr l)))))

; リストlにループが含まれているかを返す手続
(define (has-loop? l)
 (cond ((not (pair? l)) #f)
       ((in-list? (car l) pointer-list) #t) 
       (else (append-pointer-list (car l))
             (has-loop? (cdr l)))))


(print (has-loop?  (list 1)))
(print (has-loop?  (list 1 2 3 4 5)))
(print (has-loop?  (list '(1 2) '(3 4) 5 6)))

(define z1 '(1 2 3))
(define z2 '(1 2 z1))
(print (has-loop? (list z2 z1)))

(define z3 '(1 2 3 4 5 6))
(set-cdr! z3 z3)
(print (has-loop? z3))

(define z4 (list 1 2 3 4))
(set! z4 (list 1 (cons z4 1) 2 3))
(print (has-loop? z4))
