(use gauche.test)

; 対を保持する値
(define current-pointer '())

; 構造 l に x が含まれているかを返す手続
(define (in-list? x l)
 (cond ((not (pair? l)) #f)
       ((eq? x (car l)) #t)
       (else (or (in-list? x (car l)) (in-list? x (cdr l))))))

; 構造 l にループが含まれているかを返す手続
(define (has-loop? l)
 (cond ((not (pair? l)) #f)
       (else (set! current-pointer l)
             (cond ((or 
                    (and (pair? (car l)) (in-list? current-pointer (car l)))
                    (and (pair? (cdr l)) (in-list? current-pointer (cdr l)))
                   ) #t)
             (else (or (has-loop? (car l)) (has-loop? (cdr l))))))))


(test-section "in-list?")
(test "No list given" #f (lambda () (in-list? 0 0)))
(test "No match" #f (lambda () (in-list? 0 (list 1 2 3))))
(test "No match" #f (lambda () (in-list? '(1 1) (list 1 2 3))))
(test "Match" #t (lambda () (in-list? 3 (list 1 2 3))))
(test "Include pair" #t (lambda () (in-list? 3 (list (cons 1 2) (cons 3 4)))))
(test "Include list" #t (lambda () (in-list? 2 (list (list 1 2 3) (list 4 5 6)))))
(test "Include list" #t (lambda () (in-list? 5 (list (list 1 2 3) (list 4 5 6)))))
(test "List Match" #t (lambda () (let ((p1 '(1 2)))
                                      (in-list? p1 (list p1)))))
(test "List Match Inner" #t (lambda () (let ((p1 '(1 2)))
                                      (in-list? p1 (list (list 1 p1) 2 3)))))
                                  

(test-section "has-loop?")
(test "No list given" #f (lambda () (has-loop? 1)))
(test "No loop given" #f (lambda () (has-loop? (list 1 2 3))))
(test "No loop given" #f (lambda () (has-loop? (list (list 0 1) 2 3))))
(test "No Loop given" #f (lambda () 
                          (let (
                           (z1 '(1 2 3)) 
                           (z2 '(1 2 z1))
                          )
                          (has-loop? (list z2 z1)))))
;無限ループする
;(test "Loop given" #t (lambda () 
;                       (let (
;                           (z3 '(1 1)) 
;                          )
;                          (set-cdr! z3 z3)
;                          (has-loop? z3))))
;
