; Q4.24a
; $B;O$a$ND6=[4DI>2A5!$HK\@a$G$NHG$NB.EY$NHf3S$9$k$?$a$$$/$D$+@_7W$7$F<B9T$;$h(B
;
(load "./eval_q4.20.scm")

; Printing
(define (user-print object)
 (if (compound-procedure? object)
  (display (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<procedure-env>))
  (display object)))

(define input-prompt ";;; M-Eval input:")
(define (announce-input string)
 (display string) (newline))
(define (prompt-for-input string)
 (newline) (newline) (display string) (newline))

(define output-prompt ";;; M-Eval value:")
(define (announce-output string)
 (newline) (display string) (newline))


; Eval with logs
;(define (go exp)
; (prompt-for-input input-prompt)
; (announce-input exp)
; (let ((output (eval exp the-global-environment)))
;   (announce-output output-prompt)
;   (user-print output)))

; Eval
(define test-time 10000)
(define (go exp)
  (prompt-for-input input-prompt)
  (announce-input exp)
  (time 
    (dotimes (i test-time) 
      (eval exp the-global-environment)
    )
  )
)


; Exam body
(define the-global-environment (setup-environment))
(go (list '* 20 100))
(go (list 'define 'a 2))
(go (list 'lambda (list 'a 'b) (list '* 'a 10) (list '+ 'a 'b)))
(go (list (list 'lambda (list 'a 'b) (list '* 'a 10) (list '+ 'a 'b)) 1 2))

;(define (factorial n)
; (define (fact-iter n ans)
;  (if (= n 0) ans (fact-iter (- n 1) (* n ans))))
; (fact-iter n 1))
(go (list 'define (list 'factorial 'n)
 (list 'define (list 'fact-iter 'n 'ans)
  (list 'if (list '= 'n 0) 'ans (list 'fact-iter (list '- 'n 1) (list '* 'n 'ans))))
 (list 'fact-iter 'n 1)))

; Apply factorial
(go (list 'factorial 10))

;(define (uselet b)
; (let ((double (* b 2))
;       (triple (* b 3)))
; (- triple double)))
(go (list 'define (list 'uselet 'b)
 (list 'let (list
                  (list 'double (list '* 'b 2))
                  (list 'triple (list '* 'b 3))
             )
 (list '- 'triple 'double))))

; Apply proc which using let
(go (list 'uselet 1))
