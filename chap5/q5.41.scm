(load "./operations.scm")
(load "./register_machine_with_trace.scm")

; テスト用の環境を作る
; { // env1
;   a = 1
;   b = 2
;   c = 3
;   {
;     // env2
;     d = 4
;     e = 5
;     f = 6
;     a = 7
;     {
;       // env3
;       b = 9
;     }
;   }
; }
(define root-env the-empty-environment)
(define env1 (extend-environment '(a b c) '(1 2 3) root-env))
(define env2 (extend-environment '(d e f a) '(4 5 6 7) env1))
(define env3 (extend-environment '(b) '(9) env2))


; find-variableのテスト
(print "a of env3 => " (find-variable 'a env3))
(print "b of env3 => " (find-variable 'b env3))
(print "c of env3 => " (find-variable 'c env3))
(print "x of env3 => " (find-variable 'x env3))
