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
;   }
; }
(define root-env the-empty-environment)
(define env1 (extend-environment '(a b c) '(1 2 3) root-env))
(define env2 (extend-environment '(d e f a) '(4 5 6 7) env1))


; lexical-address-lookupのテスト
(print "(0, 0) of env2 => " (lexical-address-lookup '(0 0) env2))
(print "(0, 1) of env2 => " (lexical-address-lookup '(0 1) env2))
(print "(0, 2) of env2 => " (lexical-address-lookup '(0 2) env2))
(print "(0, 3) of env2 => " (lexical-address-lookup '(0 3) env2))

(print "(1, 0) of env2 => " (lexical-address-lookup '(1 0) env2))
(print "(1, 1) of env2 => " (lexical-address-lookup '(1 1) env2))
(print "(1, 2) of env2 => " (lexical-address-lookup '(1 2) env2))

(print "(0, 0) of env1 => " (lexical-address-lookup '(0 0) env1))
(print "(0, 1) of env1 => " (lexical-address-lookup '(0 1) env1))
(print "(0, 2) of env1 => " (lexical-address-lookup '(0 2) env1))

; error
;(print "(1, 0) of env1 => " (lexical-address-lookup '(1 0) env1))

; lexical-address-set!のテスト
(print "Set 99 to (0, 0) of env2")
(lexical-address-set! '(0 0) 99 env2)
(print "(0, 0) of env2 => " (lexical-address-lookup '(0 0) env2))

(print "Set 77 to (1, 2) of env2")
(lexical-address-set! '(1 2) 77 env2)
(print "(1, 2) of env2 => " (lexical-address-lookup '(1 2) env2))
