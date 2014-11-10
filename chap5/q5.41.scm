(load "./operations.scm")
(load "./register_machine_with_trace.scm")

; find-variableのテスト
(print (find-variable 'c '((y z) (a b c d e) (x y))))
(print (find-variable 'x '((y z) (a b c d e) (x y))))
(print (find-variable 'w '((y z) (a b c d e) (x y))))
