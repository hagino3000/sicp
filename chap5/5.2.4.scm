(load "./register_machine.scm")

(define plus-machine
 (make-machine
  '(a b c d)
  (list
   (list 'plus +)
   (list '= =))
  '(start
      (assign d (op plus) (const 10) (reg b))
      (assign c (op plus) (reg a) (reg b))
      (goto (label done))
   done)))


(set-register-contents! plus-machine 'a 3)
(set-register-contents! plus-machine 'b 7)
(start plus-machine)
(print (get-register-contents plus-machine 'c))
(print (get-register-contents plus-machine 'd))
((plus-machine 'stack) 'print-statistics)

