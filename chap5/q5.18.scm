(load "./register_machine_with_trace3.scm")

(define factorial-machine
 (make-machine
  '(n val continue)
  (list
   (list '= =)
   (list '* *)
   (list '- -))
  '(
   start
      (assign continue (label fact-done))
   fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
   after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
   base-case
      (assign val (const 1))
      (goto (reg continue))
   fact-done)))


(define n 10)
(print "trace-on")
((factorial-machine 'register-trace-on) 'n)
(set-register-contents! factorial-machine 'n n)
(start factorial-machine)
(print "n=>" n " val=>" (get-register-contents factorial-machine 'val))

(print "trace-off")
((factorial-machine 'register-trace-off) 'n)
(set-register-contents! factorial-machine 'n n)
(start factorial-machine)
(print "n=>" n " val=>" (get-register-contents factorial-machine 'val))
