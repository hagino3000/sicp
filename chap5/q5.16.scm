(load "./register_machine_with_trace.scm")

(define factorial-machine
 (make-machine
  '(n val continue)
  (list
   (list '= =)
   (list '* *)
   (list '- -))
  '(
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


(define n 2)
(set-register-contents! factorial-machine 'n n)
(factorial-machine 'reset-instruction-count)
(start factorial-machine)
(print "n=>" n " val=>" (get-register-contents factorial-machine 'val))

(factorial-machine 'trace-on)
(set-register-contents! factorial-machine 'n n)
(factorial-machine 'reset-instruction-count)
(start factorial-machine)
(print "n=>" n " val=>" (get-register-contents factorial-machine 'val))

(factorial-machine 'trace-off)
(set-register-contents! factorial-machine 'n n)
(factorial-machine 'reset-instruction-count)
(start factorial-machine)
(print "n=>" n " val=>" (get-register-contents factorial-machine 'val))
