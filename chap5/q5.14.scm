(load "./register_machine.scm")

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


(define (test n)
 (if (eq? n 0)
    'done
    (begin
        (set-register-contents! factorial-machine 'n n)
        ((factorial-machine 'stack) 'initialize)
        (start factorial-machine)
        (print "n=>" n " val=>" (get-register-contents factorial-machine 'val))
        ((factorial-machine 'stack) 'print-statistics)
        (test (- n 1)))))

(test 10)
