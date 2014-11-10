(load "./operations.scm")
(load "./compiler_lexical.scm")
(load "./register_machine_with_trace.scm")

(define compiledeval-operations
  (list
   (list 'read read)
   (list 'list list)
   (list 'cons cons)
   (list 'false? false?)
   (list 'true? true?)
   (list 'lexical-address-lookup lexical-address-lookup)
   (list 'lexical-address-set! lexical-address-set!)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'make-compiled-procedure make-compiled-procedure)

   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   (list 'true? true?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)
   (list 'get-global-environment get-global-environment))
   )

(define the-global-environment (setup-environment))

(define compiled
  (compile
    ;'(+ 10 20 30)
    '(begin
        (define v 10)
        (define (factorial-alt n)
            (if (= n 1)
            1
            (* n (factorial-alt (- n 1)))))
        (factorial-alt v))
    'val
    'next
    the-global-environment
    ))

;(print (caddr compiled))
(print "Compiled")

(define factorial-machine
 (make-machine
  '(val env proc argl continue)
  compiledeval-operations
  (caddr compiled)
  ))

(print "Start machine")

(factorial-machine 'trace-on)
(set-register-contents! factorial-machine 'env the-global-environment)
(start factorial-machine)
(print "val=>" (get-register-contents factorial-machine 'val))
