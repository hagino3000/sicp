; amb評価機の実装
;
(define false #f)
(define true #t)

(define (true? x)
 (not (eq? x false)))
(define (false? x)
 (eq? x false))

; eval
(define (ambeval exp env succeed fail)
 ((analyze exp) env succeed fail))


(define (analyze exp)
 (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
       ((quoted? exp) (analyze-quoted exp))
       ((variable? exp) (analyze-variable exp))
       ((assignment? exp) (analyze-assignment exp))
       ((definition? exp) (analyze-definition exp))
       ((amb? exp) (analyze-amb exp))
       ((if? exp) (analyze-if exp))
       ((lambda? exp) (analyze-lambda exp))
       ((begin? exp) (analyze-sequence (begin-actions exp)))
       ((cond? exp) (analyze (cond->if exp)))
       ((application? exp) (analyze-application exp))
       (else
        (error "Unknown expression type -- ANALYZE" exp))))

; self-evaluating
(define (self-evaluating? exp)
 (cond ((number? exp) true)
       ((string? exp) true)
       (else false)))

(define (analyze-self-evaluating exp)
 (lambda (env succeed fail)
  (succeed exp fail)))

; quote
(define (quoted? exp)
 (tagged-list? exp 'quote))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (text-of-quotation exp) (cadr exp))

; variable
(define (variable? exp) (symbol? exp))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

; tagged-list
(define (tagged-list? exp tag)
 (if (pair? exp)
     (eq? (car exp) tag)
     false))

; amb
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
 (let ((cprocs (map analyze (amb-choices exp))))
  (lambda (env succeed fail)
   (define (try-next choices)
    (if (null? choices)
     (fail)
     ((car choices) env
                    succeed
                    (lambda ()
                     (try-next (cdr choices))))))
   (try-next cprocs))))


; assignment
(define (assignment? exp)
 (tagged-list? exp 'set!))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))


(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definition
(define (definition? exp)
 (tagged-list? exp 'define))

(define (analyze-definition exp)
 (let ((var (definition-variable exp))
       (vproc (analyze (definition-value exp))))
 (lambda (env)
  (define-variable! var (vproc env) env)
  'ok)))

(define (analyze-definition exp)
 (let ((var (definition-variable exp))
       (vproc (analyze (definition-value exp))))
  (lambda (env succeed fail)
   (vproc env
          (lambda (val fail2)
           (define-variable! var val env)
           (succeed 'ok fail2))
          fail))))

(define (definition-variable exp)
 (if (symbol? (cadr exp))
     (cadr exp)
     (caadr exp)))

(define (definition-value exp)
 (if (symbol? (cadr exp))
     (caddr exp)
     (make-lambda (cdadr exp)   ; 仮パラメタ
                  (cddr exp)))) ; 本体

; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
 (cons 'lambda (cons parameters body)))


; if
(define (if? exp) (tagged-list? exp 'if))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; pred-valueを得るための述語の評価の成功継続
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; 述語の評価の失敗継続
             fail))))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
 (if (not (null? (cdddr exp)))
     (cadddr exp)
     'false))

(define (make-if predicate consequent alternative)
 (list 'if predicate consequent alternative))

; begin
; 与えられた式を先頭から順に評価する構文
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; a を呼び出す時の成功継続
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; a を呼び出す時の失敗継続
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (sequence->exp seq)
 (cond ((null? seq) seq)
       ((last-exp? seq) (first-exp seq))
       (else (make-begin seq))))

(define (make-begin seq)
 (cons 'begin seq))

; 式
(define (application? exp) (pair? exp))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; このaprocの成功継続
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; get-argsの再帰呼出の成功継続
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))


(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (operator exp) (car exp))
(define (operands  exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
 (eq? (cond-predicate clause) 'else))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
 (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
 (if (null? clauses)
  'false
  (let ((first (car clauses))
        (rest (cdr clauses)))
   (if (cond-else-clause? first)
       (if (null? rest)
           (sequence->exp (cond-actions first))
           (error "ELSE clause isn't last -- COND->IF"
                  clauses))
       (make-if (cond-predicate first)
                (sequence->exp (cond-actions first))
                (expand-clauses rest))))))

; procedure
(define primitive-procedures
 (list (list 'car car)
       (list 'cdr cdr)
       (list 'cons cons)
       (list 'null? null?)
       (list 'list list)
       (list 'print print)
       (list 'not not)
       (list '+ +)
       (list '- -)
       (list '* *)
       (list '/ /)
       (list '= =)
 ))
(define (primitive-procedure? proc)
 (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedure-names)
 (map car
  primitive-procedures))
(define (primitive-procedure-objects)
 (map (lambda (proc) (list 'primitive (cadr proc)))
  primitive-procedures))
(define apply-in-underlying-acheme apply)
(define (apply-primitive-procedure proc args)
 (apply-in-underlying-acheme
  (primitive-implementation proc) args))

(define (make-procedure parameters body env)
 (list 'procedure parameters body env))
(define (compound-procedure? p)
 (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
; q 4.16
(define (procedure-body p) (caddr p))
;(define (procedure-body p)
; (scan-out-defines (caddr p)))

(define (procedure-environment p) (cadddr p))

; environment
(define (lookup-variable-value var env)
 (define (env-loop env)
  (define (scan vars vals)
   (cond ((null? vars)
          (env-loop (enclosing-environment env)))
    ((eq? var (car vars))
     (let ((result (car vals)))
      (if (unassigned? result)
          (error "Unassigned variable" var)
          result)))
    (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
     (error "Unbound variable" var)
     (let ((frame (first-frame env)))
      (scan (frame-variables frame)
            (frame-values frame)))))
  (env-loop env))

; unassigned
(define (unassigned? val)
 (eq? val '*unassigned*))

(define (extend-environment vars vals base-env)
 (if (= (length vars) (length vals))
     (cons (make-frame vars vals) base-env)
     (if (< (length vars) (length vals))
         (error "Too many arguments supplied" vars vals)
         (error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env)
 (define (env-loop env)
  (define (scan vars vals)
   (cond ((null? vars)
          (env-loop (enclosing-environment env)))
         ((eq? var (car vars))
          (set-car! vals val))
         (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
   (error "Unbound variable -- SET!" var)
   (let ((frame (first-frame env)))
    (scan (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
 (let ((frame (first-frame env)))
  (define (scan vars vals)
   (cond ((null? vars)
          (add-binding-to-frame! var val frame))
         ((eq? var (car vars))
          (set-car! vals val))
         (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
        (frame-values frame))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())


(define (make-frame variables values)
 (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
 (set-car! frame (cons var (car frame)))
 (set-cdr! frame (cons val (cdr frame))))

(define (setup-environment)
 (let ((initial-env
        (extend-environment (primitive-procedure-names)
                            (primitive-procedure-objects)
                            the-empty-environment)))
  (define-variable! 'true true initial-env)
  (define-variable! 'false false initial-env)
  initial-env))
