; q4.22
; 解析と実行の分離
;
(define false #f)
(define true #t)

(define (true? x)
 (not (eq? x false)))
(define (false? x)
 (eq? x false))

; eval
(define (eval exp env)
 ((analyze exp) env))

(define (analyze exp)
 (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
       ((quoted? exp) (analyze-quoted exp))
       ((variable? exp) (analyze-variable exp))
       ((assignment? exp) (analyze-assignment exp))
       ((definition? exp) (analyze-definition exp))
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
 (lambda (env) exp))

; quote
(define (quoted? exp)
 (tagged-list? exp 'quote))

(define (analyze-quoted? exp)
 (let ((qval (text-of-quotation exp)))
  (lambda (env) qval)))

(define (text-of-quotation exp) (cadr exp))

; variable
(define (variable? exp) (symbol? exp))
(define (analyze-variable exp)
 (lambda (env) (lookup-variable-value exp env)))

; tagged-list
(define (tagged-list? exp tag)
 (if (pair? exp)
     (eq? (car exp) tag)
     false))


; assignment
(define (assignment? exp)
 (tagged-list? exp 'set!))

(define (analyze-assignment exp)
 (let ((var (assignment-variable exp))
       (vproc (analyze (definition-value exp))))
  (lambda (env)
   (set-variable-value! var (vproc env) env)
   'ok)))

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
  (lambda (env) (make-procedure vars bproc env))))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
 (cons 'lambda (cons parameters body)))

; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp)
 (map car (cadr exp)))
(define (let-real-parameters exp)
 (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
 (let ((names (let-parameters exp))
       (values (let-real-parameters exp))
       (body (let-body exp)))
       (cons (make-lambda names body) values)))

; unassigned
(define (unassigned? val)
 (eq? val '*unassigned*))

; Q 4.16
;(lambda (vars)
; (let ((u '*unassigned*)
;       (v '*unassigned*))
;  (set! u <e1>)
;  (set! v <e2>)
;  (<e3>)))
;(define (scan-out-defines exp)
;  ; 内部定義のみを抽出する
;  (define (definitions exp)
;   (cond
;    ((null? exp) '())
;    ((definition? (car exp))
;     (cons (car exp) (definitions (cdr exp))))
;    (else
;     (definitions (cdr exp)))))
;
;  ; 内部定義を除いた部分を得る
;  (define (filter-definitions exp)
;   (cond
;    ((null? exp) '())
;    ((definition? (car exp))
;     (filter-definitions (cdr exp)))
;    (else
;     (cons (car exp) (filter-definitions (cdr exp))))))
;
;  ; *unassigned*
;  (define (unassignments definitions)
;    (map (lambda (definition) (list
;                               (definition-variable definition)
;                               ''*unassigned*
;                               )
;          ) definitions))
;
;  (define (assignments definitions)
;    (map (lambda (definition) (list 'set!
;                               (definition-variable definition)
;                               (definition-value definition))
;          ) definitions))
;
;  (define (make-let bindings body)
;   (append (list 'let bindings) body))
;
;  (let (
;        (definitions (definitions exp))
;        (body (filter-definitions exp))
;       )
;   (if (null? definitions)
;    ; 内部定義無しなので、そのまま返す?
;    exp
;    ; 内部定義をletに掃き出す
;    (list (make-let (unassignments definitions)
;                    (append (assignments definitions)
;                            body))))))


; 問 Q 4.20
; letrec
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-definitions exp)
 (cadr exp))
(define (letrec-parameters exp)
 (map car (cadr exp)))
(define (letrec-parameter-fns exp)
 (map cadr (cadr exp)))
(define (letrec-body exp) (cddr exp))

(define (letrec->combination exp)
  ; *unassigned*
  (define (unassignments names)
    (map (lambda (name) (list
                          name
                          ''*unassigned*
                          )
           ) names))

  ; (set! name fn) の作成
  (define (assignments definitions)
    (map (lambda (definition) (list 'set!
                                    (car definition)
                                    (cadr definition))
           ) definitions))

  (define (make-let bindings body)
    (append (list 'let bindings) body))

  (let (
        (names (letrec-parameters exp))
        (fns (letrec-parameter-fns exp))
        (body (letrec-body exp))
        )

     (make-let (unassignments names)
                     (append (assignments (letrec-definitions exp))
                             body))))

; if
(define (if? exp) (tagged-list? exp 'if))
(define (analyze-if exp)
 (let ((pproc (analyze (if-predicate exp)))
       (cproc (analyze (if-consequent exp)))
       (aproc (analyze (if-alternative exp))))
  (lambda (env)
   (if (true? (pproc env))
       (cproc env)
       (aproc env)))))

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
 (define (sequentally proc1 proc2)
  (lambda (env) (proc1 env) (proc2 env)))
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
 (let ((pproc (analyze (operator exp)))
       (aprocs (map analyze (operands exp))))
  (lambda (env)
   (execute-application (pproc env)
                        (map (lambda (aproc) (aproc env))
                         aprocs)))))

(define (execute-application proc args)
 (cond ((primitive-procedure? proc)
        (apply-primitive-procedure proc args))
       ((compound-procedure? proc)
        ((procedure-body proc)
         (extend-environment (procedure-parameters proc)
                             args
                             (procedure-environment proc))))
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
