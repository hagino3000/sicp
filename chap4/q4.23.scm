; 初期版
; 解析時に手続のリストを走査する
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

; Alyssa
; 実行時に手続のリストを走査する
(define (analyze-sequence exps)
 (define (execute-sequence procs env)
  (cond ((null? (cdr procs)) ((car procs) env))
        (else ((car procs) env)
              (execute-sequence (cdr procs) env))))
 (let ((procs (map analyze exps)))
  (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
  (lambda (env) (execute-sequence procs env))))
