; https://github.com/monmon/sicp/blob/master/s5/5.5/compile.scm
; =======================================================================
; p.340
; =======================================================================

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))
(define (empty-instruction-seque)
  (make-instruction-sequence '() '() '()))

; =======================================================================
; p.343 $B@\B3%3!<%I$NK]Lu(B
; =======================================================================

; $B=PNO(B
; return $B$J$i(B continue $B%l%8%9%?$r;H$&(B
; next $B$J$iDI2C$NL?Na$O2?$b$J$7(B
; $BB>$O%l%8%9%?$rI,MW$H$;$:!"$=$N%i%Y%k$X$N(B goto $BL?Na$r@8@.$9$k(B
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-seque))
        (else
          (make-instruction-sequence '() '()
                                     `((goto (label ,linkage)))))))

; return $B$O(B continue $B%l%8%9%?$r;H$&$N$G(B preserving $B$7$FL?NaNs$KO"7k(B
; p.342 $B$N(B (preserving (list <reg1> <reg2>) <seq1> <seq2>) $B$NOC(B
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

; =======================================================================
; p.344 $BC1=c<0$NK]Lu(B -- $B<+8JI>2A<0(B $B%/%)!<%H<0(B $BJQ?t(B
; =======================================================================

; $BMW5a$5$l$?CM$r(B target $B%l%8%9%?$XBeF~$7!"(B linkage $B$N;XDjDL$j$K?J$`L?Na$r9=@.$9$k(B

; $B<+8JI>2A<0(B
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,exp))))))

; $B%/%)!<%H<0(B
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,(text-of-quotation exp)))))))

; $BJQ?t(B
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '(env) (list target)
                                               `((assign ,target
                                                         (op lookup-variable-value)
                                                         (const ,exp)
                                                         (reg env))))))

; =======================================================================
; p.344 $BC1=c<0$NK]Lu(B -- assignment $B$H(B define
; =======================================================================

; $BJQ?t$K3d$jEv$F$i$l$?CM$r:F5"E*$K@8@.(B
; $B<B:]$KJQ?t$N@_Dj!J(B or $BDj5A!K$7$?$b$N!"$H!"<0A4BN$NCM!J(B 'ok $B!K$r(B target $B$K3d$jEv$F$l$k$b$N!"$N(B2$BL?NaNs$rO"7k$9$k(B
; $BO"7k$7$?(B2$BL?NaNs$O(B env $B$H(B val $B$rI,MW$H$7$F$$$F!"(B target $B$r=$@5$9$k(B

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op set-variable-value!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))

; =======================================================================
; p.345 $B>r7o<0$NK]Lu(B
; =======================================================================

; $B%F%9%H$,56$J$i??$N(B branch $B$rHt$S1[$5$J$1$l$P$J$i$J$$(B
; env $B$O??$+56$N(B branch $B$GI,MW$+$b$7$l$J$$$N$G(B predicate $B$GJ]B8!"(B continue $B$bF1MM$KJ]B8(B
; $B!JC`<!E*$K$O<B9T$7$J$$!K??$H56$N(B branch $B$O(B 5.5.4 $B@a$G=R$Y$kFCJL$J<jB3$-(B parallel-instruction-sequences $B$GO"7k(B

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
              (compile
                (if-consequent exp) target consequent-linkage))
            (a-code
              (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                      (make-instruction-sequence '(val) '()
                                                 `((test (op false?) (reg val))
                                                   (branch (label ,f-branch))))
                      (parallel-instruction-sequences
                        (append-instruction-sequences t-branch c-code)
                        (append-instruction-sequences f-branch a-code))
                      after-if))))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

; =======================================================================
; p.346 $BJB$S$NK]Lu(B
; =======================================================================

; $B:G8e$N<0$OJB$S$,;XDj$5$l$?(B linkage $B$r$b$C$FK]Lu$9$k(B
; $B$=$NB>$N<0$O!JJB$S$N;D$j$r<B9T$9$k$?$a!K(B next $B$N@\B3$r$b$C$FK]Lu$9$k(B
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
    (compile (first-exp seq) target linkage)
    (preserving '(env continue)
                (compile (first-exp seq) target 'next)
                (compile-sequence (rest-exps seq) target linkage))))

; =======================================================================
; p.346 lambda $B<0$NK]Lu(B
; =======================================================================

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
            (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
                            (make-instruction-sequence '(env) (list target)
                                                       `((assign ,target
                                                                 (op make-compiled-procedure)
                                                                 (label ,proc-entry)
                                                                 (reg env)))))
          (compile-lambda-body exp proc-entry))
        after-lambda))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
                                 `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
      (compile-sequence (lambda-body exp) 'val 'return))))

; =======================================================================
; p.347 $BAH9g$;$NK]Lu(B
; =======================================================================

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
               (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
                                 '((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
          code-to-get-last-arg
          (preserving '(env)
                      code-to-get-last-arg
                      (code-to-get-rest-args
                        (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 '((assign argl
                                                           (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env)
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes))))))

; =======================================================================
; p.349 $B<jB3$-$N:nMQ(B
; =======================================================================

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
            (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
                                   `((test (op primitive-procedure?) (reg proc))
                                     (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
                              (make-instruction-sequence '(proc argl)
                                                         (list target)
                                                         `((assign ,target
                                                                   (op apply-primitive-procedure)
                                                                   (reg proc)
                                                                   (reg argl)))))))
        after-call))))

; =======================================================================
; p.350 $BK]Lu$7$?<jB3$-$N:nMQ(B
; =======================================================================

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
                                    `((assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(define all-regs '(env proc val argl continue))

; =======================================================================
; p.351 $BL?NaNs$NAH9g$;(B
; =======================================================================

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))
(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
      (empty-instruction-seque)
      (append-2-sequences (car seqs)
                          (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequences seq1 seq2)
    (let ((first-reg (car regs)))
      (if (and (needs-register? seq2 first-reg)
               (modifies-register? seq1 first-reg))
        (preserving (cdr regs)
                    (make-instruction-sequence
                      (list-union (list first-reg)
                                  (registers-needed seq1))
                      (list-difference (registers-modified seq1)
                                       (list first-reg))
                      (append `((save ,first-reg))
                              (statements seq1)
                              `((restore ,first-reg))))
                    seq2)
        (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))
