(module-name (kawa pprint))
(require <kawa.lib.prim_syntax>)
(require <kawa.lib.syntax>)
(import (class gnu.lists Consumer PrintConsumer)
        (class gnu.kawa.io PrettyWriter))

(define (pprint-indent mode amount::int
                      #!optional (port ::PrintConsumer (current-output-port)))
  ::void
  (port:setIndentation amount (eq? mode 'current)))

(define (pprint-newline kind
                        #!optional (port ::PrintConsumer (current-output-port)))
  ::void
  
  (port:writeBreak
   (case kind
     ((fill) PrettyWriter:NEWLINE_FILL)
     ((linear) PrettyWriter:NEWLINE_LINEAR)
     ((miser) PrettyWriter:NEWLINE_MISER)
     ((mandatory) PrettyWriter:NEWLINE_LITERAL)
     (else (error "unknown newline kind")))))

(define (pprint-start-logical-block prefix::String per-line::boolean
                                    suffix::String
                                    #!optional (out ::Consumer (current-output-port)))
  (PrintConsumer:startLogicalBlock prefix per-line suffix out))
  

(define (pprint-end-logical-block suffix::String
                                  #!optional (out ::Consumer (current-output-port)))
  (PrintConsumer:endLogicalBlock suffix out))

(define-syntax-case pprint-logical-block ()
  ((_ . body)
   (begin
     (define prefix-expr #!null)
     (define per-line-prefix-expr #!null)
     (define suffix-expr #!null)
     (define port-expr #!null)
     (let loop ((rest #'body))
       (syntax-case rest ()
         ((prefix: sexp . r)
          (begin
            (if (or prefix-expr per-line-prefix-expr)
                (report-syntax-error rest "prefix keyword duplicates previous prefix or per-line-prefix"))
            (set! prefix-expr #'sexp)
            (loop #'r)))
         ((per-line-prefix: sexp . r)
          (begin
            (if (or prefix-expr per-line-prefix-expr)
                (report-syntax-error rest "per-line-prefix keyword duplicates previous prefix or per-line-prefix"))
            (set! per-line-prefix-expr #'sexp)
            (loop #'r)))
         ((suffix: sexp . r)
          (begin
            (set! suffix-expr #'sexp)
            (loop #'r)))
         ((out: sexp . r)
          (begin
            (set! out-expr #'sexp)
            (loop #'r)))
         (()
          (report-syntax-error #'body "missing body"))
         (r
          (begin
            (if (keyword? (car #'r))
                (report-syntax-error #'r "unknown keyword " (car #'r)))
            #`(let ((oport #,(if out-expr out-expr #`(current-output-port))))
                (try-finally
                 (begin
                   (pprint-start-logical-block
                    #,(cond (prefix-expr prefix-expr)
                            (per-line-prefix-expr per-line-prefix-expr)
                            (else ""))
                    #,(if per-line-prefix-expr #t #f)
                    #,(if suffix-expr suffix-expr "")
                    oport)
                   . r)
                 (pprint-end-logical-block
                  #,(if suffix-expr suffix-expr "") oport))))))))))

(define (pprint-tail body newline-kind out ::PrintConsumer) ::void
  (let loop ((rest body))
    (cond ((pair? rest)
           (display " " out)
           (pprint-newline newline-kind out)
           (pprint (car rest) out)
           (loop (cdr rest)))
          ((not (null? rest))
           (display " . " out)
           (pprint rest out)))))

(define (pprint-body body out ::PrintConsumer) ::void
  (pprint-tail body 'linear out))

   (define (pprint-define obj out ::PrintConsumer) ::void
  (pprint-logical-block
   prefix: "(" suffix: ")" out: out
   (display (car obj) out)
   (display " " out)
   (pprint-newline 'miser out)
   (if (not (null? (cdr obj)))
       (let* ((rest1 (cdr obj))
              (args (car rest1))
              (body (cdr rest1)))
         (display args out)
         ;; FIXME should check for type-specifier
         (pprint-indent 'block 1 out)
         (pprint-body body out)))))

(define (pprint-let obj out ::PrintConsumer) ::void
  (pprint-logical-block
   prefix: "(" suffix: ")" out: out
   (display (car obj) out)
   (display " " out)
   ;;(pprint-newline 'miser out)
   (define rest (cdr obj))
   (cond (and (pair? rest) (symbol? (car rest)))
         (display (car rest) out)
         (display " " out)
         (pprint-newline 'fill out)
         (set! rest (cdr rest)))
   (cond ((and (pair? rest) (list? (car rest)))
          (pprint-logical-block
           prefix: "(" suffix: ")" out: out
           (cond ((pair? (car rest))
                  (let largs ((args (car rest)))
                    (pprint (car args) out) ;; not quite right but close
                    (cond ((pair? (cdr args))
                           (display " " out)
                           (pprint-newline 'linear out)
                           (largs (cdr args))))))))
          (pprint-indent 'block 1 out)
          (pprint-body (cdr rest) out))
         (else
          (pprint rest out)))))

(define (pprint-if obj out)
  (pprint-logical-block
   prefix: "(" suffix: ")" out: out
   (display (car obj) out)
   (display " " out)
   (cond ((pair? (cdr obj))
          (pprint-indent 'current 0 out)
          (pprint (cadr obj) out)
          (pprint-tail (cddr obj) 'linear out)))))

(define (pprint-call obj out)
  (pprint-logical-block
   prefix: "(" suffix: ")" out: out
   (pprint (car obj) out)
   (pprint-tail (cdr obj) 'fill out)))

(define (pprint obj #!optional (out ::PrintConsumer (current-output-port)))
  (if (pair? obj)
      (let ((fun (car obj)))
        ;; FIXME should use a dynamic dispatch table
        (case fun
          ((define define-private lambda)
           (pprint-define obj out))
          ((cond if)
           (pprint-if obj out))
          ((let)
           (pprint-let obj out))
          (else
           (pprint-call obj out))))
      (display obj out)))
