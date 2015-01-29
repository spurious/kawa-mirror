(require <kawa.lib.kawa.expressions>)
(import (kawa lib kawa string-cursors))
(define-alias Convert gnu.kawa.functions.Convert)

;; Optimize (string-cursor-for-each proc str [start [end]])
(define-validate stringCursorForEachValidateApply (exp required proc)
  ((exp:isSimple 2 4)
   (let ((comp (get-compilation))
         (func (exp:getArg 0)))
     (comp:letStart)
     (define seqDecl
       (comp:letVariable #!null string (exp:getArg 1)))
     (define idxDecl
       (comp:letVariable #!null string-cursor
                         (if (> exp:arg-count 2) (exp:getArg 2)
                             (apply-exp as string-cursor 0))))
     (define endDecl
       (comp:letVariable #!null string-cursor
                         (if (> exp:arg-count 3) (exp:getArg 3)
                             (apply-exp string-cursor-end seqDecl))))
     (comp:letEnter)
     (comp:letDone
      (let ((loopLambda (comp:loopStart)))
        (comp:loopEnter)
        (comp:loopDone
         (if-exp (apply-exp string-cursor<? idxDecl endDecl)
                 (begin
                   (comp:letStart)
                   (define chDecl (comp:letVariable #!null character
                                                    (apply-exp
                                                     string-cursor-ref
                                                     seqDecl idxDecl)))
                   (comp:letEnter)
                   (comp:letDone
                    (begin-exp
                     (apply-exp func chDecl)
                     (set-exp idxDecl
                              (apply-exp as string-cursor 
                                         (apply-exp + (apply-exp as int idxDecl)
                                                    (if-exp (apply-exp > (apply-exp as int chDecl) #xFFFF)
                                                            2 1))))
                     (comp:loopRepeat loopLambda)))))))))))

;; Optimize SRFI-13-style (string-for-each proc str [start [end]])
(define-validate stringForEach1ValidateApply (exp required proc)
  ((exp:isSimple 2 2)
   (apply-exp string-cursor-for-each (exp:getArg 0) (exp:getArg 1)))
  ((exp:isSimple 3 4)
   (define comp (get-compilation))
   (comp:letStart)
   (define decl1 (comp:letVariable #!null string (exp:getArg 1)))
   (define decl2 (comp:letVariable #!null int (exp:getArg 2)))
   (comp:letEnter)
   (comp:letDone
    (apply-exp string-cursor-for-each
               (exp:getArg 0) decl1
               (apply-exp string-cursor-next decl1
                          (apply-exp as string-cursor 0)
                          decl2)
               (if (< exp:arg-count 4)
                   (apply-exp string-cursor-end decl1)
                   (apply-exp string-cursor-next decl1
                              (apply-exp as string-cursor 0)
                              (exp:getArg 3)))))))
                   
;; Validate (string-for-each proc str1 [str... | start [end]]
(define-validate stringForEachValidateApply (exp required proc)
  ;; check-for SRFI-13-style (string-for-each proc str start [end])
  ((and (exp:isSimple 3 4)
        (let ((e2 (visit-exp (exp:getArg 2))))
          (exp:setArg 2 e2)
          (let* ((t2 (e2:getType))
                 (integer-compat (invoke integer 'isCompatibleWithValue t2)))
            (or (> integer-compat 0)
                (and (>= integer-compat 0)
                     (< (invoke string 'isCompatibleWithValue t2) 0))))))
   (apply-exp srfi-13-string-for-each (exp:getArg 0) (exp:getArg 1)
              (exp:getArg 2)
              @(if (= exp:arg-count 4) [(exp:getArg 3)] [])))
  ;; R7RS-style (string-for-each proc str1 str...)
  ((and (exp:isSimple 2)
        (or (= exp:arg-count 2)
            (> exp:arg-count 4)
            (let ((e2 (visit-exp (exp:getArg 2))))
              (exp:setArg 2 e2)
              (let* ((t2 (e2:getType))
                     (string-compat (invoke string 'isCompatibleWithValue t2)))
                (or (> string-compat 0)
                    (and (>= string-compat 0)
                         (< (invoke integer 'isCompatibleWithValue t2) 0)))))))
   (let* ((n (- (exp:getArgCount) 1))
          (seqDecls (gnu.expr.Declaration[] length: n))
          (idxDecls (gnu.expr.Declaration[] length: n))
          (endDecls (gnu.expr.Declaration[] length: n))
          (comp (get-compilation)))
     (define (init-each i arg)
       (set! (seqDecls i)
             (comp:letVariable #!null #!null
                               (visit-exp (apply-exp Convert:cast string arg))))
       (set! (idxDecls i) (comp:letVariable #!null string-cursor
                                            (apply-exp as string-cursor 0)))
       (set! (endDecls i) (comp:letVariable #!null string-cursor
                                         (apply-exp invoke (seqDecls i) 'length))))
     (define (test-each i)
       (apply-exp string-cursor<? (idxDecls i) (endDecls i)))
     (define (eval-each i)
       (comp:letVariable #!null character
                         (apply-exp string-cursor-ref
                                    (seqDecls i) (idxDecls i))))
     (define (incr-each value i)
       (set-exp (idxDecls i)
                (apply-exp as string-cursor
                           (apply-exp + (apply-exp as int (idxDecls i))
                                      (if-exp
                                       (apply-exp > value #xFFFF)
                                       2 1)))))
     (validate-generic-for-each exp required
                                init-each test-each eval-each incr-each))))

; Validate (vector-for-each proc str1 str...)
(define-validate vectorForEachValidateApply (exp required proc)
  ((exp:isSimple 2)
   (let* ((n (- (exp:getArgCount) 1))
          (seqDecls (gnu.expr.Declaration[] length: n))
          (idxDecls (gnu.expr.Declaration[] length: n))
          (endDecls (gnu.expr.Declaration[] length: n))
          (comp (get-compilation)))
     (define (init-each i arg)
       (let* ((seqArg (visit-exp (apply-exp Convert:cast java.util.List arg)))
              (seqDecl (comp:letVariable #!null #!null seqArg)))
         (seqDecl:setLocation arg)
         (set! (seqDecls i) seqDecl)
         (set! (idxDecls i)
               (comp:letVariable #!null int (->exp 0)))
         (set! (endDecls i)
               (comp:letVariable #!null int (apply-exp invoke seqDecl 'size)))))
     (define (test-each i)
       (apply-exp < (idxDecls i) (endDecls i)))
     (define (eval-each i)
       (comp:letVariable #!null #!null
                         (apply-exp invoke (seqDecls i) 'get (idxDecls i))))
     (define (incr-each value i)
       (set-exp (idxDecls i)
                (apply-exp + (idxDecls i) 1)))
     (validate-generic-for-each exp required
                                init-each test-each eval-each incr-each))))

(define (validate-generic-for-each exp::gnu.expr.ApplyExp
                                   required::gnu.bytecode.Type
                                   init-each::procedure
                                   test-each::procedure
                                   eval-each::procedure
                                   incr-each::procedure)
   (let ((n (- (exp:getArgCount) 1))
         (comp (get-compilation))
         (func (exp:getArg 0)))
     (comp:letStart)
     (define decls (gnu.expr.Declaration[][] length: n))
     (do ((i ::int 0 (+ i 1))) ((= i n))
       (init-each i (exp:getArg (+ i 1))))
     (comp:letEnter)
     (comp:letDone
      (let* ((loopLambda (comp:loopStart)))
        (comp:loopEnter)
        (comp:loopDone
         (let loop ((i ::int 0) (chlist '()))
           (cond ((= i n)
                  (begin-exp
                   (apply-exp func @(reverse chlist))
                   (comp:loopRepeat loopLambda)))
                 (else
                  (if-exp
                   (test-each i)
                   (begin
                    (comp:letStart)
                    (define chValue (eval-each i))
                    (comp:letEnter)
                    (comp:letDone
                     (begin-exp
                      (incr-each chValue i)
                      (loop (+ i 1)
                            (cons chValue chlist))))))))))))))
