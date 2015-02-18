(require <kawa.lib.kawa.expressions>)
(import (kawa lib kawa string-cursors))
(define-alias Convert gnu.kawa.functions.Convert)

(define-simple-class MapHelper ()
  (comp ::Compilation)
  ((initialize (exp::ApplyExp) (comp::Compilation))::void
   (set! (this):comp comp))
  ;; initialize for the i'th argument
  ((initEach (i::int) (arg::Expression))::void #!abstract)
  ;; emit test for if the i'th sequence has more values
  ((testEach (i::int))::Expression #!abstract)
  ((evalEach (i::int))::Declaration #!abstract)
  ((incrEach (value::Declaration) (i::int))::Expression  #!abstract)
  ;; may not need if never overridden
  ((applyFunction func args)::Expression
   (let* ((fexp (->exp func))
          (applyFunction (comp:applyFunction fexp)))
     (if (eq? applyFunction #!null)
         (apply-exp fexp @args)
         (apply-exp applyFunction fexp @args))))
  ((doCollect result::Expression)::Expression
   result)
  ((collectResult result::Expression)::Expression
   result))

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
     (validate-generic-for-each
      exp required
      (object (MapHelper)
              ((initEach i arg)
               (set! (seqDecls i)
                     (comp:letVariable #!null #!null
                                       (visit-exp (apply-exp Convert:cast string arg))))
               (set! (idxDecls i) (comp:letVariable #!null string-cursor
                                                    (apply-exp as string-cursor 0)))
               (set! (endDecls i) (comp:letVariable #!null string-cursor
                                                    (apply-exp invoke (seqDecls i) 'length))))
              ((testEach i)
               (apply-exp string-cursor<? (idxDecls i) (endDecls i)))
              ((evalEach i)
               (comp:letVariable #!null character
                                 (apply-exp string-cursor-ref
                                            (seqDecls i) (idxDecls i))))
              ((incrEach value i)
               (set-exp (idxDecls i)
                        (apply-exp as string-cursor
                                   (apply-exp + (apply-exp as int (idxDecls i))
                                              (if-exp
                                               (apply-exp > value #xFFFF)
                                               2 1))))))))))

(define-simple-class ListMapHelper (MapHelper)
  (collecting ::boolean)
  (listDecls ::Declaration[])
  (pairDecls ::Declaration[])
  (resultDecl ::Declaration)
  ((initialize exp comp)
   (invoke-special MapHelper (this) 'initialize exp comp)
   (! n (- (exp:getArgCount) 1))
   (set! listDecls (Declaration[] length: n))
   (set! pairDecls (Declaration[] length: n))
   (if collecting
       (set! resultDecl (comp:letVariable #!null list
                                          (QuoteExp:getInstance '())))))
  ((initEach i arg)
   (let* ((listArg (visit-exp arg))
          (listDecl (comp:letVariable #!null #!null listArg)))
     (listDecl:setLocation arg)
     (set! (listDecls i) listDecl)))
  ((testEach i)
   (apply-exp not (apply-exp eq? (listDecls i) '())))
  ((evalEach i)
   (define pairDecl (comp:letVariable #!null #!null
                                      (apply-exp Convert:cast gnu.lists.Pair
                                                 (listDecls i))))
   (set! (pairDecls i) pairDecl)
   (comp:letVariable #!null #!null
                     (apply-exp invoke pairDecl 'getCar)))
  ((incrEach value i)
   (set-exp (listDecls i)
            (apply-exp invoke (pairDecls i) 'getCdr)))
  ((doCollect value)
   (if collecting
       (set-exp resultDecl
                (apply-exp make gnu.lists.Pair value resultDecl))
       value))
  ((collectResult result)
   (if collecting
       (begin-exp result
                   (apply-exp gnu.lists.LList:reverseInPlace resultDecl))
       result)))

(define-validate listForEachValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each exp required (ListMapHelper))))

; Validate plain list-only (map proc lst1 lst...) TODO
(define-validate listMapValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each exp required (ListMapHelper collecting: #t))))

(define-simple-class VectorMapHelper (MapHelper)
  (collecting ::boolean)
  (seqDecls ::Declaration[])
  (idxDecls ::Declaration[])
  (endDecls ::Declaration[])
  ((initialize exp comp)
   (invoke-special MapHelper (this) 'initialize exp comp)
   (! n (- (exp:getArgCount) 1))
   (set! seqDecls (Declaration[] length: n))
   (set! idxDecls (Declaration[] length: n))
   (set! endDecls (Declaration[] length: n)))
  ((initEach i arg)
   (let* ((seqArg (visit-exp (apply-exp Convert:cast java.util.List arg)))
          (seqDecl (comp:letVariable #!null #!null seqArg)))
     (seqDecl:setLocation arg)
     (set! (seqDecls i) seqDecl)
     (set! (idxDecls i)
           (comp:letVariable #!null int (->exp 0)))
     (set! (endDecls i)
           (comp:letVariable #!null int (apply-exp invoke seqDecl 'size)))))
  ((evalEach i)
   (comp:letVariable #!null #!null
                     (apply-exp invoke (seqDecls i) 'get (idxDecls i))))
  ((incrEach value i)
   (set-exp (idxDecls i)
            (apply-exp + (idxDecls i) 1)))
  ((testEach i)
   (apply-exp < (idxDecls i) (endDecls i))))

;; Validate (vector-for-each proc str1 str...)
(define-validate vectorForEachValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each  exp required (VectorMapHelper))))

(define (validate-generic-for-each exp::gnu.expr.ApplyExp
                                   required::gnu.bytecode.Type
                                   helper::MapHelper)
   (let ((n (- (exp:getArgCount) 1))
         (comp (get-compilation))
         (func ::gnu.expr.Expression (exp:getArg 0)))
     (comp:letStart)
     (helper:initialize exp comp)
     (if (func:side_effects)
         (set! func (gnu.expr.ReferenceExp
                     (comp:letVariable #!null #!null func))))
     (do ((i ::int 0 (+ i 1))) ((= i n))
       (helper:initEach i (exp:getArg (+ i 1))))
     (comp:letEnter)
     (comp:letDone
      (let* ((loopLambda (comp:loopStart)))
        (comp:loopEnter)
        (helper:collectResult
         (comp:loopDone
          (let loop ((i ::int 0) (chlist '()))
            (cond ((= i n)
                   (begin-exp
                    (helper:doCollect
                     (helper:applyFunction func (reverse chlist)))
                    (comp:loopRepeat loopLambda)))
                  (else
                   (if-exp
                    (helper:testEach i)
                    (begin
                      (comp:letStart)
                      (define chValue (helper:evalEach i))
                      (comp:letEnter)
                      (comp:letDone
                       (begin-exp
                        (helper:incrEach chValue i)
                        (loop (+ i 1)
                              (cons chValue chlist)))))))))))))))
