(require <kawa.lib.kawa.expressions>)
(import (kawa lib kawa string-cursors))
(define-alias Convert gnu.kawa.functions.Convert)
(define-alias FVector gnu.lists.FVector)

(define-simple-class ScanHelper ()
  (comp ::Compilation)
  ((init (arg::Expression))::void #!abstract)
  ((test)::Expression #!abstract)
  ((eval)::Declaration #!abstract)
  ((incr (value::Declaration))::Expression  QuoteExp:voidExp))

(define (scanner-for exp::Expression etype::Type comp::Compilation)::ScanHelper
  (let ((stype (exp:getType)))
    (cond ((stype:isSubtype gnu.kawa.lispexpr.LangObjType:listType)
           (ListScanner comp: comp))
          ((? atype::gnu.bytecode.ArrayType stype)
           (let ((etype atype:componentType))
             (ArrayScanner comp: comp elementType: etype)))
          ((stype:isSubtype (Type:make java.lang.CharSequence))
           (StringScanner comp: comp))
          ((and (stype:isSubtype (Type:make java.util.List))
                (stype:isSubtype (Type:make java.util.RandomAccess)))
           (VectorScanner comp: comp))
          ((stype:isSubtype (Type:make java.lang.Iterable))
           (IterableScanner comp: comp useGeneric: #f))
          (else
           (IterableScanner comp: comp useGeneric: #t)))))

(define-simple-class MapHelper ()
  (comp ::Compilation)
  (scanners ::ScanHelper[])
  ((makeScanner seqArg::Expression etype::Type)::ScanHelper #!abstract)
  ((initialize (exp::ApplyExp) (comp::Compilation))::void #!void)
  ((applyFunction func args)::Expression
   (apply-to-args-exp func @args))
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

(define-simple-class StringScanner (ScanHelper)
  (seqDecl ::Declaration)
  (idxDecl ::Declaration)
  (endDecl ::Declaration)
  ((init arg)
   (set! seqDecl
         (comp:letVariable #!null #!null
                           (visit-exp (apply-exp Convert:cast string arg))))
   (set! idxDecl (comp:letVariable #!null string-cursor
                                   (apply-exp as string-cursor 0)))
   (set! endDecl (comp:letVariable #!null string-cursor
                                   (apply-exp invoke seqDecl 'length))))
  ((test)
   (apply-exp string-cursor<? idxDecl endDecl))
  ((eval)
   (comp:letVariable #!null character
                     (apply-exp string-cursor-ref seqDecl idxDecl)))
  ((incr value)
   (set-exp idxDecl
            (apply-exp as string-cursor
                       (apply-exp + (apply-exp as int idxDecl)
                                  (if-exp
                                   (apply-exp > value #xFFFF)
                                   2 1))))))

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
          (comp (get-compilation)))
     (validate-generic-for-each
      exp required
      (object (MapHelper)
              ((makeScanner exp etype) (StringScanner comp: comp)))))))

(define-simple-class IterableScanner (ScanHelper)
  (useGeneric ::boolean init: #t)
  (iteratorDecl ::Declaration)
  ((init arg)
   (! seqArg (visit-exp arg))
   (cond (useGeneric
          (set! iteratorDecl
                (comp:letVariable #!null #!null
                                  (apply-exp invoke-static gnu.lists.Sequences
                                             'getIterator seqArg))))
         (else
          (! seqItArg (apply-exp as java.lang.Iterable seqArg))
          (set! iteratorDecl (comp:letVariable #!null java.util.Iterator
                                               (apply-exp invoke seqArg 'iterator)))))
   (iteratorDecl:setLocation arg))
  ((test)
   (apply-exp invoke iteratorDecl 'hasNext))
  ((eval)
   (comp:letVariable #!null #!null
                     (apply-exp invoke iteratorDecl 'next))))

(define-simple-class ListScanner (ScanHelper)
  (listDecl ::Declaration)
  (pairDecl ::Declaration)
  ((init arg)
   (! listArg (visit-exp arg))
   (set! listDecl (comp:letVariable #!null #!null listArg))
   (listDecl:setLocation arg))
  ((test)
   (apply-exp not (apply-exp eq? listDecl '())))
  ((eval)
   (define pDecl (comp:letVariable #!null #!null
                                   (apply-exp Convert:cast gnu.lists.Pair
                                              listDecl)))
   (set! pairDecl pDecl)
   (comp:letVariable #!null #!null
                     (apply-exp invoke pDecl 'getCar)))
  ((incr value)
   (set-exp listDecl
            (apply-exp invoke pairDecl 'getCdr))))

(define-simple-class ListMapHelper (MapHelper)
  (collecting ::boolean)
  (resultDecl ::Declaration)
  (lastDecl ::Declaration)
  ((makeScanner exp etype) (scanner-for exp etype comp))
  ((initialize exp comp)
   (cond (collecting
          (set! resultDecl (comp:letVariable #!null list
                                             (QuoteExp:getInstance '())))
          (set! lastDecl (comp:letVariable #!null pair QuoteExp:nullExp)))))
  ((doCollect value)
   (cond (collecting
          ;; It would be simpler to just cons up the results in reverse order,
          ;; and call reverseInPlace at the end.  However, that would be
          ;; bad for cache locality.
          (comp:letStart)
          (! pairDecl (comp:letVariable #!null pair
                                        (apply-exp make gnu.lists.Pair
                                                   value '())))
          (pairDecl:setFlag Declaration:ALLOCATE_ON_STACK)
          (! pairLastRef (ReferenceExp pairDecl))
          (pairLastRef:setFlag ReferenceExp:ALLOCATE_ON_STACK_LAST)
          (comp:letEnter)
          (comp:letDone
           (begin-exp
            (if-exp (apply-exp eq? lastDecl #!null)
                    (set-exp resultDecl pairDecl)
                    (apply-exp invoke lastDecl 'setCdr pairDecl))
            (set-exp lastDecl pairLastRef))))
         (else
          value)))
  ((collectResult result)
   (if collecting
       (begin-exp result resultDecl)
       result)))

(define-validate listForEachValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each exp required (ListMapHelper))))

; Validate plain list-only (map proc lst1 lst...) TODO
(define-validate listMapValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each exp required (ListMapHelper collecting: #t))))

(define-simple-class ArrayScanner (ScanHelper)
  (elementType ::gnu.bytecode.Type)
  (seqDecl ::Declaration)
  (idxDecl ::Declaration)
  (lenDecl ::Declaration)
  ((init arg)
   (let* ((arrayType (gnu.bytecode.ArrayType:make elementType))
          (seqArg (visit-exp (apply-exp Convert:cast arrayType arg))))
     (set! seqDecl (comp:letVariable #!null arrayType seqArg))
     (seqDecl:setLocation arg)
     (set! idxDecl (comp:letVariable #!null int (->exp 0)))
     (set! lenDecl
           (comp:letVariable #!null int (apply-exp field seqDecl 'length)))))
  ((eval)
   (comp:letVariable #!null #!null
                     (apply-to-args-exp seqDecl idxDecl)))
  ((incr value)
   (set-exp idxDecl
            (apply-exp + idxDecl 1)))
  ((test)
   (apply-exp < idxDecl lenDecl)))
  
(define-simple-class VectorScanner (ScanHelper)
  (seqDecl ::Declaration)
  (idxDecl ::Declaration)
  (endDecl ::Declaration)
  ((init arg)
   (let* ((seqArg (visit-exp (apply-exp Convert:cast java.util.List arg))))
     (set! seqDecl (comp:letVariable #!null #!null seqArg))
     (seqDecl:setLocation arg)
     (set! idxDecl (comp:letVariable #!null int (->exp 0)))
     (set! endDecl
           (comp:letVariable #!null int (apply-exp invoke seqDecl 'size)))))
  ((eval)
   (comp:letVariable #!null #!null
                     (apply-exp invoke seqDecl 'get idxDecl)))
  ((incr value)
   (set-exp idxDecl
            (apply-exp + idxDecl 1)))
  ((test)
   (apply-exp < idxDecl endDecl)))
  
;; Validate (vector-for-each proc str1 str...)
(define-validate vectorForEachValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each
    exp required
    (object (MapHelper)
            ((makeScanner exp etype) (VectorScanner comp: comp))))))

(define-validate vectorMapValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each
    exp required
    (object (MapHelper)
            (idxDecl ::Declaration)
            (resultDecl ::Declaration)
            ((makeScanner exp etype) (scanner-for exp etype comp))
            ((initialize exp comp)
             (cond ((and (= scanners:length 1)
                         (? vscanner ::VectorScanner (scanners 0)))
                    (set! idxDecl vscanner:idxDecl)
                    ;(comp:letVariable #!null int (->exp 0)))
                    (set! resultDecl
                          (comp:letVariable
                           #!null (Type:make FVector)
                           (apply-to-args-exp FVector
                                              (apply-exp as int
                                                         vscanner:endDecl)))))
                   (else
                    (set! resultDecl
                          (comp:letVariable #!null (Type:make FVector)
                                            (apply-to-args-exp FVector))))))
            ((doCollect value)
             (cond (idxDecl
                  ;  (begin-exp
                    (apply-exp invoke resultDecl 'set idxDecl value))
                   (else
                    (apply-exp invoke resultDecl 'add value))))
            ((collectResult result)
             (begin-exp result resultDecl))))))

(define (validate-generic-for-each exp::gnu.expr.ApplyExp
                                   required::gnu.bytecode.Type
                                   helper::MapHelper)
  (let ((n (- (exp:getArgCount) 1))
        (comp (get-compilation))
        (func ::gnu.expr.Expression (exp:getArg 0)))
    (comp:letStart)
    (set! helper:comp comp)
    (set! helper:scanners (ScanHelper[] length: n))
    (do ((i ::int 0 (+ i 1))) ((= i n))
      (set! (helper:scanners i)
            (helper:makeScanner (exp:getArg (+ i 1)) #!null)))
    (if (func:side_effects)
        (set! func (ReferenceExp
                    (comp:letVariable #!null #!null func))))
    (do ((i ::int 1 (+ i 1))) ((> i n))
      (let ((arg (visit-exp (exp:getArg i))))
        (exp:setArg i arg)
        ((helper:scanners (- i 1)):init arg)))
    (helper:initialize exp comp)
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
                    @(let loop-incr ((j ::int n) (chlist chlist) (incrs '()))
                       (if (= j 0) incrs
                           (let* ((j1 (- j 1))
                                  (scanner (helper:scanners j1)))
                             (loop-incr j1
                                        (cdr chlist)
                                        (cons (scanner:incr (car chlist))
                                              incrs)))))
                    (comp:loopRepeat loopLambda)))
                  (else
                   (if-exp
                    ((helper:scanners i):test)
                    (begin
                      (comp:letStart)
                      (define chValue ((helper:scanners i):eval))
                      (comp:letEnter)
                      (comp:letDone
                       (loop (+ i 1)
                             (cons chValue chlist))))))))))))))
