(require <kawa.lib.kawa.expressions>)
(import (kawa lib kawa string-cursors))
(define-alias Convert gnu.kawa.functions.Convert)

(define-simple-class ScanHelper ()
  (comp ::Compilation)
  ((init (arg::Expression))::void #!abstract)
  ((test)::Expression #!abstract)
  ((eval)::Declaration #!abstract)
  ((incr (value::Declaration))::Expression  #!abstract))

(define-simple-class MapHelper ()
  (comp ::Compilation)
  (scanners ::ScanHelper[])
  ((initialize (exp::ApplyExp) (comp::Compilation))::void
   (! n (- (exp:getArgCount) 1))
   (set! scanners (ScanHelper[] length: n))
   (set! (this):comp comp))
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
              ((initialize (exp::ApplyExp) (comp::Compilation))::void
               (invoke-special MapHelper (this) 'initialize exp comp)
               (do ((i ::int 0 (+ i 1))) ((= i n))
                 (set! (scanners i) (StringScanner comp: comp)))))))))

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
  ((initialize exp comp)
   (invoke-special MapHelper (this) 'initialize exp comp)
   (! n (- (exp:getArgCount) 1))
   (do ((i ::int 0 (+ i 1))) ((= i n))
     (set! (scanners i) (ListScanner comp: comp)))
   (if collecting
       (set! resultDecl (comp:letVariable #!null list
                                          (QuoteExp:getInstance '())))))
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
  
(define-simple-class VectorMapHelper (MapHelper)
  (collecting ::boolean)
  ((initialize exp comp)
   (invoke-special MapHelper (this) 'initialize exp comp)
   (! n (- (exp:getArgCount) 1))
   (do ((i ::int 0 (+ i 1))) ((= i n))
     (set! (scanners i) (VectorScanner comp: comp)))))

;; Validate (vector-for-each proc str1 str...)
(define-validate vectorForEachValidateApply (exp required proc)
  ((exp:isSimple 2)
   (validate-generic-for-each exp required (VectorMapHelper))))

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
       ((helper:scanners i):init (exp:getArg (+ i 1))))
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
                    ((helper:scanners i):test)
                    (begin
                      (comp:letStart)
                      (define chValue ((helper:scanners i):eval))
                      (comp:letEnter)
                      (comp:letDone
                       (begin-exp
                        ((helper:scanners i):incr chValue)
                        (loop (+ i 1)
                              (cons chValue chlist)))))))))))))))
