(require <kawa.lib.characters>)
(require <kawa.lib.kawa.expressions>)

(define-validate pipeProcessValidateApply (exp required proc)
  ((= exp:arg-count 2)
   (exp:visitArgs (get-visitor))
   (let ((e0 (exp:getArg 0))
         (e1 (exp:getArg 1))
         (visitor (get-visitor)))
     (if (and (gnu.expr.ApplyExp? e1)
              (eq? ((->gnu.expr.ApplyExp e1):function:valueIfConstant)
                   gnu.kawa.functions.RunProcess:instance))
         (let* ((ae1 ::gnu.expr.ApplyExp e1)
                (aeargs ae1:args))
           (visit-exp
            (apply-exp ae1:function in: e0 @aeargs)
            require))
         (visitor:error #\e "pipe-process arg not run-process" e1)))))

(define-validate charToIntegerValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (let ((e0 (visit-exp (exp:getArg 0) character-or-eof)))
     (visit-exp
      (apply-exp as int
                 (apply-exp gnu.kawa.functions.Convert:cast character e0))
      required))))

(define-validate integerToCharValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (let ((e0 (visitor:visit (exp:getArg 0) int)))
     (visit-exp
      (apply-exp as character
                 (apply-exp gnu.kawa.functions.Convert:cast int e0))
      required))))

(define-validate isEofValidateApply (exp required proc)
  ((exp:isSimple 1 1)
   (exp:visitArgs visitor)
   (let* ((e0 (exp:getArg 0))
          (t0 (e0:getType)))
     (cond ((or (eq? t0 character) (eq? t0 character-or-eof))
            (visit-exp
             (apply-exp = (apply-exp as int e0) -1)
             required))
           ((gnu.kawa.reflect.LazyType:maybeLazy t0)
            (visit-exp
             (apply-exp eq? (apply-exp gnu.mapping.Promise:force e0) #!eof)
             required))
           (else exp)))))

(define-validate charCompareValidateApply (exp required proc)
  ((exp:isSimple)
   (define name proc:name)
   (define n exp:arg-count)
   (define num-op
     (cond ((or (name:equals "char=?") (name:equals "char-ci=?")) =)
           ((or (name:equals "char<?") (name:equals "char-ci<?")) <)
           ((or (name:equals "char>?") (name:equals "char-ci>?")) >)
           ((or (name:equals "char<=?") (name:equals "char-ci<=?")) <=)
           ((or (name:equals "char>=?") (name:equals "char-ci>=?")) >=)
           (else #!null)))
   (cond ((eq? num-op #!null)
          exp)
         (else
          (define ci (> (name:indexOf "ci") 0))
          (do ((i::int 0 (+ i 1)))
              ((= i n))
            (let ((e (apply-exp char->integer (exp:getArg i))))
              (if ci
                  (set! e (apply-exp invoke-static java.lang.Character 'toUpperCase e)))
              (exp:setArg i e)))
          (visit-exp (apply-exp num-op @exp:args) required)))))
