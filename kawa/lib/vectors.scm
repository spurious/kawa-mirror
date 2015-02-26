(require <kawa.lib.prim_syntax>)

(define (vector? x) :: <boolean>
  (instance? x vector))

(define (make-vector (k :: <int>) #!optional (fill #!undefined)) :: vector
  (gnu.lists.FVector k fill))

(define (vector-length x :: <vector>) :: <int>
  (invoke x 'size))

(define (vector-set! (vector ::vector) (k ::int) obj) :: <void>
  (invoke vector 'set k obj))

(define-procedure vector-ref
  setter: vector-set!
  (begin
    (define (vector-ref (vector :: <vector>) (k :: <int>))
      (invoke vector 'get k))
    vector-ref))

(define (vector->list (vec :: <vector>)
                      #!optional (start ::int 0) (end ::int (vec:size)))
                      :: <list>
  (let loop ((result :: <list> '())
	     (i ::int end))
    (set! i (- i 1))
    (if (< i start)
	result
	(loop (cons (vector-ref vec i) result) i))))

(define (list->vector (x :: <list>)) :: <vector>
  (gnu.lists.FVector x))

(define (vector->string (vec ::vector)
                        #!optional (start ::int 0) (end ::int (vec:size)))
  ::string
  (let loop ((result ::java.lang.StringBuilder (java.lang.StringBuilder))
	     (i ::int start))
    (if (>= i end)
	(gnu.lists.FString result)
        (let ((ch (vector-ref vec i)))
          (if (java.lang.Character? ch)
              (result:append ((as java.lang.Character ch):charValue))
              (gnu.text.Char:print ((as gnu.text.Char ch):intValue) result))
          (loop result (+ i 1))))))

(define (string->vector (str ::string)
                        #!optional (start ::int 0) (end ::int (str:length)))
  ::vector
  (let loop ((result ::Object[] (Object[] length: (- end start)))
	     (i ::int start)
             (j ::int 0))
    (if (>= i end)
	(gnu.lists.FVector result)
        ;; FIXME not handling surrogates.
        (let ((ch ::char (str:charAt i)))
          (set! (result j) ch)
          (loop result (+ i 1) (+ j 1))))))

(define (vector-copy (vec :: vector)
                     #!optional
                     (start ::int 0)
                     (end ::int (vec:size)))
  ::vector
  (let ((result (gnu.lists.FVector (- end start))))
    (result:copyFrom 0 vec start end)
    result))

(define (vector-copy! (to ::vector)
                      (at ::int)
                      (from ::vector)
                      #!optional
                      (start ::int 0)
                      (end ::int (from:size)))
  (to:copyFrom  at from start end))

(define (vector-fill! (vec :: vector) fill
                      #!optional (start ::int 0) (end ::int (vec:size)))
  :: void
  (vec:fill fill start end))

;;; vector-map and vector-for-each are mandated by R6RS. These
;;; implementations are optimized for the one-vector case, and permit
;;; vectors of varying length, using the shortest vector as the
;;; limiting factor.
(define (vector-map (proc :: procedure) vec1 #!rest (vecs :: object[]))
  :: vector
  validate-apply: "kawa.lib.compile_map:vectorMapValidateApply"
  (define (vector-map-simple proc::procedure vec::java.util.List) :: vector
    (let* ((len :: int (vec:size))
           (r :: vector (make-vector len)))
      (do ((i :: int 0 (+ i 1)))
          ((= i len) r)
        (vector-set! r i (proc (vec:get i))))))
  (define (vector-map-one proc vec) :: vector
    (if (and (? fp::procedure proc) (? vlist::gnu.lists.SimpleVector vec))
        (vector-map-simple fp vlist)
        (let* ((r :: vector (make-vector 0))
               (it (gnu.lists.Sequences:getIterator vec)))
          (do ((i :: int 0 (+ i 1))) ((not (it:hasNext)) r)
            (vector-set! r i (proc (it:next)))))))

  (if (= 0 vecs:length)
      (vector-map-one proc vec1)
      (let* ((nargs (+ vecs:length 1))
             (iterators (java.util.Iterator[] length: nargs))
             (elements (object[] length: nargs))
             (result (make-vector 0)))
        (do ((i ::int 0 (+ i 1))) ((= i nargs))
          (set! (iterators i)
                (gnu.lists.Sequences:getIterator
                 (if (= i 0) vec1 (vecs (- i 1))))))
        (let loop1 ()
          (let loop2 ((i ::int 0))
            (cond ((= i nargs)
                   (result:add (proc @elements))
                   (loop1))
                  (((iterators i):hasNext)
                   (set! (elements i) ((iterators i):next))
                   (loop2 (+ i 1)))
                  (else
                   result)))))))

(define (vector-for-each (f :: procedure) (vec :: java.util.List)
                         #!rest (vecs :: java.util.List[]))
  :: void
  validate-apply: "kawa.lib.compile_map:vectorForEachValidateApply"
  (define (vector-for-each-one (f :: procedure) (vec :: java.util.List))
    :: void
    (let ((len :: int (vec:size)))
      (do ((i :: int 0 (+ i 1)))
          ((= i len))
        (f (vec:get i)))))
  (define (vector-for-each-generic (f :: procedure)
                                   (vec :: java.util.List)
                                   #!rest (vecs :: java.util.List[]))
    (let loop ((ls :: list '())
               (len :: int (vec:size))
               (i :: int (- vecs:length 1)))
      (if (>= i 0)
          (loop (cons (vecs i) ls)
                (min len (vector-length (vecs i)))
                (- i 1))
          (do ((i :: int 0 (+ i 1)))
              ((= i len))
            (apply f (vec:get i)
                   (map (lambda (v::java.util.List) (v:get i))
                        ls))))))
  (if (= 0 vecs:length)
      (vector-for-each-one f vec)
      (vector-for-each-generic f vec vecs)))
