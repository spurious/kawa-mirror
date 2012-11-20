(require <kawa.lib.prim_syntax>)

(define (vector? x) :: <boolean>
  (instance? x vector))

(define (make-vector (k :: <int>) #!optional (fill #!undefined)) :: vector
  (gnu.lists.FVector k fill))

(define (vector-length x :: <vector>) :: <int>
  (invoke x 'size))

(define (vector-set! (vector <vector>) (k <int>) obj) :: <void>
  (invoke vector 'set k obj))

(define-procedure vector-ref
  setter: vector-set!
  (begin
    (define (vector-ref (vector :: <vector>) (k :: <int>))
      (invoke vector 'get k))
    vector-ref))

(define (vector->list (vec :: <vector>)) :: <list>
  (let loop ((result :: <list> '())
	     (i :: <int> (vector-length vec)))
    (set! i (- i 1))
    (if (< i 0)
	result
	(loop (cons (vector-ref vec i) result) i))))

(define (list->vector (x :: <list>)) :: <vector>
  (gnu.lists.FVector x))

(define (vector-fill! (vec :: vector) fill
                      #!optional (start ::int 0) (end ::int (vec:size)))
  :: void
  (vec:fill fill start end))

;;; vector-map and vector-for-each are mandated by R6RS. These
;;; implementations are optimized for the one-vector case, and permit
;;; vectors of varying length, using the shortest vector as the
;;; limiting factor.
(define (vector-map (f :: procedure) (vec :: vector)
                    #!rest (vecs :: vector[]))
  :: vector
  (define (vector-map-one (f :: procedure) (vec :: vector)) :: vector
    (let* ((len :: int (vector-length vec))
           (r :: vector (make-vector len)))
      (do ((i :: int 0 (+ i 1)))
          ((= i len) r)
        (vector-set! r i (f (vector-ref vec i))))))
  (define (vector-map-generic (f :: procedure)
                              (vec :: vector)
                              #!rest (vecs :: vector[]))
    :: vector
    (let loop ((ls :: list '())
                (len :: int (vector-length vec))
                (i :: int (- vecs:length 1)))
      (if (>= i 0)
          (loop (cons (vecs i) ls)
                (min len (vector-length (vecs i)))
                (- i 1))
          (do ((r :: vector (make-vector len))
               (i :: int 0 (+ i 1)))
              ((= i len) r)
            (vector-set! r i
                         (apply f (vector-ref vec i)
                                (map (lambda (v) (vector-ref v i))
                                     ls)))))))
  (if (= 0 vecs:length)
      (vector-map-one f vec)
      (vector-map-generic f vec vecs)))

(define (vector-for-each (f :: procedure) (vec :: vector)
                         #!rest (vecs :: vector[]))
  :: void
  (define (vector-for-each-one (f :: procedure) (vec :: vector))
    :: void
    (let ((len :: int (vector-length vec)))
      (do ((i :: int 0 (+ i 1)))
          ((= i len))
        (f (vector-ref vec i)))))
  (define (vector-for-each-generic (f :: procedure)
                                   (vec :: vector)
                                   #!rest (vecs :: vector[]))
    (let loop ((ls :: list '())
               (len :: int (vector-length vec))
               (i :: int (- vecs:length 1)))
      (if (>= i 0)
          (loop (cons (vecs i) ls)
                (min len (vector-length (vecs i)))
                (- i 1))
          (do ((i :: int 0 (+ i 1)))
              ((= i len))
            (apply f (vector-ref vec i)
                   (map (lambda (v) (vector-ref v i))
                        ls))))))
  (if (= 0 vecs:length)
      (vector-for-each-one f vec)
      (vector-for-each-generic f vec vecs)))
