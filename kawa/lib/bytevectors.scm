(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)

(define (make-bytevector (n ::int) #!optional (init ::int 0)) :: bytevector
  (make bytevector n init))

(define (bytevector-length (v :: bytevector)) ::int
  (invoke v 'size))

(define (bytevector-u8-ref (v ::bytevector) (i ::int)) ::int
  (invoke v 'intAt i))

(define (bytevector-u8-set! (v ::bytevector) (i ::int) (x ::int)) ::void
  (invoke v 'setByteAt i x))

(define (bytevector-copy v::bytevector
                         #!optional (start ::int 0) (end ::int (v:size)))
  ::bytevector
  (gnu.lists.U8Vector v start (- end start)))

;; FIXME
;; (define (bytevector-append ...) ...)

;; FIXME add start end
(define (utf8->string v::bytevector) ::string
  (v:toUtf8))

;; FIXME add start end
(define (string->utf8 v::string) ::bytevector
  (gnu.lists.U8Vector
   ((v:toString):getBytes
    (cond-expand (java-7 java.nio.charset.StandardCharsets:UTF_8)
                 (else "UTF-8")))))

