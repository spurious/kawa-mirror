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

(define (bytevector-copy v::bytevector) ::bytevector
  (gnu.lists.U8Vector v))

#|
(define (bytevector-copy-partial v::bytevector start::int end::int) ::bytevector
  (gnu.lists.U8Vector v start (- end start)))
|#

(define (utf8->string v::bytevector) ::string
  (v:toUtf8))

(define (string->utf8 v::string) ::bytevector
  (gnu.lists.U8Vector
   ((v:toString):getBytes
    (cond-expand (java-7 java.nio.charset.StandardCharsets:UTF_8)
                 (else "UTF-8")))))

