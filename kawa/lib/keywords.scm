(define (keyword? object) :: <boolean>
  (invoke-static <keyword> 'isKeyword object))

(define (keyword->string (keyword :: <keyword>)) :: <string>
  (make <string>
    (invoke keyword 'getName)))

(define (string->keyword (string :: <String>)) :: <keyword>
  (invoke-static <keyword> 'make string))
