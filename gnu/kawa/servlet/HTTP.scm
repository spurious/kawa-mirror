(define (get-request) :: <javax.servlet.http.HttpServletRequest>
  ((static-field <gnu.kawa.servlet.GetRequest> 'getRequest)))

(define (request-method) :: <String>
  (invoke (get-request) 'getMethod))

(define (request-uri) :: <String>
  (invoke (get-request) 'getRequestURI))

(define (request-url) :: <String>
  (invoke (get-request) 'getRequestURL))
