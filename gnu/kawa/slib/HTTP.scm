(define (response-header key value)
  ((static-field <gnu.kawa.xml.MakeResponseHeader> 'makeResponseHeader)
   key value))

(define (response-content-type type)
  (response-header '|Content-Type| type))

(define (get-request) :: <javax.servlet.http.HttpServletRequest>
  ((static-field <gnu.kawa.servlet.GetRequest> 'getRequest)))

(define (request-method) :: <String>
  (invoke (get-request) 'getMethod))

(define (request-uri) :: <String>
  (invoke (get-request) 'getRequestURI))

(define (request-url) :: <String>
  (invoke (get-request) 'getRequestURL))

(define (request-path-info) :: <String>
  (invoke (get-request) 'getPathInfo))

(define (request-path-translated) :: <String>
  (invoke (get-request) 'getPathTranslated))

(define (request-servlet-path) :: <String>
  (invoke (get-request) 'getServletPath))

(define (request-query-string)
  (let ((query (invoke (get-request) 'getQueryString)))
    (if (eq? query #!null) #f query)))

