(define (response-header key value)
  ((static-field <gnu.kawa.xml.MakeResponseHeader> 'makeResponseHeader)
   key value))

(define (response-content-type type)
  (response-header '|Content-Type| type))

(define (response-status (code :: <int>) #!optional (message :: <String>  #!null))
  (response-header '|Status|
		     (format (if (eq? message #!null) "~d " "~d ~a")
			     code message)))

;; For now the same as response-status, "Error" is the default for message.
(define (error-response (code :: <int>)
			#!optional (message :: <String>  "Error"))
  (response-header '|Status|
		     (format "~d ~a" code message)))

(define (current-servlet) :: <javax.servlet.http.HttpServlet>
  (invoke-static <gnu.kawa.servlet.ServletCallContext> "getServlet"))

(define (current-servlet-context) :: <javax.servlet.ServletContext>
  (invoke-static <gnu.kawa.servlet.ServletCallContext> 'getServletContext))

(define (current-servlet-config) :: <javax.servlet.ServletConfig>
  (invoke-static <gnu.kawa.servlet.ServletCallContext> 'getServletConfig))

(define (servlet-context-realpath #!optional (path :: <String> '||)) :: <String>
  (let ((context :: <javax.servlet.ServletContext>
		 (invoke-static <gnu.kawa.servlet.ServletCallContext>
				'getServletContext)))
    (invoke context 'getRealPath path)))

(define (get-response) :: <javax.servlet.http.HttpServletResponse>
  (invoke-static <gnu.kawa.servlet.ServletCallContext> 'getResponse))

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

(define (request-parameter (name :: <String>) #!optional (default #!null))
  :: <String>
  (let ((value :: <java.lang.String>
	       (invoke (get-request) 'getParameter name)))
    (if (eq? value #!null) default value)))

(define (request-parameters (name :: <String>))
  (make <gnu.mapping.Values>
    (invoke (get-request) 'getParameterValues name)))
