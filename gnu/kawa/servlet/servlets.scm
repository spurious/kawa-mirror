(define (current-servlet) ::javax.servlet.http.HttpServlet
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "current-servlet"):getServlet))

(define (current-servlet-context) ::javax.servlet.ServletContext
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "current-servlet-context"):getServletContext))

(define (current-servlet-config) ::javax.servlet.ServletConfig
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "current-servlet-config"):getServletConfig))

(define (servlet-context-realpath #!optional (path :: <String> '||)) :: <String>
  (((gnu.kawa.servlet.KawaServlet$Context:getInstance "servlet-context-realpath"):getServletContext):getRealPath))

(define (get-response) ::javax.servlet.http.HttpServletResponse
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "get-response"):getResponse))

(define (get-request) ::javax.servlet.http.HttpServletRequest
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "get-request"):getRequest))

(define (request-servlet-path) ::String
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "request-servlet-path"):getServletPath))

(define (request-path-info) ::String
  (((gnu.kawa.servlet.KawaServlet$Context:getInstance "request-path-info"):getRequest):getPathInfo))

  