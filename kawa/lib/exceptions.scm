(require <kawa.lib.ExceptionClasses>)

(define-procedure with-exception-handler
  validate-apply:  "gnu.kawa.functions.CompileMisc:validateApplyWithExceptionHandler"
  (lambda (handler::procedure thunk::procedure)
    name: 'with-exception-handler
    ;; It would be neat to be able to write just 
    ;;    (with-exception-handler handler (lambda () (thunk)))))
    ;; and have validateApplyWithExceptionHandler generate the
    ;; necessary code.  However, we only support calling the
    ;; validate-apply method for already-compiled Procedure values.
    (let ((link (HandlerLink:push handler)))
      (try-catch
       (let ((v (thunk)))
         (link:pop)
         v)
       (ex java.lang.Throwable
           (primitive-throw (link:handle ex)))))))

(define (raise obj)
  (primitive-throw (ExceptionWithValue:wrap obj)))

(define (raise-continuable obj)
  (let ((save (current-handler:get)))
    (try-finally
     (begin
       (current-handler:set save:outer)
       (save:handlerProc obj))
     (current-handler:set save))))

(define-syntax simple-guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     (try-catch
      (begin e1 e2 ...)
      (ex java.lang.Throwable
          (let ((var (ExceptionWithValue:unwrap ex)))
            (guard-aux
             (primitive-throw ex)
             clause ...)))))))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            ((call/cc
              (lambda (handler-k)
                (guard-k
                 (lambda ()
                   (let ((var condition))
                     (guard-aux
                      (handler-k
                       (lambda ()
                         (raise-continuable condition)))
                      clause ...))))))))
          (lambda ()
            (call-with-values
                (lambda () e1 e2 ...)
              (lambda args
                (guard-k
                 (lambda ()
                   (apply values args)))))))))))))


(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

;; Guile has: (throw key . args) where key is a symbol.
;; We also support: (throw throwable)
(define (throw #!rest (args ::Object[])) ::never-returns
  (let ((len args:length))
    (if (> len 0)
        (let ((key (args 0)))
          (cond ((symbol? key)
                 (primitive-throw (kawa.lang.NamedException key args)))
                ((and (java.lang.Throwable? key) (= len 1))
                 (gnu.kawa.reflect.Throw:doThrow key)))))
    (primitive-throw (kawa.lang.GenericError "bad arguments to throw"))))
              
;;; The one-argument case is a standard DSSSL procedure.
;;; The multi-argument extension matches Guile.
(define (error #!rest args::Object[])  ::never-returns
  (primitive-throw (kawa.lang.NamedException:makeError args)))

(define (catch key (thunk :: <procedure>) (handler :: <procedure>))
  (try-catch (thunk)
	     (ex <kawa.lang.NamedException>
                 (invoke ex 'applyHandler key handler))))

(define (error-object? obj) ::boolean
  (instance? obj kawa.lang.NamedException))

(define (error-object-message err::kawa.lang.NamedException)
  (err:getObjectMessage))

(define (error-object-irritants err::kawa.lang.NamedException) ::list
  (err:getObjectIrritants))

(define (read-error? obj) ::boolean
  (instance? obj gnu.text.SyntaxException))

(define (file-error? obj) ::boolean
  (instance? obj java.io.FileNotFoundException))
