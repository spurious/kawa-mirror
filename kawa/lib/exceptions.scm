(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.misc>)

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
