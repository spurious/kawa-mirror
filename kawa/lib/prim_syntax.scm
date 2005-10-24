;; Definitins for some primitives before we define anything else.

(%define-syntax define-syntax
  (syntax-rules ()
    ((define-syntax (name . pattern) . forms)
     (%define-syntax name (lambda pattern . forms)))
    ((define-syntax name function)
     (%define-syntax name function))))

(%define-syntax define
  (syntax-rules (::)
    ((define (name . formals) . body)
     (%define name 2 #t formals . body))
    ((define name :: type value)
     (%define name 1 type value))
    ((define name value)
     (%define name 0 #!null value))))

;; For now the same as plain define.
(%define-syntax define-for-syntax
  (syntax-rules (::)
    ((define-for-syntax (name . formals) . body)
     (%define name 2 #t formals . body))
    ((define-for-syntax name :: type value)
     (%define name 1 type value))
    ((define-for-syntax name value)
     (%define name 0 #!null value))))

(%define-syntax define-private
  (syntax-rules (::)
    ((define-private (name . formals) . body)
     (%define name 6 #t formals . body))
    ((define-private name :: type value)
     (%define name 5 type value))
    ((define-private name value)
     (%define name 4 #!null value))))

(%define-syntax define-constant
  (syntax-rules (::)
    ((define-constant (name . formals) . body)
     (%define name 10 #t formals . body))
    ((define-constant name :: type value)
     (%define name 9 type value))
    ((define-constant name value)
     (%define name 8 #!null value))))

(%define syntax-error 2 #!null (id #!rest (msg :: <Object[]>))
  (invoke-static <kawa.standard.syntax_error> 'error id msg))

(%define-syntax syntax->expression
  (syntax-rules ()
    ((syntax->expression x)
     (invoke-static <kawa.lang.SyntaxForm> 'rewrite x))))

(%define-syntax syntax-body->expression
  (syntax-rules ()
    ((syntax-body->expression x)
     (invoke-static <kawa.lang.SyntaxForm> 'rewriteBody x))))

(%define-syntax if
  (lambda (x)
    (syntax-case x ()
		 ((_ test then)
		  (make <gnu.expr.IfExp>
		    (syntax->expression (syntax test))
		    (syntax->expression (syntax then))
		    #!null))
		 ((_ test then else)
		  (make <gnu.expr.IfExp>
		    (syntax->expression (syntax test))
		    (syntax->expression (syntax then))
		    (syntax->expression (syntax else))))
		 ((_ e1 e2 e3 e4 . rest)
		  (syntax-error (syntax e4)
				"too many expressions for 'if'"))
		 ((_ . rest)
		  (syntax-error (syntax rest)
				"too few expressions for 'if'")))))

(%define-syntax try-catch
  (lambda (x)
    (syntax-case x ()
		 ((_ try-part (var type . catch-body) ...)
		  (invoke-static <kawa.standard.try_catch> 'rewrite
				 (syntax try-part)
				 (syntax
				  #((((var :: type)) . catch-body) ...)))))))

