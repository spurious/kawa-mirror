(define (%process-activity form)
  (syntax-case form (on-create on-create-view)
    (((on-create stmt ...) . rest)
     (cons #`( (onCreate (savedInstanceState :: android.os.Bundle)):: void
	       (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
	       stmt ...)
	   (%process-activity #`rest)))
    (((on-create-view stmt ... view) . rest)
     (cons #`( (onCreate (savedInstanceState :: android.os.Bundle)):: void
	       (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
	       stmt ...
	       ((this):setContentView view))
	   (%process-activity #`rest)))
    ((first . rest)
     (cons #`first (%process-activity #`rest)))
    (()
     '())))

(define-syntax-case activity (on-create on-create-view)
  ((activity name . parts)
   #`(define-simple-class name (android.app.Activity)
       ,@(%process-activity #`parts))))
