(define (scheme-window #!optional share)
  (let* ((language :: <gnu.expr.Language>
		 (invoke-static <gnu.expr.Language> 'getDefaultLanguage))
	 (env :: <gnu.mapping.Environment>
	      (if share (interaction-environment)
		  (invoke language 'getNewEnvironment))))
    ((primitive-constructor <kawa.GuiConsole> (<gnu.expr.Language>
					       <gnu.mapping.Environment>))
     language env)))
