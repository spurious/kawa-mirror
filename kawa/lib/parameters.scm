;; This implements SRFI-39 "Parameter objects".
(module-export make-parameter parameterize)

(require <kawa.lib.prim_syntax>)

(define (make-parameter init #!optional (converter #!null))
  (if (not (eq? converter #!null))
      (set! init (converter init)))
  (let ((loc (gnu.mapping.ThreadLocation:new)))
    (invoke loc 'setGlobal init)
    (gnu.mapping.LocationProc:new loc converter)))

(define (as-location% param) :: <gnu.mapping.Location>
  (let ((loc (if (instance? param <gnu.mapping.LocationProc>)
		 (gnu.mapping.LocationProc:getLocation param)
		 (as <gnu.mapping.Location> param))))
    (if (instance? loc <gnu.mapping.ThreadLocation>)
	(set! loc (gnu.mapping.ThreadLocation:getLocation loc)))
    loc))

(define-syntax parameterize%
  (syntax-rules ()
    ((parameterize% () restore . body)
     (try-finally
      (begin . body)
      (begin . restore)))
    ((parameterize% ((param1 value1) . rest) restore . body)
     (let* ((p :: <gnu.mapping.Location> (as-location% param1))
	    (v value1)
	    (save (gnu.mapping.Location:setWithSave p v)))
       (parameterize% rest
		      ((gnu.mapping.Location:setRestore p save) . restore)
		      . body)))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () . body)
     (begin . body))
    ((parameterize ((param1 value1) . rest) . body)
     (parameterize% ((param1 value1) . rest) () . body))))
