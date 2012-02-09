;;; Tests for the existence and proper behavior of bridge methods.

(test-begin "bridge-methods" 11)

(define (find-method (cls ::class)
                     (name ::String)
                     (ptypes ::class[])
                     (return-type ::class)
                     (bridge ::boolean)
                     (synthetic ::boolean))
  ::java.lang.reflect.Method
  "`find-method' looks for a method with the given NAME and signature
PTYPES/RETURN-TYPE in CLS, ignoring methods inherited from
superclasses. If no such method is found, returns #!null."
  (define methods ::java.lang.reflect.Method[] (*:get-methods cls))
  (let loop ((i ::int 0))
    (if (= i methods:length) #!null
        (let ((mtd (methods i)))
          (if (and (eq?    mtd:declaring-class cls)
                   (eq?    mtd:name            name)
                   (eq?    mtd:return-type     return-type)
                   (equal? mtd:parameter-types ptypes)
                   (eq?    (mtd:bridge?)       bridge)
                   (eq?    (mtd:synthetic?)    synthetic))
              mtd
              (loop (+ i 1)))))))

(define (method-exists? (cls ::class)
                        (name ::String)
                        (ptypes ::class[])
                        (return-type ::class)
                        (bridge ::boolean)
                        (synthetic ::boolean))
  ::boolean
  "`method-exists?' checks to see whether the class CLS has a method
with the given NAME which has a signature of taking PTYPES arguments
and returning an instance of RETURN-TYPE. We ignore inherited methods,
so this is suitable for checking for the existence of a bridge
method."
  (not (eq? #!null (find-method cls name ptypes return-type bridge
                                synthetic))))


;;; Covariant return type (returning a more specific subtype)

(define-simple-class CanBeCloned (java.lang.Cloneable)
  (x ::long)
  ((clone) ::CanBeCloned
   (invoke-special java.lang.Object (this) 'clone)))

(define no-args ::class[] (class[]))


(test-equal "covariant return type source method" #t
            (method-exists? CanBeCloned "clone" no-args
                            CanBeCloned #f #f))
(test-equal "covariant return type bridge method" #t
            (method-exists? CanBeCloned "clone" no-args
                            java.lang.Object #t #t))

;; Now test that the two methods produce identical results (i.e. that
;; the bridge method is actually invoking the source method).

;; In this case, the cloned object should also have its x set to 4.

(test-equal "covariant return source result" 4
            (with-compile-options
             warn-unknown-member: #f
             (*:invoke (find-method CanBeCloned "clone" no-args
                                    CanBeCloned #f #f)
                       (CanBeCloned x: 4)):x))

(test-equal "covariant return bridge result" 4
            (with-compile-options
             warn-unknown-member: #f
             (*:invoke (find-method CanBeCloned "clone" no-args
                                    java.lang.Object #t #t)
                       (CanBeCloned x: 4)):x))

;;; Covariant return type with classes defined in the same module.

(define-simple-class A ()
  ((get (x ::int)) ::A #!null))

(define-simple-class B (A)
  ((get (x ::int)) ::B (this)))

(define inttype ::class java.lang.Integer:TYPE)
(define int-arg ::class[] (class[] inttype))

(test-equal "covariant return source 2" #t
            (method-exists? B "get" int-arg B #f #f))

(test-equal "covariant return bridge 2" #t
            (method-exists? B "get" int-arg A #t #t))

;; Test the result. If "public A get(int)" is inherited from A or is
;; invoking A's implementation, then this test will fail.

(define my-b ::B (B))

(test-equal
 "covariant return result 2"
 my-b
 (*:invoke (find-method B "get" int-arg A #t #t) my-b (as int 0)))

;;; Parameterized interface (bridge for type erasure)

(define-simple-class CanBeCompared
  (java.lang.Comparable[CanBeCompared])
  (x ::int)
  ((compareTo (o ::CanBeCompared)) ::int
   (- x o:x)))

(test-equal "type erasure source method" #t
            (method-exists? CanBeCompared "compareTo"
                            (class[] CanBeCompared)
                            inttype #f #f))

(test-equal "type erasure bridge method" #t
            (method-exists? CanBeCompared "compareTo"
                            (class[] object)
                            inttype #t #t))

(define comp1 ::CanBeCompared (CanBeCompared x: 10))
(define comp2 ::CanBeCompared (CanBeCompared x: 5))

(test-equal "type erasure bridge result"
            5
            (*:invoke (find-method CanBeCompared "compareTo"
                                   (class[] object) inttype #t #t)
                      comp1 comp2))

(test-error "bridge method arg casting" java.lang.ClassCastException
            (comp1:compareTo (Object)))

(test-end)
