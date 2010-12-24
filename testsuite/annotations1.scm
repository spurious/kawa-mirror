(define-alias Resource javax.annotation.Resource)
(define-alias AuthenticationType javax.annotation.Resource:AuthenticationType)
(define-alias ElementType java.lang.annotation.ElementType)
;(define-constant CC javax.annotation.Resource:AuthenticationType:CONTAINER)
;(define-constant XX 20)

(define-class MyClass ()
  interface: #f
  (x #|(@java.lang.Deprecated)|#
   ;;     (@javax.jws.soap.InitParam name: (make-string 4)  value: 0)
   ;; Also array of annotation: SOAPMessageHandler
   ;(@javax.xml.bind.annotation.XmlSchemaTypes({ @javax.xml.bind.annotation.XmlSchemaType(...), @XmlSchemaType(...) })
   (@MyAnnotType
    svalue: 4324
    name: "myName"
    ;names: #("name1" "name2")
    bvalue: 2
    ivalue: (+ 100 12)
    blvalue: (> 3 4)
    chvalue: #\B
    etype: ElementType:PACKAGE
    ) ::integer)
  (y
   (@MyAnnotType
    names: (string[] "name1" "name2")
    clvalue: java.io.InputStream)
   (@javax.annotation.Resource
    authenticationType: Resource:AuthenticationType:CONTAINER
    ;;authenticationType: AuthenticationType:CONTAINER
    ;;authenticationType: javax.annotation.Resource:AuthenticationType:CONTAINER
    ;;shareable: (> 3 4)
    type: java.util.ArrayList)
   :: integer)
)
#|    
;     (@java.beans.ConstructorProperties value: #("abc" "def"))
     ::integer))
|#
