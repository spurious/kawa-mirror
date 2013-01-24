(test-begin "xml")

(define-syntax xmltest
  (syntax-rules ()
    ((xmltest value quoted evaluated)
     (begin
       (test-equal quoted (quote value))
       (test-equal evaluated (format "~a" value))))))

(xmltest #<a id="mine"/>
         '($xml-element$ () ($resolve-qname$ a)
                 ($xml-attribute$ 'id "mine"))
         &{<a id="mine" />})

(xmltest #<x:a xmlns:x="X" x:id="mine">text</x:a>
         '($xml-element$ (("x" "X")) ($resolve-qname$ a x)
                         ($xml-attribute$ ($resolve-qname$ id x) "mine")
                         "text")
         &{<x:a xmlns:x="X" x:id="mine">text</x:a>})

(xmltest #<a xmlns="X" id="mine"/>
         '($xml-element$ (("" "X")) ($resolve-qname$ a)
                         ($xml-attribute$ 'id "mine"))
         &{<a xmlns="X" id="mine" />})

(let ((xid 'id)
      (xa 'a)
      (xine "ine")
      (xtex "tex"))
(xmltest #<[xa] [xid]="m&[xine]">&[xtex]t</>
         '($xml-element$ () xa
                 ($xml-attribute$ xid "m" xine)
                 xtex "t")
         &{<a id="mine">text</a>}))

;; Computed attribute value.
(let ((idatr ($xml-attribute$ 'id "myId")))
  (xmltest #<ab [idatr] x="name"/>
           '($xml-element$ () ($resolve-qname$ ab)
                           idatr ($xml-attribute$ 'x "name"))
           &{<ab id="myId" x="name" />}))

(test-end)
