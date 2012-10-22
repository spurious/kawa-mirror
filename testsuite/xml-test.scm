(test-begin "xml")
(test-equal
 '($xml-element$ () ($resolve-qname$ a)
                 ($xml-attribute$ 'id "mine"))
 '#<a id="mine"/>)
(test-equal
 '($xml-element$ (("x" "X")) ($resolve-qname$ a x)
                 ($xml-attribute$ ($resolve-qname$ id x) "mine"))
 '#<x:a xmlns:x="X" x:id="mine"/>)
(test-equal
 '($xml-element$ (("" "X")) ($resolve-qname$ a)
                 ($xml-attribute$ 'id "mine"))
 '#<a xmlns="X" id="mine"/>)

(test-equal
 "<a id=\"mine\" />"
 (format #f "~a" #<a id="mine"/>))
(test-equal
 "<x:a xmlns:x=\"X\" x:id=\"mine\">text</x:a>"
 (format #f #<x:a xmlns:x="X" x:id="mine">text</>))
(test-end)
