(define (parse-xml-from-url url) :: <document>
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (invoke-static <gnu.kawa.xml.ParsedXMLToConsumer> 'parse url))

(define (print-as-xml (data :: <gnu.kawa.util.Consumable>)
		      #!optional (out (current-output-port)))
  (invoke data 'consume (make <gnu.kawa.xml.XMLPrinter> out)))
