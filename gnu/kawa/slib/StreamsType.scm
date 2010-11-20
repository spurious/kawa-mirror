(module-export stream-type stream? make-stream stream-promise stream-promise!)
(require gnu.kawa.slib.DefineRecordType "DefineRecordType.scm")

;; This is the R6RS-style to define a record type.
;; (define-record-type (stream-type make-stream stream?)
;;   (fields (mutable box stream-promise stream-promise!)))
;; Kawa version:
(define-record-type stream-type
  (make-stream box)
  stream?
  (box stream-promise stream-promise!))
