;; Utility functions primarily for XQuery and XSLT.

(module-compile-options full-tailcalls: #t)

(define (count-values arg) ::int
  (if (instance? arg gnu.mapping.Values)
      ((as gnu.mapping.Values arg):size)
      1))
