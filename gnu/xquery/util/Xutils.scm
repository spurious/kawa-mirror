;; Utility functions primarily for XQuery and XSLT.

(module-compile-options full-tailcalls: #t)

(define-alias BooleanValue gnu.xquery.util.BooleanValue)

(define (count-values arg) ::int
  (if (instance? arg gnu.mapping.Values)
      ((as gnu.mapping.Values arg):size)
      1))

(define (every proc::procedure values)::boolean
  (every-or-some-values proc values #t))

(define (some proc::procedure values)::boolean
  (every-or-some-values proc values #f))

(define-private (every-or-some-values proc::procedure values match-all::boolean)::boolean
  (if (instance? values gnu.mapping.Values)
      (let* ((v ::gnu.mapping.Values values)
             (ipos ::int 0))
        (let loop ()
          (set! ipos (v:nextPos ipos))
          (if (= ipos 0)
              match-all
              (let* ((item (v:getPosPrevious ipos))
                     (ok (BooleanValue:booleanValue (proc item))))
                (if (eqv? ok match-all)
                    (loop)
                    ok)))))
      (BooleanValue:booleanValue (proc values))))
