(define (car x)
  (if (eq? x '()) x (slot-ref (as <pair> x) 'car)))

(define (cdr x)
  (if (eq? x '()) x (slot-ref (as <pair> x) 'cdr)))

(define (setcar (p <pair>) x)
  (set-car! p x))

(define (setcdr (p <pair>) x)
  (set-cdr! p x))
