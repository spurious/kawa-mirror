(import (scheme base)
        (scheme write)
        (tests scheme time)
        (tests scheme test))

(display "Running tests for (scheme time)\n")
(define rt (run-time-tests))
(cond (#f ;; avoid non-repeatable output
       (write (round (/ rt 1e6)))
       (display " megaloops/s\n")))
(report-test-results)
