(test-begin "system")

(define split-with-kawa
  ["../bin/kawa" "-e" "(format #t \"|~{~a|~}~%\" command-line-arguments)"])

(define-syntax test-str-equal
  (syntax-rules ()
    ((_ expected value)
     (test-equal expected (->string value)))))

(define p1 &`{echo foo bar})
(test-equal "[foo bar\n]" (format #f "[~a]" p1))

(define p2 &`[in: &`{echo bar bar baz}]{tr a-m A-M})
(test-equal "[BAr BAr BAz\n]" (format #f "[~a]" p2))

(define p3 (run-process "echo foo baz"))
(test-equal "[foo baz\n]" (format #f "[~a]" p3))

(define p4 &`{sh -c 'for x in $*; do echo $x; sleep 1; done; echo Done' "-" "α" "β" "γ"})
(test-equal "[α\nβ\nγ\nDone\n]" (format #f "[~a]" p4))

(test-str-equal "|abc|def ghi|\n" &`{&[split-with-kawa] abc "def ghi"})

(let ((v1 " x yz"))
  (test-str-equal "|x|yzabc|x|yz|def x yzghi|\n"
                  &`{&[split-with-kawa] &[v1]abc &[v1] "def&[v1]ghi"}))

(let ((v1 " x yz"))
  (test-str-equal "|x|yzabc|x|yz|def x yzghi|\n"
                  &sh{&[split-with-kawa] &[v1]abc &[v1] "def&[v1]ghi"}))

(let ((v2 ["a b" "c\"d"]))
  (test-str-equal "|cmd|a b|c\"d|\n" &`{&[split-with-kawa] cmd &[v2]}))

(let ((v2 ["a b" "c\"d"]))
  (test-str-equal "|cmd|a b c\"d|\n" &`{&[split-with-kawa] cmd  "&[v2]"}))

(let ((vq (unescaped-data "' bc '")))
  (test-str-equal "|cmd|a|bc|z|\n"
                  &`{&[split-with-kawa] cmd 'a&[vq]z'}))
(let ((vq (unescaped-data "' bc '")))
  (test-str-equal "|shcmd|a|bc|z|\n"
                  &sh{&[split-with-kawa] shcmd 'a&[vq]z'}))
;; In contrast with:
(let ((vq "' bc '"))
  (test-str-equal "|cmd|a' bc 'z|\n"
                  &`{&[split-with-kawa] cmd 'a&[vq]z'}))
(let ((vu (unescaped-data "b ' c d '")))
  (test-str-equal "|cmd|a b |c|d|z|\n"
                  &`{&[split-with-kawa] cmd 'a &[vu]z'}))

(let ((tmp1 (java.io.File:createTempFile "kawa-test" #!null)))
  &`[out-to: tmp1]{echo ab cd}
  &`[out-to: tmp1]{echo cd ef}
  &`[out-append-to: tmp1]{echo gh ij}
  (test-equal "cd ef\ngh ij\n" (utf8->string (path-bytes tmp1)))
  (tmp1:delete))

(test-equal 0 (process-exit-wait (run-process "echo foo")))
(test-equal #t (process-exit-ok? (run-process "echo foo")))
(test-equal #f (process-exit-ok? (run-process "/bin/false")))
(test-equal #t (process-exit-ok? (run-process "/bin/true")))

(test-end)
