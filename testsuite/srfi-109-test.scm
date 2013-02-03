(test-begin "srfi-109")

(define-syntax strtest
  (syntax-rules ()
    ((strtest string quoted evaluated)
     (begin
       (test-equal quoted (quote string))
       (test-equal evaluated string)))))

(strtest &{abc}
         '($string$ "abc")
         "abc")
(strtest &{ab&(+ 3 4)xz}
         '($string$ "ab" |$[$| (+ 3 4) |$]$| "xz")
         "ab7xz")
(strtest &{ab&[(+ 3 4)]xz}
         '($string$ "ab" |$[$| (+ 3 4) |$]$| "xz")
         "ab7xz")
;; Literal nested braces.
(strtest &{ab{x}{}c{{d}}}
         '($string$ "ab{x}{}c{{d}}")
         "ab{x}{}c{{d}}")
;; Literal nested braces with enclosed expression.
(strtest &{ab{&[(+ 5 7)]c}z}
         '($string$ "ab{" |$[$| (+ 5 7) |$]$| "c}z")
         "ab{12c}z")
(strtest &{ab&[3 4]xzy}
         '($string$ "ab" |$[$| 3 4 |$]$| "xzy")
         "ab3 4xzy")
(strtest &{_&lbrace;_&rbrace;_&gt;_&lt;_&quot;_&apos;_}
         '($string$ "_" $entity$:lbrace "_" $entity$:rbrace "_" $entity$:gt
                    "_" $entity$:lt "_" $entity$:quot "_" $entity$:apos "_")
         "_{_}_>_<_\"_'_")

(strtest &{_&alarm;_&backspace;_&delete;_&escape;_&newline;_&null;_&return;_&space;_&tab;_}
         '($string$ "_" $entity$:alarm "_" $entity$:backspace
                    "_" $entity$:delete "_" $entity$:escape "_" $entity$:newline
                    "_" $entity$:null "_" $entity$:return "_" $entity$:space
                    "_" $entity$:tab "_")
         "_\a_\b_\x7f;_\x1b;_\n_\x0;_\r_ _\t_")

(strtest &{a
b}
         '($string$ "a\nb")
         "a\nb")

(strtest &{_&#64;_&#x3f;_&#125;_}
         '($string$ "_@_?_}_")
         "_@_?_}_")

(strtest &{abc&#|comment|#xyz} '($string$ "abcxyz") "abcxyz")

(strtest &{abc
    &|def
    &| klm}
         '($string$ "abc\ndef\n klm")
         "abc\ndef\n klm")

(strtest &{abc&-
  def&-
  &| klm}
         '($string$ "abc  def klm")
         "abc  def klm")

;; Some tests using format
(strtest &{abc&~3d(+ 4 5)z}
         '($string$ "abc" ($format$ "~3d" (+ 4 5)) "z")
         "abc  9z")

(test-end)
