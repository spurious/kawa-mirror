(test-begin "resolve-uri")

;; Tests from http://www.ietf.org/rfc/rfc3986.txt

(define base "http://a/b/c/d;p?q")
(define-syntax test1
  (syntax-rules ()
    ((_ uri resolved)
     (begin
       (test-equal resolved ((resolve-uri (URI uri) (URI base)):toString))
       (test-equal resolved ((resolve-uri uri (URI base)):toString))))))

(test1 "g:h"           "g:h")
(test1 "g"             "http://a/b/c/g")
(test1 "./g"           "http://a/b/c/g")
(test1 "g/"            "http://a/b/c/g/")
(test1 "/g"            "http://a/g")
(test1 "//g"           "http://g")
(test1 "?y"            "http://a/b/c/d;p?y")
(test1 "g?y"           "http://a/b/c/g?y")
(test1 "#s"            "http://a/b/c/d;p?q#s")
(test1 "g#s"           "http://a/b/c/g#s")
(test1 "g?y#s"         "http://a/b/c/g?y#s")
(test1 ";x"            "http://a/b/c/;x")
(test1 "g;x"           "http://a/b/c/g;x")
(test1 "g;x?y#s"       "http://a/b/c/g;x?y#s")
(test1 ""              "http://a/b/c/d;p?q")
(test1 "."             "http://a/b/c/")
(test1 "./"            "http://a/b/c/")
(test1 ".."            "http://a/b/")
(test1 "../"           "http://a/b/")
(test1 "../g"          "http://a/b/g")
(test1 "../.."         "http://a/")
(test1 "../../"        "http://a/")
(test1 "../../g"       "http://a/g")

(test1 "../../../g"    "http://a/g")
(test1 "../../../../g" "http://a/g")
(test1 "/./g"          "http://a/g")
(test1 "/../g"         "http://a/g")
(test1 "g."            "http://a/b/c/g.")
(test1 ".g"            "http://a/b/c/.g")
(test1 "g.."           "http://a/b/c/g..")
(test1 "..g"           "http://a/b/c/..g")

(test1 "./../g"        "http://a/b/g")
(test1 "./g/."         "http://a/b/c/g/")
(test1 "g/./h"         "http://a/b/c/g/h")
(test1 "g/../h"        "http://a/b/c/h")
(test1 "g;x=1/./y"     "http://a/b/c/g;x=1/y")
(test1 "g;x=1/../y"    "http://a/b/c/y")

(test1 "g?y/./x"       "http://a/b/c/g?y/./x")
(test1 "g?y/../x"      "http://a/b/c/g?y/../x")
(test1 "g#s/./x"       "http://a/b/c/g#s/./x")
(test1 "g#s/../x"      "http://a/b/c/g#s/../x")

(define fbase "file:///b/c/d")
(define-syntax test2
  (syntax-rules ()
    ((_ uri resolved)
      (test-equal resolved ((resolve-uri uri (filepath fbase)):toString)))))

(test2 "g"             "/b/c/g")
(test2 "g/"            "/b/c/g")
(test2 "/g"            "/g")

(test-end)
