;; char-set implementation for Kawa, backed by Unicode inversion lists
;; Copyright (C) 2011 by Jamison Hope. The implementations of
;; char-set-unfold and char-set-unfold!, and all documentation strings
;; were taken from the text of the SRFI-14 document, which  is
;; copyright (C) Olin Shivers (1998, 1999, 2000). All Rights
;; Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; INVERSION LISTS
;;;
;;; char-sets are backed by a data structure called an inversion
;;; list. These inversion lists work in Unicode code points (which
;;; are, helpfully, the values returned by char->integer). An
;;; inversion list is a sort of run-length encoding of an ordered
;;; set. Here's a description of inversion lists from
;;; http://www.ibm.com/developerworks/linux/library/l-cpinv.html :
;;;
;;; \begin{quotation}
;;; So what are inversion lists? Inversion lists are best described as
;;; a condensed summary of a bit string. They are similar to a simple
;;; run-length encoding of data, though there are some differences.
;;;
;;; Let's look at an illustrative example. Suppose you want to encode
;;; the bit string "1110011." An inversion list would store a list of
;;; three numbers: 0, 3, 5. All we store is the start position of the
;;; 1s, then the start position of the 0s, then the position of 1s
;;; again, and so on until the bit string is over.
;;;
;;; If the run begins with a 1, we start the list with the number 0,
;;; meaning the 1s start the bit string. This rule can be inverted to
;;; store a 0 if the run begins with a 0 bit, but the effect is the
;;; same as long as both the encoder and the decoder of the inversion
;;; list agree on this detail. In fact, this rule is the only tricky
;;; thing about inversion lists!
;;;
;;; If we are building an inversion list for searching only, we do not
;;; need to store the position of the last bit. If, however, we want
;;; to construct the full original data, we need to know where to stop
;;; adding bits.
;;;
;;; Making an inversion list, then, is nothing more than counting
;;; bits. For specific applications such as Unicode character ranges,
;;; inversion lists can save you a lot of time and effort.
;;; \end{quotation}
;;;
;;; Here, we do not explicitly include the position of the last bit in
;;; each list; instead we just assume that the list ends with
;;; *highest-code-point* (#\u10FFFF).
;;;
;;; Moreover, the lists are stored in arrays of ints, and they are
;;; stored *backwards*. The list length is stored separately, so that
;;; reallocation isn't required on every update. In particular,
;;; because an inversion list can be complemented by adding or
;;; removing a leading 0, we can complement the array representation
;;; with {arr[length++] = 0;} or {length--;}.
;;;
;;; EXAMPLES
;;;
;;; {#\a} => [98 97 ...]
;;; {#\a #\b #\d} => [101 100 99 97 ...]
;;; {#\a #\b #\c ... #\z} => [123 97 ...]
;;; {} => [...]
;;; {#\u0000 #\u0001 #\u0002 ... #\u10FFFF} => [0 ...]
;;;
;;; Additionally, the built-in char-sets are marked immutable, so that
;;; any attempts to mutate them via a linear-update function will
;;; signal an error (and not instead silently corrupt later
;;; computations which expect, for example, that char-set:empty is
;;; truly empty).

(module-compile-options warn-undefined-variable: #t
                        warn-invoke-unknown-method: #t)
(module-export
 ;; Predicates & comparison
 ;; char-set?
 char-set= char-set<= char-set-hash
 ;; Iterating over character sets
 char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
 char-set-fold char-set-unfold char-set-unfold! char-set-for-each
 char-set-map
 ;; Creating character sets
 char-set-copy char-set list->char-set string->char-set
 list->char-set! string->char-set! char-set-filter ucs-range->char-set
 char-set-filter! ucs-range->char-set! ->char-set
 ;; Querying character sets
 char-set->list char-set->string char-set-size char-set-count
 char-set-contains? char-set-every char-set-any
 ;; Character-set algebra
 char-set-adjoin char-set-delete char-set-adjoin! char-set-delete!
 char-set-complement char-set-union char-set-intersection
 char-set-complement! char-set-union! char-set-intersection!
 char-set-difference char-set-xor char-set-diff+intersection
 char-set-difference! char-set-xor! char-set-diff+intersection!
 ;; Standard character sets
 ;; char-set:lower-case  char-set:upper-case  char-set:title-case
 ;; char-set:letter      char-set:digit       char-set:letter+digit
 ;; char-set:graphic     char-set:printing    char-set:whitespace
 ;; char-set:iso-control char-set:punctuation char-set:symbol
 ;; char-set:hex-digit   char-set:blank       char-set:ascii
 ;; char-set:empty       char-set:full
)

;;; char-set? and char-set:* are implicitly provided by the char-set
;;; class.

(define-alias reflectArray java.lang.reflect.Array)
(define-alias Arrays java.util.Arrays)
(define-alias Character java.lang.Character)

(define-syntax array-copy
  (syntax-rules ()
    ((_ orig len)
     (cond-expand
      (java-6 (Arrays:copy-of orig len))
      (else
       (let* ((cls ::java.lang.Class
                   (*:get-component-type (*:get-class orig)))
              (arr (reflectArray:new-instance cls len))
              (cp-len (min len (reflectArray:get-length orig))))
         (java.lang.System:arraycopy orig 0 arr 0 cp-len)
         arr))))))

;;; The largest valid Unicode code point.
(define *highest-code-point* ::int #x10FFFF)

(define (char-set= #!rest (csets ::char-set[])) ::boolean
  "Are the character sets equal?

Boundary cases:
  (char-set=) => true
  (char-set= cs) => true"
  (or (< csets:length 2)
      (let loop ((i ::int 1))
        (or (= i csets:length)
            (and (equal? (csets 0) (csets i))
                 (loop (+ 1 i)))))))

(define (char-set<= #!rest (csets ::char-set[])) ::boolean
  "Returns #t if every character set cs_i is a subset of character set
cs_{i+1}.

Boundary cases:
  (char-set<=) => true
  (char-set<= cs) => true"
  (or (< csets:length 2)
      (let loop ((i ::int 1))
        (or (= i csets:length)
            (and ((csets (- i 1)):subset-of? (csets i))
                 (loop (+ 1 i)))))))

(define (char-set-hash (cs ::char-set)
                       #!optional (bound ::int 0))
  ::int
  "Compute a hash value for the character set CS. BOUND is a
non-negative exact integer specifying the range of the hash
function. A positive value restricts the return value to the range
[0,BOUND-1].

If BOUND is either zero or not given, the default range will be the
size of an int, which will maintain compatibility with Java hash
codes."
  (let ((natural-hash ::int (*:hash-code cs)))
    (when (< natural-hash 0)
          (set! natural-hash
                (modulo natural-hash java.lang.Integer:MAX_VALUE)))
    (if (or (= 0 bound) (< natural-hash bound))
        natural-hash
        (remainder natural-hash bound))))

(define (char-set-cursor (cset ::char-set)) ::int
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. `char-set-cursor' produces a new cursor for a given set."
  (*:get-cursor cset))

(define (char-set-ref (cset ::char-set) (cursor ::int)) ::character
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. The set element indexed by the cursor is fetched with
`char-set-ref'.
"
  (integer->char cursor))

(define (char-set-cursor-next (cset ::char-set) (cursor ::int)) ::int
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. A cursor index is incremented with `char-set-cursor-next'; in
this way, code can step through every character in a char set."
  (*:cursor-next cset cursor))

(define (end-of-char-set? (cursor ::int)) ::boolean
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. Stepping a cursor \"past the end\" of a char set produces a
cursor that answers true to `end-of-char-set?'. It is an error to pass
such a cursor to `char-set-ref' or to `char-set-cursor-next'."
  (> cursor *highest-code-point*))

(define (char-set-fold (kons ::procedure) knil (cs ::char-set))
  "This is the fundamental iterator for character sets. Applies the
function KONS across the character set CS using initial state value
KNIL. That is, if CS is the empty set, the procedure returns
KNIL. Otherwise, some element C of CS is chosen; let CS' be the
remaining, unchosen characters. The procedure returns
  (char-set-fold KONS (KONS C KNIL) CS')"
  (let loop ((cursor (char-set-cursor cs)) (answer knil))
    (if (end-of-char-set? cursor) answer
        (loop (char-set-cursor-next cs cursor)
              (kons (char-set-ref cs cursor) answer)))))

(define (char-set-unfold (p ::procedure) (f ::procedure)
                         (g ::procedure) seed
                         #!optional
                         (base-cs ::char-set char-set:empty))
  ::char-set
  "This is a fundamental constructor for char-sets. G is used to
generate a series of \"seed\" values from the initial seed: SEED,
(g SEED), (g (g SEED)), .... P tells us when to stop -- when it
returns true when applied to one of these seed values. F maps each
seed value to a character. These characters are added to the base
character set BASE-CS to form the result; BASE-CS defaults to the
empty set."
  ;; this implementation copied from SRFI text
  (char-set-unfold! p f g seed (char-set-copy base-cs)))

(define (char-set-unfold! (p ::procedure) (f ::procedure)
                          (g ::procedure) seed (base-cs ::char-set))
  ::char-set
  "`char-set-unfold!' adds the characters to BASE-CS in a
linear-update -- it is allowed, but not required, to side-effect and
use BASE-CS's storage to construct the result."
  ;; this implementation copied from SRFI text
  (let loop ((seed seed) (cs ::char-set base-cs))
    (if (p seed) cs
        (loop (g seed) (char-set-adjoin! cs (f seed))))))

(define (char-set-for-each (proc ::procedure) (cs ::char-set))
  "Apply procedure PROC to each character in the character set
CS. Note that the order in which PROC is applied to the characters in
the set is not specified, and may even change from one procedure
application to another."
  (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
      ((end-of-char-set? cursor) 'done) ; a single unspecified return
                                        ; value is required (due to
                                        ; R5RS language)
    (proc (char-set-ref cs cursor))))

(define (char-set-map (proc ::procedure) (cs ::char-set)) ::char-set
  "PROC is a char->char procedure. Apply it to all the characters in
the char-set CS, and collect the results into a new character set.

Essentially lifts PROC from a char->char procedure to a
char-set->char-set procedure."
  (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
       (result-cs (char-set-copy char-set:empty)
                  (char-set-adjoin! result-cs
                                    (proc (char-set-ref cs cursor)))))
      ((end-of-char-set? cursor) result-cs)))

(define (char-set-copy (cs ::char-set)) ::char-set
  "Returns a copy of the character set CS. \"Copy\" means that if
either the input parameter or the result valoue of this procedure is
passed to one of the linear-update procedures described below, the
other character set is guaranteed not to be altered."
  (*:clone cs))

;;; char-set, char-set? and the built-in char-set:* sets are all
;;; provided by the char-set class. Several of the built-ins were
;;; originally generated dynamically, but have been inlined to speed
;;; up load time. Refer to the description of inversion lists below
;;; for the meanings of the lists of numbers.

(define-simple-class char-set (java.lang.Cloneable)
  class-name: ".CharSet"

  (empty                                ; contains no characters
   ::char-set allocation: 'static init:
   (char-set name: "char-set:empty" immutable?: #t))

  (full                                 ; contains all characters
   ::char-set allocation: 'static init:
   (char-set inversion-list: [0] inversion-list-size: 1
             name: "char-set:full" immutable?: #t))

  (ascii                                ; contains 0-127
   ::char-set allocation: 'static init:
   (char-set inversion-list: [128 0] inversion-list-size: 2
             name: "char-set:ascii" immutable?: #t))

  (title-case
   ;; The set of title-case characters was taken directly from the
   ;; SRFI document.
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [8189 8188 8141 8140 8125 8124 8112 8104 8096 8088
                   8080 8072 499 498 460 459 457 456 454 453 0]
             inversion-list-size: 20 name: "char-set:title-case"
             immutable?: #t))

  (whitespace
   ;; The set of whitespace characters was taken directly from the
   ;; SRFI document.
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [12289 12288 8240 8239 8234 8232 8204 8192 5761 5760
                    161 160 33 32 14 9 0]
             inversion-list-size: 16
             name: "char-set:whitespace" immutable?: #t))

  (hex-digit                            ; [0-9A-Fa-f]
   ::char-set allocation: 'static init:
   (char-set inversion-list: [103 97 71 65 58 48 0] inversion-list-size: 6
             name: "char-set:hex-digit" immutable?: #t))

  (iso-control
   ;; Characters for which Character:isISOControl returns #t.
   ::char-set allocation: 'static init:
   (char-set inversion-list: [160 127 32 0] inversion-list-size: 4
             name: "char-set:iso-control" immutable?: #t))

  (blank
   ;; The set of blank characters was taken directly from the SRFI
   ;; document.
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [12289 12288 8240 8239 8204 8192 5761 5760 161 160
                    33 32 10 9 0]
             inversion-list-size: 14
             name: "char-set:blank" immutable?: #t))

  (lower-case
   ;; Characters for which Character:lower-case? returns #t
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [120778 120772 120771 120746 120720 120714 120713
                     120688 120662 120656 120655 120630 120604 120598
                     120597 120572 120546 120540 120539 120514 120484
                     120458 120432 120406 120380 120354 120328 120302
                     120276 120250 120224 120198 120172 120146 120120
                     120094 120068 120042 120016 120005 120004 119997
                     119996 119995 119994 119990 119964 119938 119912
                     119894 119893 119886 119860 119834 66640 66600
                     65371 65345 64280 64275 64263 64256 8522 8518 8510
                     8509 8506 8505 8501 8500 8496 8495 8468 8467 8464
                     8462 8459 8458 8320 8319 8306 8305 8184 8182 8181
                     8178 8168 8160 8152 8150 8148 8144 8136 8134 8133
                     8130 8127 8126 8120 8118 8117 8112 8104 8096 8088
                     8080 8072 8064 8062 8048 8040 8032 8024 8016 8006
                     8000 7992 7984 7976 7968 7958 7952 7944 7936 7930
                     7929 7928 7927 7926 7925 7924 7923 7922 7921 7920
                     7919 7918 7917 7916 7915 7914 7913 7912 7911 7910
                     7909 7908 7907 7906 7905 7904 7903 7902 7901 7900
                     7899 7898 7897 7896 7895 7894 7893 7892 7891 7890
                     7889 7888 7887 7886 7885 7884 7883 7882 7881 7880
                     7879 7878 7877 7876 7875 7874 7873 7872 7871 7870
                     7869 7868 7867 7866 7865 7864 7863 7862 7861 7860
                     7859 7858 7857 7856 7855 7854 7853 7852 7851 7850
                     7849 7848 7847 7846 7845 7844 7843 7842 7841 7836
                     7829 7828 7827 7826 7825 7824 7823 7822 7821 7820
                     7819 7818 7817 7816 7815 7814 7813 7812 7811 7810
                     7809 7808 7807 7806 7805 7804 7803 7802 7801 7800
                     7799 7798 7797 7796 7795 7794 7793 7792 7791 7790
                     7789 7788 7787 7786 7785 7784 7783 7782 7781 7780
                     7779 7778 7777 7776 7775 7774 7773 7772 7771 7770
                     7769 7768 7767 7766 7765 7764 7763 7762 7761 7760
                     7759 7758 7757 7756 7755 7754 7753 7752 7751 7750
                     7749 7748 7747 7746 7745 7744 7743 7742 7741 7740
                     7739 7738 7737 7736 7735 7734 7733 7732 7731 7730
                     7729 7728 7727 7726 7725 7724 7723 7722 7721 7720
                     7719 7718 7717 7716 7715 7714 7713 7712 7711 7710
                     7709 7708 7707 7706 7705 7704 7703 7702 7701 7700
                     7699 7698 7697 7696 7695 7694 7693 7692 7691 7690
                     7689 7688 7687 7686 7685 7684 7683 7682 7681 7532
                     7522 7468 7424 1416 1377 1296 1295 1294 1293 1292
                     1291 1290 1289 1288 1287 1286 1285 1284 1283 1282
                     1281 1274 1273 1270 1269 1268 1267 1266 1265 1264
                     1263 1262 1261 1260 1259 1258 1257 1256 1255 1254
                     1253 1252 1251 1250 1249 1248 1247 1246 1245 1244
                     1243 1242 1241 1240 1239 1238 1237 1236 1235 1234
                     1233 1231 1230 1229 1228 1227 1226 1225 1224 1223
                     1222 1221 1220 1219 1218 1216 1215 1214 1213 1212
                     1211 1210 1209 1208 1207 1206 1205 1204 1203 1202
                     1201 1200 1199 1198 1197 1196 1195 1194 1193 1192
                     1191 1190 1189 1188 1187 1186 1185 1184 1183 1182
                     1181 1180 1179 1178 1177 1176 1175 1174 1173 1172
                     1171 1170 1169 1168 1167 1166 1165 1164 1163 1154
                     1153 1152 1151 1150 1149 1148 1147 1146 1145 1144
                     1143 1142 1141 1140 1139 1138 1137 1136 1135 1134
                     1133 1132 1131 1130 1129 1128 1127 1126 1125 1124
                     1123 1122 1121 1120 1072 1020 1019 1017 1016 1014
                     1013 1012 1007 1006 1005 1004 1003 1002 1001 1000
                     999 998 997 996 995 994 993 992 991 990 989 988 987
                     986 985 984 981 978 976 975 940 913 912 688 592 567
                     563 562 561 560 559 558 557 556 555 554 553 552 551
                     550 549 548 547 546 545 544 543 542 541 540 539 538
                     537 536 535 534 533 532 531 530 529 528 527 526 525
                     524 523 522 521 520 519 518 517 516 515 514 513 512
                     511 510 509 508 507 506 505 502 501 500 499 497 495
                     494 493 492 491 490 489 488 487 486 485 484 483 482
                     481 480 479 478 476 475 474 473 472 471 470 469 468
                     467 466 465 464 463 462 461 460 458 457 455 454 448
                     445 443 441 439 438 437 436 433 432 430 429 428 426
                     425 424 422 421 420 419 418 417 415 414 412 409 406
                     405 403 402 398 396 393 392 390 389 388 387 385 382
                     381 380 379 378 376 375 374 373 372 371 370 369 368
                     367 366 365 364 363 362 361 360 359 358 357 356 355
                     354 353 352 351 350 349 348 347 346 345 344 343 342
                     341 340 339 338 337 336 335 334 333 332 331 330 328
                     327 326 325 324 323 322 321 320 319 318 317 316 315
                     314 313 311 310 309 308 307 306 305 304 303 302 301
                     300 299 298 297 296 295 294 293 292 291 290 289 288
                     287 286 285 284 283 282 281 280 279 278 277 276 275
                     274 273 272 271 270 269 268 267 266 265 264 263 262
                     261 260 259 258 257 256 248 247 223 187 186 182 181
                     171 170 123 97 0]
             inversion-list-size: 848
             name: "char-set:lower-case" immutable?: #t))

  (upper-case
   ;; Characters for which Character:upper-case? returns #t
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [120745 120720 120687 120662 120629 120604 120571
                     120546 120513 120488 120458 120432 120406 120380
                     120354 120328 120302 120276 120250 120224 120198
                     120172 120145 120138 120135 120134 120133 120128
                     120127 120123 120122 120120 120093 120086 120085
                     120077 120075 120071 120070 120068 120042 120016
                     119990 119982 119981 119977 119975 119973 119971
                     119970 119968 119966 119965 119964 119938 119912
                     119886 119860 119834 119808 66600 66560 65339 65313
                     8518 8517 8512 8510 8500 8499 8498 8496 8494 8490
                     8489 8488 8487 8486 8485 8484 8478 8473 8470 8469
                     8467 8464 8462 8459 8456 8455 8451 8450 8188 8184
                     8173 8168 8156 8152 8140 8136 8124 8120 8048 8040
                     8032 8031 8030 8029 8028 8027 8026 8025 8014 8008
                     8000 7992 7984 7976 7966 7960 7952 7944 7929 7928
                     7927 7926 7925 7924 7923 7922 7921 7920 7919 7918
                     7917 7916 7915 7914 7913 7912 7911 7910 7909 7908
                     7907 7906 7905 7904 7903 7902 7901 7900 7899 7898
                     7897 7896 7895 7894 7893 7892 7891 7890 7889 7888
                     7887 7886 7885 7884 7883 7882 7881 7880 7879 7878
                     7877 7876 7875 7874 7873 7872 7871 7870 7869 7868
                     7867 7866 7865 7864 7863 7862 7861 7860 7859 7858
                     7857 7856 7855 7854 7853 7852 7851 7850 7849 7848
                     7847 7846 7845 7844 7843 7842 7841 7840 7829 7828
                     7827 7826 7825 7824 7823 7822 7821 7820 7819 7818
                     7817 7816 7815 7814 7813 7812 7811 7810 7809 7808
                     7807 7806 7805 7804 7803 7802 7801 7800 7799 7798
                     7797 7796 7795 7794 7793 7792 7791 7790 7789 7788
                     7787 7786 7785 7784 7783 7782 7781 7780 7779 7778
                     7777 7776 7775 7774 7773 7772 7771 7770 7769 7768
                     7767 7766 7765 7764 7763 7762 7761 7760 7759 7758
                     7757 7756 7755 7754 7753 7752 7751 7750 7749 7748
                     7747 7746 7745 7744 7743 7742 7741 7740 7739 7738
                     7737 7736 7735 7734 7733 7732 7731 7730 7729 7728
                     7727 7726 7725 7724 7723 7722 7721 7720 7719 7718
                     7717 7716 7715 7714 7713 7712 7711 7710 7709 7708
                     7707 7706 7705 7704 7703 7702 7701 7700 7699 7698
                     7697 7696 7695 7694 7693 7692 7691 7690 7689 7688
                     7687 7686 7685 7684 7683 7682 7681 7680 4294 4256
                     1367 1329 1295 1294 1293 1292 1291 1290 1289 1288
                     1287 1286 1285 1284 1283 1282 1281 1280 1273 1272
                     1269 1268 1267 1266 1265 1264 1263 1262 1261 1260
                     1259 1258 1257 1256 1255 1254 1253 1252 1251 1250
                     1249 1248 1247 1246 1245 1244 1243 1242 1241 1240
                     1239 1238 1237 1236 1235 1234 1233 1232 1230 1229
                     1228 1227 1226 1225 1224 1223 1222 1221 1220 1219
                     1218 1216 1215 1214 1213 1212 1211 1210 1209 1208
                     1207 1206 1205 1204 1203 1202 1201 1200 1199 1198
                     1197 1196 1195 1194 1193 1192 1191 1190 1189 1188
                     1187 1186 1185 1184 1183 1182 1181 1180 1179 1178
                     1177 1176 1175 1174 1173 1172 1171 1170 1169 1168
                     1167 1166 1165 1164 1163 1162 1153 1152 1151 1150
                     1149 1148 1147 1146 1145 1144 1143 1142 1141 1140
                     1139 1138 1137 1136 1135 1134 1133 1132 1131 1130
                     1129 1128 1127 1126 1125 1124 1123 1122 1121 1120
                     1072 1024 1019 1017 1016 1015 1013 1012 1007 1006
                     1005 1004 1003 1002 1001 1000 999 998 997 996 995
                     994 993 992 991 990 989 988 987 986 985 984 981 978
                     940 931 930 913 912 910 909 908 907 904 903 902 563
                     562 561 560 559 558 557 556 555 554 553 552 551 550
                     549 548 547 546 545 544 543 542 541 540 539 538 537
                     536 535 534 533 532 531 530 529 528 527 526 525 524
                     523 522 521 520 519 518 517 516 515 514 513 512 511
                     510 509 508 507 506 505 502 501 500 498 497 495 494
                     493 492 491 490 489 488 487 486 485 484 483 482 481
                     480 479 478 476 475 474 473 472 471 470 469 468 467
                     466 465 464 463 462 461 459 458 456 455 453 452 445
                     444 441 439 438 437 436 433 432 430 429 428 426 425
                     424 422 421 420 419 418 417 415 414 412 409 406 405
                     403 402 398 396 393 392 390 389 388 387 385 382 381
                     380 379 378 376 375 374 373 372 371 370 369 368 367
                     366 365 364 363 362 361 360 359 358 357 356 355 354
                     353 352 351 350 349 348 347 346 345 344 343 342 341
                     340 339 338 337 336 335 334 333 332 331 330 328 327
                     326 325 324 323 322 321 320 319 318 317 316 315 314
                     313 311 310 309 308 307 306 305 304 303 302 301 300
                     299 298 297 296 295 294 293 292 291 290 289 288 287
                     286 285 284 283 282 281 280 279 278 277 276 275 274
                     273 272 271 270 269 268 267 266 265 264 263 262 261
                     260 259 258 257 256 223 216 215 192 91 65 0]
             inversion-list-size: 842
             name: "char-set:upper-case" immutable?: #t))

  (letter
   ;; Characters for which Character:letter? returns #t
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [195102 194560 173783 131072 120778 120772 120771
                     120746 120745 120714 120713 120688 120687 120656
                     120655 120630 120629 120598 120597 120572 120571
                     120540 120539 120514 120513 120488 120484 120146
                     120145 120138 120135 120134 120133 120128 120127
                     120123 120122 120094 120093 120086 120085 120077
                     120075 120071 120070 120005 120004 119997 119996
                     119995 119994 119982 119981 119977 119975 119973
                     119971 119970 119968 119966 119965 119894 119893
                     119808 67648 67647 67645 67644 67641 67639 67638
                     67594 67593 67592 67590 67584 66718 66560 66462
                     66432 66378 66352 66335 66304 65787 65664 65630
                     65616 65614 65599 65598 65596 65595 65576 65575
                     65549 65548 65536 65501 65498 65496 65490 65488
                     65482 65480 65474 65471 65382 65371 65345 65339
                     65313 65277 65142 65141 65136 65020 65008 64968
                     64914 64912 64848 64830 64467 64434 64326 64325
                     64323 64322 64320 64319 64318 64317 64312 64311
                     64298 64297 64287 64286 64285 64280 64275 64263
                     64256 64107 64048 64046 63744 55204 44032 42125
                     40960 40870 19968 19894 13312 12800 12784 12728
                     12704 12687 12593 12589 12549 12544 12540 12539
                     12449 12448 12445 12439 12353 12349 12347 12342
                     12337 12295 12293 8522 8517 8512 8509 8506 8499
                     8498 8495 8494 8490 8489 8488 8487 8486 8485 8484
                     8478 8473 8470 8469 8468 8458 8456 8455 8451 8450
                     8320 8319 8306 8305 8189 8182 8181 8178 8173 8160
                     8156 8150 8148 8144 8141 8134 8133 8130 8127 8126
                     8125 8118 8117 8064 8062 8031 8030 8029 8028 8027
                     8026 8025 8024 8016 8014 8008 8006 7968 7966 7960
                     7958 7936 7930 7840 7836 7680 7532 7424 6517 6512
                     6510 6480 6429 6400 6313 6272 6264 6176 6109 6108
                     6104 6103 6068 6016 6001 5998 5997 5984 5970 5952
                     5938 5920 5906 5902 5901 5888 5867 5792 5787 5761
                     5751 5743 5741 5121 5109 5024 4955 4936 4935 4896
                     4895 4888 4886 4882 4881 4880 4879 4848 4847 4824
                     4823 4816 4815 4808 4806 4802 4801 4800 4799 4792
                     4790 4786 4785 4784 4783 4752 4750 4746 4745 4744
                     4743 4704 4702 4698 4697 4696 4695 4688 4686 4682
                     4681 4680 4679 4616 4615 4608 4602 4520 4515 4447
                     4442 4352 4345 4304 4294 4256 4182 4176 4139 4137
                     4136 4131 4130 4096 3980 3976 3947 3913 3912 3904
                     3841 3840 3806 3804 3783 3782 3781 3776 3774 3773
                     3764 3762 3761 3757 3756 3754 3752 3751 3750 3749
                     3748 3745 3744 3737 3736 3732 3726 3725 3723 3722
                     3721 3719 3717 3716 3715 3713 3655 3648 3636 3634
                     3633 3585 3527 3520 3518 3517 3516 3507 3506 3482
                     3479 3461 3426 3424 3386 3370 3369 3346 3345 3342
                     3341 3333 3298 3296 3295 3294 3262 3261 3258 3253
                     3252 3242 3241 3218 3217 3214 3213 3205 3170 3168
                     3130 3125 3124 3114 3113 3090 3089 3086 3085 3077
                     3002 2999 2998 2990 2987 2984 2981 2979 2976 2974
                     2973 2972 2971 2969 2966 2962 2961 2958 2955 2949
                     2948 2947 2930 2929 2914 2911 2910 2908 2878 2877
                     2874 2869 2868 2866 2865 2858 2857 2835 2833 2831
                     2829 2821 2786 2784 2769 2768 2750 2749 2746 2741
                     2740 2738 2737 2730 2729 2707 2706 2703 2702 2693
                     2677 2674 2655 2654 2653 2649 2618 2616 2615 2613
                     2612 2610 2609 2602 2601 2579 2577 2575 2571 2565
                     2546 2544 2530 2527 2526 2524 2494 2493 2490 2486
                     2483 2482 2481 2474 2473 2451 2449 2447 2445 2437
                     2402 2392 2385 2384 2366 2365 2362 2308 1970 1969
                     1958 1920 1872 1869 1840 1810 1809 1808 1792 1791
                     1789 1786 1776 1774 1767 1765 1750 1749 1748 1649
                     1648 1646 1611 1600 1595 1569 1523 1520 1515 1488
                     1416 1377 1370 1369 1367 1329 1296 1280 1274 1272
                     1270 1232 1231 1162 1154 1024 1020 1015 1014 976
                     975 931 930 910 909 908 907 904 903 902 891 890 751
                     750 741 736 722 710 706 592 567 248 247 216 215 192
                     187 186 182 181 171 170 123 97 91 65 0]
             inversion-list-size: 650 name: "char-set:letter" immutable?: #t))

  (digit
   ;; Characters for which Character:digit? returns #t
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [120832 120782 66730 66720 65306 65296 6480 6470
                     6170 6160 6122 6112 4978 4969 4170 4160 3882 3872
                     3802 3792 3674 3664 3440 3430 3312 3302 3184 3174
                     3056 3047 2928 2918 2800 2790 2672 2662 2544 2534
                     2416 2406 1786 1776 1642 1632 58 48 0]
             inversion-list-size: 46 name: "char-set:digit" immutable?: #t))

  (punctuation
   ;; Characters whose Unicode type is one of:
   ;; Character:CONNECTOR_PUNCTUATION, Character:DASH_PUNCTUATION,
   ;; Character:START_PUNCTUATION, Character:END_PUNCTUATION,
   ;; Character:INITIAL_QUOTE_PUNCTUATION,
   ;; Character:FINAL_QUOTE_PUNCTUATION, or
   ;; Character:OTHER_PUNCTUATION
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [66464 66463 65794 65792 65382 65375 65374 65373
                    65372 65371 65344 65343 65342 65339 65313 65311
                    65308 65306 65296 65292 65291 65285 65284 65281
                    65132 65130 65129 65128 65124 65123 65122 65108
                    65107 65072 64832 64830 12540 12539 12449 12448
                    12350 12349 12337 12336 12320 12308 12306 12296
                    12292 12289 10750 10748 10716 10712 10649 10627
                    10220 10214 10102 10088 9143 9140 9003 9001 8335
                    8333 8319 8317 8280 8279 8277 8275 8274 8261 8260
                    8240 8232 8208 6470 6468 6155 6144 6107 6104 6103
                    6100 5943 5941 5870 5867 5789 5787 5743 5741 4969
                    4961 4348 4347 4176 4170 3974 3973 3902 3898 3859
                    3844 3676 3674 3664 3663 3573 3572 2417 2416 2406
                    2404 1806 1792 1749 1748 1646 1642 1568 1567 1564
                    1563 1550 1548 1525 1523 1476 1475 1473 1472 1471
                    1470 1419 1417 1376 1370 904 903 895 894 192 191
                    188 187 184 183 172 171 162 161 126 125 124 123 96
                    95 94 91 65 63 60 58 48 44 43 37 36 33 0]
             inversion-list-size: 172
             name: "char-set:punctuation" immutable?: #t))

  (symbol
   ;; Characters whose Unicode type is one of:
   ;; Character:MATH_SYMBOL, Character:CURRENCY_SYMBOL,
   ;; Character:MODIFIER_SYMBOL, or Character:OTHER_SYMBOL
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [120772 120771 120746 120745 120714 120713 120688
                     120687 120656 120655 120630 120629 120598 120597
                     120572 120571 120540 120539 120514 120513 119639
                     119552 119262 119214 119210 119180 119173 119171
                     119149 119146 119141 119082 119079 119040 119030
                     118784 65856 65847 65795 65794 65534 65532 65519
                     65512 65511 65504 65375 65374 65373 65372 65345
                     65344 65343 65342 65311 65308 65292 65291 65285
                     65284 65130 65129 65127 65124 65123 65122 65022
                     65020 64298 64297 42183 42128 19968 19904 13312
                     13056 13055 12992 12977 12938 12928 12927 12926
                     12896 12881 12880 12868 12842 12831 12800 12704
                     12694 12690 12688 12445 12443 12352 12350 12344
                     12342 12321 12320 12308 12306 12293 12292 12284
                     12272 12246 12032 12020 11931 11930 11904 11022
                     10750 10748 10716 10712 10649 10627 10224 10214
                     10192 10175 10161 10160 10136 10133 10132 10088
                     10081 10079 10072 10071 10070 10067 10063 10062
                     10061 10060 10025 10024 9996 9994 9990 9989 9985
                     9890 9888 9874 9856 9854 9753 9752 9472 9450 9372
                     9291 9280 9255 9216 9169 9143 9140 9003 9001 8592
                     8524 8522 8517 8512 8508 8506 8499 8498 8495 8494
                     8490 8489 8488 8487 8486 8485 8484 8478 8473 8470
                     8469 8468 8458 8456 8455 8451 8450 8448 8370 8352
                     8333 8330 8317 8314 8275 8274 8261 8260 8191 8189
                     8176 8173 8160 8157 8144 8141 8130 8127 8126 8125
                     6656 6624 6465 6464 6108 6107 4048 4047 4045 4039
                     4038 4030 3897 3896 3895 3894 3893 3892 3872 3866
                     3864 3859 3844 3841 3648 3647 3067 3059 2929 2928
                     2802 2801 2555 2554 2548 2546 1791 1789 1770 1769
                     1552 1550 1155 1154 1015 1014 902 900 886 884 768
                     751 750 741 736 722 710 706 248 247 216 215 185 184
                     183 182 181 180 178 174 173 172 170 162 127 126 125
                     124 97 96 95 94 63 60 44 43 37 36]
             inversion-list-size: 306 name: "char-set:symbol"
             immutable?: #t))

  (letter+digit
   ;; The union of char-set:letter and char-set:digit
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [195102 194560 173783 131072 120832 120782 120778
                     120772 120771 120746 120745 120714 120713 120688
                     120687 120656 120655 120630 120629 120598 120597
                     120572 120571 120540 120539 120514 120513 120488
                     120484 120146 120145 120138 120135 120134 120133
                     120128 120127 120123 120122 120094 120093 120086
                     120085 120077 120075 120071 120070 120005 120004
                     119997 119996 119995 119994 119982 119981 119977
                     119975 119973 119971 119970 119968 119966 119965
                     119894 119893 119808 67648 67647 67645 67644 67641
                     67639 67638 67594 67593 67592 67590 67584 66730
                     66720 66718 66560 66462 66432 66378 66352 66335
                     66304 65787 65664 65630 65616 65614 65599 65598
                     65596 65595 65576 65575 65549 65548 65536 65501
                     65498 65496 65490 65488 65482 65480 65474 65471
                     65382 65371 65345 65339 65313 65306 65296 65277
                     65142 65141 65136 65020 65008 64968 64914 64912
                     64848 64830 64467 64434 64326 64325 64323 64322
                     64320 64319 64318 64317 64312 64311 64298 64297
                     64287 64286 64285 64280 64275 64263 64256 64107
                     64048 64046 63744 55204 44032 42125 40960 40870
                     19968 19894 13312 12800 12784 12728 12704 12687
                     12593 12589 12549 12544 12540 12539 12449 12448
                     12445 12439 12353 12349 12347 12342 12337 12295
                     12293 8522 8517 8512 8509 8506 8499 8498 8495 8494
                     8490 8489 8488 8487 8486 8485 8484 8478 8473 8470
                     8469 8468 8458 8456 8455 8451 8450 8320 8319 8306
                     8305 8189 8182 8181 8178 8173 8160 8156 8150 8148
                     8144 8141 8134 8133 8130 8127 8126 8125 8118 8117
                     8064 8062 8031 8030 8029 8028 8027 8026 8025 8024
                     8016 8014 8008 8006 7968 7966 7960 7958 7936 7930
                     7840 7836 7680 7532 7424 6517 6512 6510 6470 6429
                     6400 6313 6272 6264 6176 6170 6160 6122 6112 6109
                     6108 6104 6103 6068 6016 6001 5998 5997 5984 5970
                     5952 5938 5920 5906 5902 5901 5888 5867 5792 5787
                     5761 5751 5743 5741 5121 5109 5024 4978 4969 4955
                     4936 4935 4896 4895 4888 4886 4882 4881 4880 4879
                     4848 4847 4824 4823 4816 4815 4808 4806 4802 4801
                     4800 4799 4792 4790 4786 4785 4784 4783 4752 4750
                     4746 4745 4744 4743 4704 4702 4698 4697 4696 4695
                     4688 4686 4682 4681 4680 4679 4616 4615 4608 4602
                     4520 4515 4447 4442 4352 4345 4304 4294 4256 4182
                     4176 4170 4160 4139 4137 4136 4131 4130 4096 3980
                     3976 3947 3913 3912 3904 3882 3872 3841 3840 3806
                     3804 3802 3792 3783 3782 3781 3776 3774 3773 3764
                     3762 3761 3757 3756 3754 3752 3751 3750 3749 3748
                     3745 3744 3737 3736 3732 3726 3725 3723 3722 3721
                     3719 3717 3716 3715 3713 3674 3664 3655 3648 3636
                     3634 3633 3585 3527 3520 3518 3517 3516 3507 3506
                     3482 3479 3461 3440 3430 3426 3424 3386 3370 3369
                     3346 3345 3342 3341 3333 3312 3302 3298 3296 3295
                     3294 3262 3261 3258 3253 3252 3242 3241 3218 3217
                     3214 3213 3205 3184 3174 3170 3168 3130 3125 3124
                     3114 3113 3090 3089 3086 3085 3077 3056 3047 3002
                     2999 2998 2990 2987 2984 2981 2979 2976 2974 2973
                     2972 2971 2969 2966 2962 2961 2958 2955 2949 2948
                     2947 2930 2929 2928 2918 2914 2911 2910 2908 2878
                     2877 2874 2869 2868 2866 2865 2858 2857 2835 2833
                     2831 2829 2821 2800 2790 2786 2784 2769 2768 2750
                     2749 2746 2741 2740 2738 2737 2730 2729 2707 2706
                     2703 2702 2693 2677 2674 2672 2662 2655 2654 2653
                     2649 2618 2616 2615 2613 2612 2610 2609 2602 2601
                     2579 2577 2575 2571 2565 2546 2534 2530 2527 2526
                     2524 2494 2493 2490 2486 2483 2482 2481 2474 2473
                     2451 2449 2447 2445 2437 2416 2406 2402 2392 2385
                     2384 2366 2365 2362 2308 1970 1969 1958 1920 1872
                     1869 1840 1810 1809 1808 1792 1791 1789 1774 1767
                     1765 1750 1749 1748 1649 1648 1646 1642 1632 1611
                     1600 1595 1569 1523 1520 1515 1488 1416 1377 1370
                     1369 1367 1329 1296 1280 1274 1272 1270 1232 1231
                     1162 1154 1024 1020 1015 1014 976 975 931 930 910
                     909 908 907 904 903 902 891 890 751 750 741 736 722
                     710 706 592 567 248 247 216 215 192 187 186 182 181
                     171 170 123 97 91 65 58 48 0]
             inversion-list-size: 688
             name: "char-set:letter+digit" immutable?: #t))

  (graphic
   ;; The union of char-set:letter+digit, char-set:punctuation, and
   ;; char-set:symbol.
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [195102 194560 173783 131072 120832 120782 120778
                     120488 120484 120146 120145 120138 120135 120134
                     120133 120128 120127 120123 120122 120094 120093
                     120086 120085 120077 120075 120071 120070 120005
                     120004 119997 119996 119995 119994 119982 119981
                     119977 119975 119973 119971 119970 119968 119966
                     119965 119894 119893 119808 119639 119552 119262
                     119214 119210 119180 119173 119171 119149 119146
                     119141 119082 119079 119040 119030 118784 67648
                     67647 67645 67644 67641 67639 67638 67594 67593
                     67592 67590 67584 66730 66720 66718 66560 66464
                     66463 66462 66432 66378 66352 66335 66304 65856
                     65847 65795 65792 65787 65664 65630 65616 65614
                     65599 65598 65596 65595 65576 65575 65549 65548
                     65536 65534 65532 65519 65512 65511 65504 65501
                     65498 65496 65490 65488 65482 65480 65474 65471
                     65281 65277 65142 65141 65136 65132 65128 65127
                     65108 65107 65072 65022 65008 64968 64914 64912
                     64848 64832 64467 64434 64326 64325 64323 64322
                     64320 64319 64318 64317 64312 64311 64287 64286
                     64285 64280 64275 64263 64256 64107 64048 64046
                     63744 55204 44032 42183 42128 42125 40960 40870
                     19904 19894 13056 13055 12992 12977 12938 12928
                     12927 12926 12896 12881 12880 12868 12842 12831
                     12784 12728 12694 12690 12688 12687 12593 12589
                     12549 12544 12443 12439 12353 12352 12347 12344
                     12336 12321 12296 12295 12289 12284 12272 12246
                     12032 12020 11931 11930 11904 11022 10224 10220
                     10192 10175 10161 10160 10136 10133 10132 10102
                     10081 10079 10072 10071 10070 10067 10063 10062
                     10061 10060 10025 10024 9996 9994 9990 9989 9985
                     9890 9888 9874 9856 9854 9753 9752 9472 9450 9372
                     9291 9280 9255 9216 9169 8592 8524 8509 8508 8448
                     8370 8352 8335 8330 8320 8314 8306 8305 8280 8279
                     8277 8240 8232 8208 8191 8182 8181 8178 8176 8157
                     8156 8150 8148 8134 8133 8118 8117 8064 8062 8031
                     8030 8029 8028 8027 8026 8025 8024 8016 8014 8008
                     8006 7968 7966 7960 7958 7936 7930 7840 7836 7680
                     7532 7424 6656 6624 6517 6512 6510 6468 6465 6464
                     6429 6400 6313 6272 6264 6176 6170 6160 6155 6144
                     6122 6112 6109 6100 6068 6016 6001 5998 5997 5984
                     5970 5952 5943 5941 5938 5920 5906 5902 5901 5888
                     5870 5792 5789 5761 5751 5121 5109 5024 4978 4961
                     4955 4936 4935 4896 4895 4888 4886 4882 4881 4880
                     4879 4848 4847 4824 4823 4816 4815 4808 4806 4802
                     4801 4800 4799 4792 4790 4786 4785 4784 4783 4752
                     4750 4746 4745 4744 4743 4704 4702 4698 4697 4696
                     4695 4688 4686 4682 4681 4680 4679 4616 4615 4608
                     4602 4520 4515 4447 4442 4352 4348 4347 4345 4304
                     4294 4256 4182 4160 4139 4137 4136 4131 4130 4096
                     4048 4047 4045 4039 4038 4030 3980 3976 3974 3973
                     3947 3913 3912 3904 3902 3898 3897 3896 3895 3894
                     3893 3892 3882 3866 3864 3840 3806 3804 3802 3792
                     3783 3782 3781 3776 3774 3773 3764 3762 3761 3757
                     3756 3754 3752 3751 3750 3749 3748 3745 3744 3737
                     3736 3732 3726 3725 3723 3722 3721 3719 3717 3716
                     3715 3713 3676 3663 3655 3647 3636 3634 3633 3585
                     3573 3572 3527 3520 3518 3517 3516 3507 3506 3482
                     3479 3461 3440 3430 3426 3424 3386 3370 3369 3346
                     3345 3342 3341 3333 3312 3302 3298 3296 3295 3294
                     3262 3261 3258 3253 3252 3242 3241 3218 3217 3214
                     3213 3205 3184 3174 3170 3168 3130 3125 3124 3114
                     3113 3090 3089 3086 3085 3077 3067 3059 3056 3047
                     3002 2999 2998 2990 2987 2984 2981 2979 2976 2974
                     2973 2972 2971 2969 2966 2962 2961 2958 2955 2949
                     2948 2947 2930 2918 2914 2911 2910 2908 2878 2877
                     2874 2869 2868 2866 2865 2858 2857 2835 2833 2831
                     2829 2821 2802 2801 2800 2790 2786 2784 2769 2768
                     2750 2749 2746 2741 2740 2738 2737 2730 2729 2707
                     2706 2703 2702 2693 2677 2674 2672 2662 2655 2654
                     2653 2649 2618 2616 2615 2613 2612 2610 2609 2602
                     2601 2579 2577 2575 2571 2565 2555 2554 2548 2534
                     2530 2527 2526 2524 2494 2493 2490 2486 2483 2482
                     2481 2474 2473 2451 2449 2447 2445 2437 2417 2404
                     2402 2392 2385 2384 2366 2365 2362 2308 1970 1969
                     1958 1920 1872 1869 1840 1810 1809 1808 1806 1774
                     1770 1769 1767 1765 1750 1649 1648 1632 1611 1600
                     1595 1569 1568 1567 1564 1563 1552 1548 1525 1520
                     1515 1488 1476 1475 1473 1472 1471 1470 1419 1417
                     1416 1377 1376 1369 1367 1329 1296 1280 1274 1272
                     1270 1232 1231 1162 1155 1024 1020 976 975 931 930
                     910 909 908 907 900 895 894 891 890 886 884 768 592
                     567 191 188 186 185 180 178 174 173 161 127 33 0]
             inversion-list-size: 766 name: "char-set:graphic"
             immutable?: #t))

  (printing
   ;; The union of char-set:graphic and char-set:whitespace
   ::char-set allocation: 'static init:
   (char-set inversion-list:
             [195102 194560 173783 131072 120832 120782 120778
                     120488 120484 120146 120145 120138 120135 120134
                     120133 120128 120127 120123 120122 120094 120093
                     120086 120085 120077 120075 120071 120070 120005
                     120004 119997 119996 119995 119994 119982 119981
                     119977 119975 119973 119971 119970 119968 119966
                     119965 119894 119893 119808 119639 119552 119262
                     119214 119210 119180 119173 119171 119149 119146
                     119141 119082 119079 119040 119030 118784 67648
                     67647 67645 67644 67641 67639 67638 67594 67593
                     67592 67590 67584 66730 66720 66718 66560 66464
                     66463 66462 66432 66378 66352 66335 66304 65856
                     65847 65795 65792 65787 65664 65630 65616 65614
                     65599 65598 65596 65595 65576 65575 65549 65548
                     65536 65534 65532 65519 65512 65511 65504 65501
                     65498 65496 65490 65488 65482 65480 65474 65471
                     65281 65277 65142 65141 65136 65132 65128 65127
                     65108 65107 65072 65022 65008 64968 64914 64912
                     64848 64832 64467 64434 64326 64325 64323 64322
                     64320 64319 64318 64317 64312 64311 64287 64286
                     64285 64280 64275 64263 64256 64107 64048 64046
                     63744 55204 44032 42183 42128 42125 40960 40870
                     19904 19894 13056 13055 12992 12977 12938 12928
                     12927 12926 12896 12881 12880 12868 12842 12831
                     12784 12728 12694 12690 12688 12687 12593 12589
                     12549 12544 12443 12439 12353 12352 12347 12344
                     12336 12321 12296 12295 12288 12284 12272 12246
                     12032 12020 11931 11930 11904 11022 10224 10220
                     10192 10175 10161 10160 10136 10133 10132 10102
                     10081 10079 10072 10071 10070 10067 10063 10062
                     10061 10060 10025 10024 9996 9994 9990 9989 9985
                     9890 9888 9874 9856 9854 9753 9752 9472 9450 9372
                     9291 9280 9255 9216 9169 8592 8524 8509 8508 8448
                     8370 8352 8335 8330 8320 8314 8306 8305 8280 8279
                     8277 8239 8234 8208 8204 8192 8191 8182 8181 8178
                     8176 8157 8156 8150 8148 8134 8133 8118 8117 8064
                     8062 8031 8030 8029 8028 8027 8026 8025 8024 8016
                     8014 8008 8006 7968 7966 7960 7958 7936 7930 7840
                     7836 7680 7532 7424 6656 6624 6517 6512 6510 6468
                     6465 6464 6429 6400 6313 6272 6264 6176 6170 6160
                     6155 6144 6122 6112 6109 6100 6068 6016 6001 5998
                     5997 5984 5970 5952 5943 5941 5938 5920 5906 5902
                     5901 5888 5870 5792 5789 5760 5751 5121 5109 5024
                     4978 4961 4955 4936 4935 4896 4895 4888 4886 4882
                     4881 4880 4879 4848 4847 4824 4823 4816 4815 4808
                     4806 4802 4801 4800 4799 4792 4790 4786 4785 4784
                     4783 4752 4750 4746 4745 4744 4743 4704 4702 4698
                     4697 4696 4695 4688 4686 4682 4681 4680 4679 4616
                     4615 4608 4602 4520 4515 4447 4442 4352 4348 4347
                     4345 4304 4294 4256 4182 4160 4139 4137 4136 4131
                     4130 4096 4048 4047 4045 4039 4038 4030 3980 3976
                     3974 3973 3947 3913 3912 3904 3902 3898 3897 3896
                     3895 3894 3893 3892 3882 3866 3864 3840 3806 3804
                     3802 3792 3783 3782 3781 3776 3774 3773 3764 3762
                     3761 3757 3756 3754 3752 3751 3750 3749 3748 3745
                     3744 3737 3736 3732 3726 3725 3723 3722 3721 3719
                     3717 3716 3715 3713 3676 3663 3655 3647 3636 3634
                     3633 3585 3573 3572 3527 3520 3518 3517 3516 3507
                     3506 3482 3479 3461 3440 3430 3426 3424 3386 3370
                     3369 3346 3345 3342 3341 3333 3312 3302 3298 3296
                     3295 3294 3262 3261 3258 3253 3252 3242 3241 3218
                     3217 3214 3213 3205 3184 3174 3170 3168 3130 3125
                     3124 3114 3113 3090 3089 3086 3085 3077 3067 3059
                     3056 3047 3002 2999 2998 2990 2987 2984 2981 2979
                     2976 2974 2973 2972 2971 2969 2966 2962 2961 2958
                     2955 2949 2948 2947 2930 2918 2914 2911 2910 2908
                     2878 2877 2874 2869 2868 2866 2865 2858 2857 2835
                     2833 2831 2829 2821 2802 2801 2800 2790 2786 2784
                     2769 2768 2750 2749 2746 2741 2740 2738 2737 2730
                     2729 2707 2706 2703 2702 2693 2677 2674 2672 2662
                     2655 2654 2653 2649 2618 2616 2615 2613 2612 2610
                     2609 2602 2601 2579 2577 2575 2571 2565 2555 2554
                     2548 2534 2530 2527 2526 2524 2494 2493 2490 2486
                     2483 2482 2481 2474 2473 2451 2449 2447 2445 2437
                     2417 2404 2402 2392 2385 2384 2366 2365 2362 2308
                     1970 1969 1958 1920 1872 1869 1840 1810 1809 1808
                     1806 1774 1770 1769 1767 1765 1750 1649 1648 1632
                     1611 1600 1595 1569 1568 1567 1564 1563 1552 1548
                     1525 1520 1515 1488 1476 1475 1473 1472 1471 1470
                     1419 1417 1416 1377 1376 1369 1367 1329 1296 1280
                     1274 1272 1270 1232 1231 1162 1155 1024 1020 976
                     975 931 930 910 909 908 907 900 895 894 891 890 886
                     884 768 592 567 191 188 186 185 180 178 174 173 160
                     127 32 14 9 0]
             inversion-list-size: 770
             name: "char-set:printing" immutable?: #t))

  ;; instance fields
  (inversion-list ::int[] [0])          ; list stored as int array
  (inversion-list-size ::int 0)         ; list length <= array length
  (immutable? ::boolean #f)             ; locks a set (used for
                                        ; built-ins)
  (name ::String #!null)

  ;; constructor
  ((*init* #!rest (characters ::character[]))
   "Return a character set containing the given characters."
   (if (> characters:length 0)
       (let ((chars ::character[]
                    (array-copy characters characters:length)))
         (Arrays:sort chars)
         (let ((first-pt ::int ((chars 0):int-value)))
           (let loop ((index ::int 1)
                      (pt ::int (+ 1 first-pt))
                      (inv-ls ::list `(,first-pt)))
             (cond ((= index chars:length)
                    (unless (= pt *highest-code-point*)
                            (set! inv-ls (cons pt inv-ls)))
                    (let ((len (length inv-ls)))
                      (set! inversion-list (int[] length: (+ 1 len)))
                      (set! inversion-list-size len)
                      (do ((i ::int 0 (+ i 1))
                           (inv-ls ::list inv-ls (cdr inv-ls)))
                          ((= i len))
                        (set! (inversion-list i) (car inv-ls)))))
                   (else
                    (let ((next-char-pt ::int
                                        ((chars index):int-value)))
                      (cond ((< pt next-char-pt)
                             (loop (+ 1 index) (+ 1 next-char-pt)
                                   (cons next-char-pt
                                         (cons pt inv-ls))))
                            ((= pt next-char-pt)
                             (loop (+ 1 index) (+ 1 pt) inv-ls))
                            ((> pt next-char-pt)
                             (loop (+ 1 index) pt inv-ls)))))))))))

  ;; clone
  ((clone) ::char-set
   (let ((copy ::char-set (invoke-special object (this) 'clone)))
     (set! copy:inversion-list
           (array-copy inversion-list inversion-list-size))
     (set! copy:immutable? #f)
     (set! copy:name #!null)
     copy))

  ;; hash code
  ((hash-code) (@java.lang.Override) ::int
   (let loop ((i ::int (- inversion-list-size 1)) (hash ::int 1))
     (if (= -1 i) hash
         (loop (- i 1) (+ (* 31 hash) (inversion-list i))))))

  ;; to string
  ((to-string) (@java.lang.Override) ::String
   (let ((s ::String (invoke-special object (this) 'toString)))
     (if (eq? #!null name)
         s
         (string-append s ": (" name ")"))))

  ;; equality, subset, and membership tests
  ((equals o) (@java.lang.Override) ::boolean
   (and (char-set? o)
        (let ((other ::char-set (as char-set o)))
          (and (= other:inversion-list-size inversion-list-size)
               (let loop ((i ::int 0))
                 (or (= i inversion-list-size)
                     (and (= (other:inversion-list i)
                             (inversion-list i))
                          (loop (+ i 1)))))))))

  ((subset-of? (cs ::char-set)) ::boolean
   (let loop ((ai ::int (- inversion-list-size 1))
              (bi ::int (- cs:inversion-list-size 1)))
     (cond ((= ai -1) #t)
           ((= bi -1) #f)
           ((< (inversion-list ai)
               (cs:inversion-list bi)) #f)
           ((= bi 0) #t)
           ((= ai 0) #f)
           ((< (inversion-list (- ai 1))
               (cs:inversion-list (- bi 1)))
            (loop (- ai 2) bi))
           ((= (inversion-list (- ai 1))
               (cs:inversion-list (- bi 1)))
            (loop (- ai 2) (- bi 2)))
           (else #f))))

  ((contains? (char ::character)) ::boolean
   (and (not (= 0 inversion-list-size))
        (begin
          (define charnum ::int (char->integer char))
          (let loop ((low ::int 0) (high ::int inversion-list-size))
            (let ((mid ::int (ash (+ low high) -1)))
              (cond ((= low high) #f)
                    ((and (< charnum (inversion-list mid))
                          (< mid (- inversion-list-size 1)))
                     (loop mid high))
                    ((and (> mid 0)
                          (>= charnum (inversion-list (- mid 1))))
                     (loop low mid))
                    ((and (= high inversion-list-size)
                          (= mid (- inversion-list-size 1)))
                     (>= charnum (inversion-list mid)))
                    (else (odd? (- inversion-list-size mid)))))))))

  ((size) ::int
   (let loop ((i ::int (- inversion-list-size 1))
              (num ::int 0))
     (cond ((= i -1) num)
           ((= 0 i)
            (+ num (- *highest-code-point* (inversion-list i)) 1))
           (else (loop (- i 2)
                       (+ num (- (inversion-list (- i 1))
                                 (inversion-list i))))))))

  ((to-list) ::list
   (char-set-fold cons '() (this)))

  ;; iteration
  ((get-cursor) ::int
   (if (= 0 inversion-list-size)
       (+ *highest-code-point* 1)
       (inversion-list (- inversion-list-size 1))))

  ((cursor-next (cursor ::int)) ::int
   (if (or (= 0 inversion-list-size)
           (and (even? inversion-list-size)
                (>= (+ cursor 1) (inversion-list 0))))
       (+ *highest-code-point* 1)
       (let ((cursor ::int (+ cursor 1)))
         (let loop ((low ::int 0) (high ::int inversion-list-size))
           (let ((mid ::int (ash (+ low high) -1)))
             (cond ((= low high) (inversion-list low))
                   ((< cursor (inversion-list mid))
                    (loop mid high))
                   ((and (> mid 0)
                         (>= cursor (inversion-list (- mid 1))))
                    (loop low mid))
                   ((odd? (- inversion-list-size mid)) cursor)
                   (else (inversion-list (- mid 1)))))))))

  ;; set logic mutators (complement!, intersection!, union!, xor!)
  ((complement!) ::char-set
   (when immutable?
         (error "attempted to modify an immutable char-set" (this)))
   (cond ((and (> inversion-list-size 0)
               (= 0 (inversion-list (- inversion-list-size 1))))
          (set! inversion-list-size (- inversion-list-size 1)))
         ((< inversion-list-size inversion-list:length)
          (set! (inversion-list inversion-list-size) 0)
          (set! inversion-list-size (+ 1 inversion-list-size)))
         (else                          ; must realloc
          (set! inversion-list
                (array-copy inversion-list
                            (+ 1  (* inversion-list-size 2))))
          (set! inversion-list-size (+ 1 inversion-list-size))))
   (this))

  ((adjoin! (c ::character))
   ::char-set
   (let ((i ::int (char->integer c)))
     (*:union! (this) (int[] (+ i 1) i) 2)))

  ((delete! (c ::character))
   ::char-set
   (let ((i ::int (char->integer c)))
     (*:intersection! (this) (int[] (+ i 1) i 0) 3)))

  ((combine! (arr ::int[]) (arr-size ::int) (proc ::procedure))
   ::char-set
   access: 'private
   (when immutable?
         (error "attempted to modify an immutable char-set" (this)))
   (let* ((l1 ::list (%make-boundary-pairs
                      inversion-list inversion-list-size))
          (l2 ::list (%make-boundary-pairs arr arr-size))
          (combo-pairs ::list (proc l1 l2))
          (new-length ::int (%boundary-pairs-length combo-pairs)))
     (when (or (> new-length inversion-list:length)
               (< new-length (/ inversion-list:length 2)))
           (set! inversion-list (int[] length: (* new-length 2))))
     (%write-inversion-list inversion-list combo-pairs new-length)
     (set! inversion-list-size new-length))
   (this))

  ((intersection! (cs ::char-set)) ::char-set
   (*:intersection! (this) cs:inversion-list cs:inversion-list-size))

  ((intersection! (arr ::int[]) (arr-size ::int)) ::char-set
   (*:combine! (this) arr arr-size %boundary-pairs-intersection))

  ((union! (cs ::char-set)) ::char-set
   (*:union! (this) cs:inversion-list cs:inversion-list-size))

  ((union! (arr ::int[]) (arr-size ::int)) ::char-set
   (*:combine! (this) arr arr-size %boundary-pairs-union))

  ((xor! (cs ::char-set)) ::char-set
   (*:xor! (this) cs:inversion-list cs:inversion-list-size))

  ((xor! (arr ::int[]) (arr-size ::int)) ::char-set
   (*:combine! (this) arr arr-size %boundary-pairs-xor)))

(define (list->char-set (char-list ::list)
                        #!optional
                        (base-cs ::char-set char-set:empty))
  ::char-set
  "Return a character set containing the characters in the list of
characters CHAR-LIST. If character set BASE-CS is provided, the
characters from CHAR-LIST are added to it. `list->char-set!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `list->char-set' produces a fresh character set."
  (let ((res-cs ::char-set (apply char-set char-list)))
    (char-set-union! res-cs base-cs)))

(define (list->char-set! (char-list ::list) (base-cs ::char-set))
  ::char-set
  "Return a character set containing the characters in the list of
characters CHAR-LIST. If character set BASE-CS is provided, the
characters from CHAR-LIST are added to it. `list->char-set!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `list->char-set' produces a fresh character set."
  (apply char-set-adjoin! base-cs char-list))

(define (string->char-set (s ::String)
                          #!optional
                          (base-cs ::char-set char-set:empty))
  ::char-set
  "Return a character set containing the characters in the string
S. If character set BASE-CS is provided, the characters from S are
added to it. `string->char-set!' is allowed, but not required, to
side-effect and reuse the storage in BASE-CS; `string->char-set'
produces a fresh character set."
  (list->char-set (string->list s) base-cs))

(define (string->char-set! (s ::String) (base-cs ::char-set))
  ::char-set
  "Return a character set containing the characters in the string
S. If character set BASE-CS is provided, the characters from S are
added to it. `string->char-set!' is allowed, but not required, to
side-effect and reuse the storage in BASE-CS; `string->char-set'
produces a fresh character set."
  (list->char-set! (string->list s) base-cs))

(define (char-set-filter (pred ::procedure) (cs ::char-set)
                         #!optional
                         (base-cs ::char-set char-set:empty))
  ::char-set
  "Returns a character set containing every character C in CS such
that (PRED C) returns true. If character set BASE-CS is provided, the
characters specified by PRED are added to it. `char-set-filter!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `char-set-filter' produces a fresh character set."
  (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
       (result-cs (char-set-copy base-cs)
                  (let ((c ::character (char-set-ref cs cursor)))
                    (if (pred c) (char-set-adjoin! result-cs c)
                        result-cs))))
      ((end-of-char-set? cursor) result-cs)))

(define (char-set-filter! (pred ::procedure) (cs ::char-set)
                          (base-cs ::char-set))
  ::char-set
  "Returns a character set containing every character C in CS such
that (PRED C) returns true. If character set BASE-CS is provided, the
characters specified by PRED are added to it. `char-set-filter!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `char-set-filter' produces a fresh character set."
  (let loop ((cursor (char-set-cursor cs)) (base-cs base-cs))
    (if (end-of-char-set? cursor) base-cs
        (let ((c ::character (char-set-ref cs cursor)))
          (if (pred c)
              (loop (char-set-cursor-next cs cursor)
                    (char-set-adjoin! base-cs c))
              (loop (char-set-cursor-next cs cursor) base-cs))))))

(define (ucs-range->char-set (lower ::int) (upper ::int)
                             #!optional
                             (error? ::boolean #f)
                             (base-cs ::char-set char-set:empty))
  ::char-set
  "LOWER and UPPER are exact non-negative integers; LOWER < UPPER.

Returns a character set containing every character whose UCS-4 code
lies in the half-open range [lower,upper).
If character set BASE-CS is provided, the characters specified by the
range are added to it. `ucs-range->char-set!' is allowed, but not
required, to side-effect and reuse the storage in BASE-CS;
`ucs-range->char-set' produces a fresh character set.

ERROR?, which is only meaningful for Scheme implementations which do
not natively use Unicode characters, is ignored."
  (let ((res-cs ::char-set (char-set inversion-list: [upper lower]
                                     inversion-list-size: 2)))
    (char-set-union! res-cs base-cs)))

(define (ucs-range->char-set! (lower ::int) (upper ::int)
                              (error? ::boolean)
                              (base-cs ::char-set))
  ::char-set
  "LOWER and UPPER are exact non-negative integers; LOWER < UPPER.

Returns a character set containing every character whose UCS-4 code
lies in the half-open range [lower,upper).
If character set BASE-CS is provided, the characters specified by the
range are added to it. `ucs-range->char-set!' is allowed, but not
required, to side-effect and reuse the storage in BASE-CS;
`ucs-range->char-set' produces a fresh character set.

ERROR?, which is only meaningful for Scheme implementations which do
not natively use Unicode characters, is ignored."
  (*:union! base-cs (int[] upper lower) 2))

(define (->char-set x) ::char-set
  "Coerces X into a char-set. X may be a string, character, or
char-set. A string is converted to the set of its constituent
characters; a character is converted to a singleton set; a char-set is
returned as-is. This procedure is intended for use by other procedures
that want to provide \"user-friendly,\" wide-spectrum interfaces to
their clients."
  (cond ((string? x) (string->char-set x))
        ((character? x) (char-set x))
        ((char-set? x) x)))

(define (char-set-size (cs ::char-set)) ::int
  "Returns the number of elements in character set CS."
  (*:size cs))

(define (char-set-count (pred ::procedure) (cs ::char-set)) ::int
  "Apply PRED to the chars of character set CS, and return the number
of chars that caused the predicate to return true."
  (char-set-fold
   (lambda (x sum)
     (if (pred x) (+ sum 1) sum)) 0 cs))

(define (char-set->list (cs ::char-set)) ::list
  "This procedure returns a list of the members of character set
CS. The order in which CS's characters appear in the list is not
defined, and may be different from one call to another."
  (*:to-list cs))

(define (char-set->string (cs ::char-set)) ::String
  "This procedure returns a string containing the members of character
set CS. The order in which CS's characters appear in the string is not
defined, and may be different from one call to another."
  (list->string (char-set->list cs)))

(define (char-set-contains? (cs ::char-set) (char ::character))
  ::boolean
  "This procedure tests CHAR for membership in character set CS."
  (*:contains? cs char))

(define (char-set-every (pred ::procedure) (cs ::char-set)) ::boolean
  "The `char-set-every' procedure returns true if predicate PRED
returns true of every character in the character set CS. The order in
which this procedure sequences through the elements of CS is not
specified."
  (let loop ((cursor (char-set-cursor cs)))
    (or (end-of-char-set? cursor)
        (and (pred (char-set-ref cs cursor))
             (loop (char-set-cursor-next cs cursor))))))

(define (char-set-any (pred ::procedure) (cs ::char-set))
  "`char-set-any' applies PRED to every character in character set CS,
and returns the first true value it finds. If no character produces a
true value, it returns false. The order in which this procedure
sequences through the elements of CS is not specified.

Note that if you need to determine the actual character on which a
predicate returns true, arrange for the predicate to return the
character parameter as its true value, e.g.
  (char-set-any (lambda (c) (and (char-upper-case? c) c)) cs)"
  (let loop ((cursor (char-set-cursor cs)))
    (cond ((end-of-char-set? cursor) #f)
          ((pred (char-set-ref cs cursor)) => values)
          (else (loop (char-set-cursor-next cs cursor))))))

(define (char-set-adjoin (cs ::char-set) #!rest (chars ::character[]))
  ::char-set
  "Add the CHAR_i characters to character set CS."
  (char-set-adjoin! (char-set-copy cs) chars))

(define (char-set-delete (cs ::char-set) #!rest (chars ::character[]))
  ::char-set
  "Delete the CHAR_i characters from character set CS."
  (char-set-delete! (char-set-copy cs) chars))

(define (char-set-adjoin! (cs ::char-set)
                          #!rest (chars ::character[]))
  ::char-set
  "Add the CHAR_i characters to character set CS. This is the
linear-update variant of `char-set-adjoin', which is allowed, but not
required, to side-effect its first parameter."
  (case chars:length
    ((0) cs)
    ((1) (*:adjoin! cs (chars 0)))
    (else (*:union! cs (char-set chars)))))

(define (char-set-delete! (cs ::char-set)
                          #!rest (chars ::character[]))
  ::char-set
  "Delete the CHAR_i characters from character set CS. This is the
linear-update variant of `char-set-delete', which is allowed, but not
required, to side-effect its first parameter."
  (case chars:length
    ((0) cs)
    ((1) (*:delete! cs (chars 0)))
    (else (let ((to-remove ::char-set (char-set chars)))
            (*:intersection! cs (*:complement! to-remove))))))

(define (char-set-complement (cs ::char-set)) ::char-set
  "Set complement for character sets."
  (char-set-complement! (char-set-copy cs)))

(define (char-set-union #!rest (csets ::char-set[])) ::char-set
  "Set union for character sets. `char-set-union' is n-ary; its
boundary case (when n=0) is:
  (char-set-union) => char-set:empty"
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((cs ::char-set (char-set-copy (csets 0)))
          (i ::int 1 (+ i 1)))
         ((= i csets:length) cs)
       (*:union! cs (csets i))))))

(define (char-set-intersection #!rest (csets ::char-set[])) ::char-set
  "Set intersection for character sets. `char-set-intersection' is
n-ary; its boundary case (when n=0) is:
  (char-set-intersection) => char-set:full"
  (case csets:length
    ((0) char-set:full)
    ((1) (csets 0))
    (else
     (do ((cs ::char-set (char-set-copy (csets 0)))
          (i ::int 1 (+ i 1)))
         ((= i csets:length) cs)
       (*:intersection! cs (csets i))))))

(define (char-set-difference (cs1 ::char-set)
                             #!rest (csets ::char-set[]))
  ::char-set
  "Set difference for character sets. `char-set-difference' is n-ary,
associates to the left (that is, it computes the difference between
its first argument and the union of all the other arguments), and
requires at least one argument. Its boundary case is:
  (char-set-difference cs) => cs"
  (if (= 0 csets:length) cs1
      (let ((rest (char-set-union csets)))
        (char-set-intersection cs1 (char-set-complement rest)))))

(define (char-set-xor #!rest (csets ::char-set[])) ::char-set
  "Set exclusive-or for character sets. `char-set-xor' is n-ary; its
boundary case (when n=0) is:
  (char-set-xor) => char-set:empty"
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((cs ::char-set (char-set-copy (csets 0)))
          (i ::int 1 (+ i 1)))
         ((= i csets:length) cs)
       (*:xor! cs (csets i))))))

(define (char-set-diff+intersection (cs1 ::char-set) (cs2 ::char-set)
                                    #!rest (csets ::char-set[]))
  "`char-set-diff+intersection' returns both the difference and the
intersection of the arguments -- it partitions its first parameter."
  (let ((union ::char-set (apply char-set-union cs2 csets)))
    (values (char-set-intersection cs1 (char-set-complement union))
            (char-set-intersection cs1 union))))

(define (char-set-complement! (cs ::char-set)) ::char-set
  "Set complement for character sets, linear-update variant. It is
allowed, but not required, to side-effect its argument."
  (*:complement! cs))

(define (char-set-union! #!rest (csets ::char-set[])) ::char-set
  "Set union for character sets, linear-update variant. It is allowed,
but not required, to side-effect its first argument."
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((i ::int 1 (+ i 1)))
         ((= i csets:length) (csets 0))
       (*:union! (csets 0) (csets i))))))

(define (char-set-intersection! #!rest (csets ::char-set[]))
  ::char-set
  "Set intersection for character sets, linear-update variant. It is
allowed, but not required, to side-effect its first argument."
  (case csets:length
    ((0) char-set:full)
    ((1) (csets 0))
    (else
     (do ((i ::int 1 (+ i 1)))
         ((= i csets:length) (csets 0))
       (*:intersection! (csets 0) (csets i))))))

(define (char-set-difference! (cs1 ::char-set)
                              #!rest (csets ::char-set[]))
  ::char-set
  "Set difference for character sets, linear-update variant. It is
allowed, but not required, to side-effect its first argument."
  (if (= 0 csets:length) cs1
      (let ((rest (char-set-union csets)))
        (char-set-intersection! cs1 (char-set-complement rest)))))

(define (char-set-xor! #!rest (csets ::char-set[])) ::char-set
  "Set exclusive-or for character sets, linear-update variant. It is
allowed, but not required, to side-effect its first argument."
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((i ::int 1 (+ i 1)))
         ((= i csets:length) (csets 0))
       (*:xor! (csets 0) (csets i))))))

;;; char-set-diff+intersection! is allowed to side-effect both of its
;;; first two arguments, but it doesn't.
(define char-set-diff+intersection! char-set-diff+intersection)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; HELPER FUNCTIONS ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A helper function which converts an inversion list into a list of
;;; boundary pairs -- cons cells representing groups of contiguous
;;; *included* characters, in which the car is the lower code point
;;; and the cdr is the higher code point.
(define (%make-boundary-pairs (arr ::int[]) (len ::int))
  (if (= 0 len) '()
      (begin
        (define (make-pairs (i ::int)) ::list
          (if (= i len) '()
              (cons (cons (arr (+ 1 i)) (- (arr i) 1))
                    (make-pairs (+ i 2)))))
        (if (even? len)
            (make-pairs 0)
            (cons (cons (arr 0) *highest-code-point*)
                  (make-pairs 1))))))

;;; A helper function which computes the required array size for an
;;; inversion list representing the given list of boundary pairs.
(define (%boundary-pairs-length (l ::list)) ::int
  (let loop ((l ::list l) (size ::int 0))
    (cond ((null? l) size)
          ((= *highest-code-point* (cdar l))
           (loop (cdr l) (+ size 1)))
          (else (loop (cdr l) (+ size 2))))))

;;; A helper function which writes the inversion list representation
;;; of the given boundary pairs list to the given array.
(define (%write-inversion-list (arr ::int[]) (l ::list) (len ::int))
  (when (> len 0)
        (define (write-pairs (i ::int) (l ::list))
          (when (< i len)
                (set! (arr i) (+ 1 (cdar l)))
                (set! (arr (+ i 1)) (caar l))
                (write-pairs (+ i 2) (cdr l))))
        (cond ((= *highest-code-point* (cdar l))
               (set! (arr 0) (caar l))
               (write-pairs 1 (cdr l)))
              (else (write-pairs 0 l)))))

;;; A helper function which computes the intersection of two boundary
;;; pairs lists.
(define (%boundary-pairs-intersection (l1 ::list) (l2 ::list)) ::list
  (cond ((or (null? l1) (null? l2)) '()) ; no further overlaps
        ((> (caar l1) (cdar l2)) ; (car l1) does not overlap with l2
         (%boundary-pairs-intersection (cdr l1) l2))
        ((> (caar l2) (cdar l1)) ; (car l2) does not overlap with l1
         (%boundary-pairs-intersection l1 (cdr l2)))
        (else
         (let ((l1a ::int (caar l1))
               (l1b ::int (cdar l1))
               (l2a ::int (caar l2))
               (l2b ::int (cdar l2)))
           (cond ((<= l1a l2a l1b l2b)  ; (l2a . l1b) is the overlap
                  `((,l2a . ,l1b)       ; with (car l1)
                    ,@(%boundary-pairs-intersection
                       `((,l1a . ,(- l2a 1)) ,@(cdr l1)) (cdr l2))))
                 ((<= l1a l2a l2b l1b)  ; (car l2) is entirely
                  `(,(car l2)           ; contained in (car l1)
                    ,@(%boundary-pairs-intersection
                       `((,l1a . ,(- l2a 1)) ,@(cdr l1)) (cdr l2))))
                 (else (%boundary-pairs-intersection l2 l1)))))))

;;; A helper function which computes the union of two boundary pairs
;;; lists.
(define (%boundary-pairs-union (l1 ::list) (l2 ::list)) ::list
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((or (> (cdar l2) (cdar l1))        ; swap arguments if
             (and (= (cdar l2) (cdar l1))   ; necessary to ensure
                  (< (caar l2) (cdar l1)))) ; (cdar l1) is highest
         (%boundary-pairs-union l2 l1))
        (else
         (let ((ending ::int (cdar l1)))
           (let find-start ((l1 ::list l1) (l2 ::list l2))
             ;; Walk both lists as long as there is overlap, to find
             ;; the start of the contiguous set. Then recurse.
             (cond ((null? l2)
                    `((,(caar l1) . ,ending)
                      ,@(%boundary-pairs-union (cdr l1) l2)))
                   ((>= (cdar l2) (- (caar l1) 1) (- (caar l2) 1))
                    (find-start l2 (cdr l1)))
                   ((>= (cdar l2) (- (caar l1) 1))
                    `((,(caar l1) . ,ending)
                      ,@(%boundary-pairs-union (cdr l1) (cdr l2))))
                   (else
                    `((,(caar l1) . ,ending)
                      ,@(%boundary-pairs-union (cdr l1) l2)))))))))

;;; A helper function which computes the xor of two boundary pairs
;;; lists.
(define (%boundary-pairs-xor (l1 ::list) (l2 ::list)) ::list
  (cond ((null? l1) l2)                 ; (xor () x) => x
        ((null? l2) l1)                 ; (xor x ()) => x
        ((> (caar l1) (+ 1 (cdar l2)))
         ;; (car l1) is beyond (car l2)
         `(,(car l1) ,@(%boundary-pairs-xor (cdr l1) l2)))
        ((> (caar l2) (+ 1 (cdar l1)))
         ;; (car l2) is beyond (car l1)
         `(,(car l2) ,@(%boundary-pairs-xor l1 (cdr l2))))
        ((= (caar l1) (+ 1 (cdar l2)))
         ;; (car l1) is adjacent to (car l2) -- merge them
         (%boundary-pairs-xor
          (cons (cons (caar l2) (cdar l1)) (cdr l1)) (cdr l2)))
        ((= (caar l2) (+ 1 (cdar l1)))
         ;; (car l2) is adjacent to (car l1) -- merge them
         (%boundary-pairs-xor
          (cdr l1) (cons (cons (caar l1) (cdar l2)) (cdr l2))))
        ;; the rest of these handle overlapping regions...
        ((> (cdar l1) (cdar l2)) ; (car l1) extends beyond l2
         `((,(+ 1 (cdar l2)) . ,(cdar l1))
           ,@(%boundary-pairs-xor
              `((,(caar l1) . ,(cdar l2)) ,@(cdr l1)) l2)))
        ((> (cdar l2) (cdar l1)) ; (car l2) extends beyond l1
         `((,(+ 1 (cdar l1)) . ,(cdar l2))
           ,@(%boundary-pairs-xor
              `((,(caar l2) . ,(cdar l1)) ,@(cdr l2)) l1)))
        ;; otherwise, they end at the same point
        ((> (caar l1) (caar l2)) ; (car l1) is a subset of (car l2)
         (%boundary-pairs-xor
          (cdr l1) `((,(caar l2) . ,(- (caar l1) 1)) ,@(cdr l2))))
        ((> (caar l2) (caar l1)) ; (car l2) is a subset of (car l1)
         (%boundary-pairs-xor
          (cdr l2) `((,(caar l1) . ,(- (caar l2) 1)) ,@(cdr l1))))
        ;; otherwise (car l1) == (car l2). (xor x x) => ()
        (else (%boundary-pairs-xor (cdr l1) (cdr l2)))))
