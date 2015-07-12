(module-name (rnrs arithmetic bitwise))
(require <kawa.lib.numbers>)

(module-export bitwise-not
               bitwise-and bitwise-ior bitwise-xor bitwise-if
               bitwise-bit-count bitwise-length bitwise-first-bit-set
               bitwise-bit-set? bitwise-copy-bit
               bitwise-bit-field bitwise-copy-bit-field
               bitwise-arithmetic-shift
               bitwise-arithmetic-shift-left
               bitwise-arithmetic-shift-right
               bitwise-rotate-bit-field
               bitwise-reverse-bit-field)

(define-alias arithmetic-shift gnu.kawa.functions.BitwiseOp:ashift)
(define-alias bitwise-and gnu.kawa.functions.BitwiseOp:and)
(define-alias bitwise-ior gnu.kawa.functions.BitwiseOp:ior)
(define-alias bitwise-not gnu.kawa.functions.BitwiseOp:not)
(define-alias bitwise-xor gnu.kawa.functions.BitwiseOp:xor)

(define-alias bitwise-arithmetic-shift gnu.kawa.functions.BitwiseOp:ashift)
(define-alias bitwise-arithmetic-shift-left
  gnu.kawa.functions.BitwiseOp:ashiftl)
(define-alias bitwise-arithmetic-shift-right
  gnu.kawa.functions.BitwiseOp:ashiftr)
