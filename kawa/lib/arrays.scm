;;; Procedures for implementing SRFI-25 arrays.

(require <kawa.lib.prim_syntax>)

(define-alias <array>  <gnu.lists.Array>)

(define (array? x) :: <boolean>
  (instance? x <array>))

(define (shape #!rest (args :: <Object[]>)) :: <array>
  (invoke-static <gnu.kawa.functions.Arrays> 'shape args))

(define (make-array (shape :: <array>) #!rest (obj ::object[])) :: <array>
  (invoke-static <gnu.kawa.functions.Arrays> 'makeFromValues shape obj))

(define (array (shape :: <array>) #!rest (vals :: <Object[]>))
  (invoke-static <gnu.kawa.functions.Arrays> 'makeSimple
		 shape (gnu.lists.FVector vals)))

(define (array-rank (array :: <array>)) :: <int>
  (invoke array 'rank))

(define (array-size (arr :: <array>)) :: <int>
  (arr:getSize))

(define (array-start (array :: <array>) (k :: <int>)) :: <int>
  (invoke array 'getLowBound k))

(define (array-end  (array :: <array>) (k :: <int>)) :: <int>
  (+ (invoke array 'getLowBound k) (invoke array 'getSize k)))

(define (share-array (array :: <array>) (shape :: <array>)
		     (mapper :: <procedure>))
  (invoke-static  <gnu.kawa.functions.Arrays> 'shareArray array shape mapper))

(define (array-index-ref (arr ::<array>) #!rest (indexes ::object[]))
  (gnu.lists.ComposedArray:generalIndex arr #f @indexes))

(define (array-index-share (arr ::<array>) #!rest (indexes ::object[]))
  (gnu.lists.ComposedArray:generalIndex arr #t @indexes))

(define (array-flatten (arr ::<array>))
  (gnu.lists.Arrays:flattenCopy arr #t))

(define (array->vector (arr ::<array>))
  (gnu.lists.FlattenedArray:flatten arr))

(define (index-array (shape ::<array>)) ::<array>
  (let ((arr (gnu.kawa.functions.Arrays:allocateArray shape)))
    (arr:setBase gnu.lists.Range:zeroAndUp)
    arr))

(define (array-copy! dst::<array> src::<array>)::void
   (gnu.lists.Arrays:copy dst src))

(define (array-fill! arr::<array> value)::void
  (gnu.lists.Arrays:fill arr value))

(define (array-transform arr::<array> shape::<array> mapper::procedure)::<array>
  (gnu.kawa.functions.Arrays:getTransformed arr mapper shape))

(define (build-array shape::<array> proc::procedure) ::<array>
   (gnu.kawa.functions.Arrays:getBuiltArray shape proc))

(define (array-reshape arr::<array> shape::<array>)::<array>
  (let* ((result (gnu.kawa.functions.Arrays:allocateArray shape))
         (vec (gnu.lists.FlattenedArray:flatten arr))
         (vsz (vec:size))
         (sz (result:getSize)))
    (if (not (= sz vsz))
        (error (format "shape requires ~d elements but argument has ~d"
                       sz vsz)))
    (result:setBase vec)
    result))

(define (format-array value #!optional (elementFormat ::string #!null))::string
  (gnu.kawa.functions.ArrayPrint:print value elementFormat))
