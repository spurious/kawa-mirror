;; Uniform vectors, as specified by SRFI-4.

(define (s8vector? x)
  (instance? x <s8vector>))

(define (make-s8vector (n :: <int>) #!optional (init :: <int> 0))
  (make <s8vector> n init))

(define (s8vector . values)
  (list->s8vector values))

(define (s8vector-length (v :: <s8vector>) <int>)
  (invoke v 'length))

(define (s8vector-ref (v :: <s8vector>) (i :: <int>))
  (invoke v 'intValue i))

(define (s8vector-set! (v :: <s8vector>) (i :: <int>) x)
  (invoke v 'setElementAt x i))

(define (s8vector->list (v :: <s8vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->s8vector (l :: <list>)) <s8vector>
  (make <s8vector> l))

(define (u8vector? x)
  (instance? x <u8vector>))

(define (make-u8vector (n :: <int>) #!optional (init :: <int> 0))
  (make <u8vector> n init))

(define (u8vector . values)
  (list->u8vector values))

(define (u8vector-length (v :: <u8vector>) <int>)
  (invoke v 'length))

(define (u8vector-ref (v :: <u8vector>) (i :: <int>))
  (invoke v 'intValue i))

(define (u8vector-set! (v :: <u8vector>) (i :: <int>) x)
  (invoke v 'setElementAt x i))

(define (u8vector->list (v :: <u8vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->u8vector (l :: <list>)) <u8vector>
  (make <u8vector> l))

(define (s16vector? x)
  (instance? x <s16vector>))

(define (make-s16vector (n :: <int>) #!optional (init :: <int> 0))
  (make <s16vector> n init))

(define (s16vector . values)
  (list->s16vector values))

(define (s16vector-length (v :: <s16vector>) <int>)
  (invoke v 'length))

(define (s16vector-ref (v :: <s16vector>) (i :: <int>))
  (invoke v 'intValue i))

(define (s16vector-set! (v :: <s16vector>) (i :: <int>) x)
  (invoke v 'setElementAt x i))

(define (s16vector->list (v :: <s16vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->s16vector (l :: <list>)) <s16vector>
  (make <s16vector> l))

(define (u16vector? x)
  (instance? x <u16vector>))

(define (make-u16vector (n :: <int>) #!optional (init :: <int> 0))
  (make <u16vector> n init))

(define (u16vector . values)
  (list->u16vector values))

(define (u16vector-length (v :: <u16vector>) <int>)
  (invoke v 'length))

(define (u16vector-ref (v :: <u16vector>) (i :: <int>))
  (invoke v 'intValue i))

(define (u16vector-set! (v :: <u16vector>) (i :: <int>) x)
  (invoke v 'setElementAt x i))

(define (u16vector->list (v :: <u16vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->u16vector (l :: <list>)) <u16vector>
  (make <u16vector> l))

(define (s32vector? x)
  (instance? x <s32vector>))

(define (make-s32vector (n :: <int>) #!optional (init :: <int> 0))
  (make <s32vector> n init))

(define (s32vector . values)
  (list->s32vector values))

(define (s32vector-length (v :: <s32vector>) <int>)
  (invoke v 'length))

(define (s32vector-ref (v :: <s32vector>) (i :: <int>))
  (invoke v 'intValue i))

(define (s32vector-set! (v :: <s32vector>) (i :: <int>) (x :: <int>))
  (invoke v 'set i x))

(define (s32vector->list (v :: <s32vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->s32vector (l :: <list>)) <s32vector>
  (make <s32vector> l))

(define (u32vector? x)
  (instance? x <u32vector>))

(define (make-u32vector (n :: <int>) #!optional (init 0))
  (make <u32vector> n init))

(define (u32vector . values)
  (list->u32vector values))

(define (u32vector-length (v :: <u32vector>) <int>)
  (invoke v 'length))

(define (u32vector-ref (v :: <u32vector>) (i :: <int>))
  (invoke v 'get i))

(define (u32vector-set! (v :: <u32vector>) (i :: <int>) x)
  (invoke v 'setElementAt x i))

(define (u32vector->list (v :: <u32vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->u32vector (l :: <list>)) <u32vector>
  (make <u32vector> l))

(define (s64vector? x)
  (instance? x <s64vector>))

(define (make-s64vector (n :: <int>) #!optional (init :: <long> 0))
  (make <s64vector> n init))

(define (s64vector . values)
  (list->s64vector values))

(define (s64vector-length (v :: <s64vector>) <int>)
  (invoke v 'length))

(define (s64vector-ref (v :: <s64vector>) (i :: <int>))
  (invoke v 'longValue i))

(define (s64vector-set! (v :: <s64vector>) (i :: <int>) (x :: <long>))
  (invoke v 'set i x))

(define (s64vector->list (v :: <s64vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->s64vector (l :: <list>)) <s64vector>
  (make <s64vector> l))

(define (u64vector? x)
  (instance? x <u64vector>))

(define (make-u64vector (n :: <int>) #!optional (init 0))
  (make <u64vector> n init))

(define (u64vector . values)
  (list->u64vector values))

(define (u64vector-length (v :: <u64vector>) <int>)
  (invoke v 'length))

(define (u64vector-ref (v :: <u64vector>) (i :: <int>))
  (invoke v 'get i))

(define (u64vector-set! (v :: <u64vector>) (i :: <int>) x)
  (invoke v 'setElementAt x i))

(define (u64vector->list (v :: <u64vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->u64vector (l :: <list>)) <u64vector>
  (make <u64vector> l))

(define (f32vector? x)
  (instance? x <f32vector>))

(define (make-f32vector (n :: <int>) #!optional (init :: <float> 0.0))
  (make <f32vector> n init))

(define (f32vector . values)
  (list->f32vector values))

(define (f32vector-length (v :: <f32vector>) <int>)
  (invoke v 'length))

(define (f32vector-ref (v :: <f32vector>) (i :: <int>))
  (invoke v 'floatValue i))

(define (f32vector-set! (v :: <f32vector>) (i :: <int>) (x :: <float>))
  (invoke v 'set i x))

(define (f32vector->list (v :: <f32vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->f32vector (l :: <list>)) <f32vector>
  (make <f32vector> l))

(define (f64vector? x)
  (instance? x <f64vector>))

(define (make-f64vector (n :: <int>) #!optional (init :: <double> 0.0))
  (make <f64vector> n init))

(define (f64vector . values)
  (list->f64vector values))

(define (f64vector-length (v :: <f64vector>) <int>)
  (invoke v 'length))

(define (f64vector-ref (v :: <f64vector>) (i :: <int>))
  (invoke v 'doubleValue i))

(define (f64vector-set! (v :: <f64vector>) (i :: <int>) x)
  (invoke v 'setElementAt x i))

(define (f64vector->list (v :: <f64vector>)) <list>
  (invoke-static <list> 'makeList v))

(define (list->f64vector (l :: <list>)) <f64vector>
  (make <f64vector> l))
