;;; Copyright (c) 2015 Jamison Hope
;;; This is free software;  for terms and warranty disclaimer see ./COPYING.
;;;
;;; A library of functions related to 3D spatial rotations.
;;;
;;; Representation conversions:
;;; - quaternion <-> Rotation Matrix (DCM)
;;; - quaternion <-> axis/angle
;;; - quaternion <-> body frame (intrinsic) angle sets (Euler, Tait-Bryan)
;;; - quaternion <-> fixed frame (extrinsic) angle sets
;;;
;;; Rotation operations:
;;; - make-rotation-procedure q->(q->q) for rotating multiple vector
;;;   quaternions by the same rotation quaternion
;;; - rotate-vector for one-off rotations
;;;
;;; The rotation matrix representation uses a SRFI-25 multidimensional
;;; array.  As this is a concrete type, the matrix/quaternion
;;; functions are named `quaternion->rotation-matrix' and
;;; `rotation-matrix->quaternion'.
;;;
;;; The other representations are not backed by distinct types, but
;;; rather make use of multiple values to hold their parts.  The
;;; functions which decompose a rotation quaternion into the three
;;; angles of an angle set are named according to their type and the
;;; axes, e.g. `intrinsic-xyx' or `extrinsic-zxz'.  The functions
;;; which produce a quaternion from three angles are named
;;; `make-ANGLESET', as in `make-intrinsic-xyx' or
;;; `make-extrinsic-zxz'.
;;;
;;; For convenience, the intrinsic-rotation functions have `euler' and
;;; `tait-bryan' aliases: the six intrinsic-rotations with same first
;;; and third axes are euler, and the other six are tait-bryan.  So
;;; for example `euler-xyx' is an alias for `intrinsic-xyx', and
;;; `make-tait-bryan-xyz' is an alias for `make-intrinsic-xyz'.
;;;
;;; Also, because the Roll/Pitch/Yaw set is fairly common, `rpy' and
;;; `make-rpy' are provided as aliases for `extrinsic-xyz' and
;;; `make-extrinsic-xyz'.
;;;
;;; For Euler angle sets, the second angle will be in [0,pi], and if
;;; it is within *epsilon*==1.0E-12 of 0 or pi (such that the first
;;; and third axes are colinear), the first angle will be set to 0.
;;;
;;; For Tait-Bryan angle sets, the second angle will be in
;;; [-pi/2,pi/2].  As with the Euler angle sets, if the first and
;;; third axes are deemed to be colinear then the first angle will be
;;; set to 0.
;;;
;;; All of the functions which use quaternions as rotations normalize
;;; their input by calling unit-quaternion.

(module-static #t)
(module-name (kawa rotations))
(export
 ;; quaternion <-> rotation matrix
 quaternion->rotation-matrix rotation-matrix->quaternion
 ;; quaternion <-> axis/angle
 rotation-axis rotation-angle rotation-axis/angle make-axis/angle
 rotx roty rotz
 ;; quaternion -> intrinsic angle sets
 intrinsic-xyx intrinsic-xzx intrinsic-yxy
 intrinsic-yzy intrinsic-zxz intrinsic-zyz
 intrinsic-xyz intrinsic-xzy intrinsic-yxz
 intrinsic-yzx intrinsic-zxy intrinsic-zyx
 euler-xyx euler-xzx euler-yxy euler-yzy euler-zxz euler-zyz
 tait-bryan-xyz tait-bryan-xzy tait-bryan-yxz
 tait-bryan-yzx tait-bryan-zxy tait-bryan-zyx
 ;; quaternion <- intrinsic angle sets
 make-intrinsic-xyx make-intrinsic-xzx make-intrinsic-yxy
 make-intrinsic-yzy make-intrinsic-zxz make-intrinsic-zyz
 make-intrinsic-xyz make-intrinsic-xzy make-intrinsic-yxz
 make-intrinsic-yzx make-intrinsic-zxy make-intrinsic-zyx
 make-euler-xyx make-euler-xzx make-euler-yxy
 make-euler-yzy make-euler-zxz make-euler-zyz
 make-tait-bryan-xyz make-tait-bryan-xzy make-tait-bryan-yxz
 make-tait-bryan-yzx make-tait-bryan-zxy make-tait-bryan-zyx
 ;; quaternion -> extrinsic angle sets
 extrinsic-xyx extrinsic-xyz extrinsic-xzx
 extrinsic-xzy extrinsic-yxy extrinsic-yxz
 extrinsic-yzx extrinsic-yzy extrinsic-zxy
 extrinsic-zxz extrinsic-zyx extrinsic-zyz
 rpy
 ;; quaternion <- extrinsic angle sets
 make-extrinsic-xyx make-extrinsic-xyz make-extrinsic-xzx
 make-extrinsic-xzy make-extrinsic-yxy make-extrinsic-yxz
 make-extrinsic-yzx make-extrinsic-yzy make-extrinsic-zxy
 make-extrinsic-zxz make-extrinsic-zyx make-extrinsic-zyz
 make-rpy
 ;; applying rotations
 make-rotation-procedure rotate-vector)

(require <kawa.lib.prim_syntax>)
(import (kawa quaternions))

;; The limit below which an angle is treated as effectively zero.
(define-constant *epsilon* ::double 1E-12)

(define-alias M java.lang.Math)

;;; Rotation Matrix
;;; quaternion->rotation-matrix
;;; rotation-matrix->quaternion

;; Helper syntax to evaluate the elements of the rotation matrix from
;; the elements of the unit quaternion -- this is used by
;; quaternion->rotation-matrix and also by the angle set accessors.
(define-syntax u
  (syntax-rules ()
    ((_ x) (M:min (M:max x -1d0) 1d0))))
(define-syntax m
  (syntax-rules ()
    ((_ 0 0 r i j k) (u (- (+ (* r r) (* i i)) (+ (* j j) (* k k)))))
    ((_ 0 1 r i j k) (u (* 2 (- (* i j) (* r k)))))
    ((_ 0 2 r i j k) (u (* 2 (+ (* i k) (* r j)))))
    ((_ 1 0 r i j k) (u (* 2 (+ (* i j) (* r k)))))
    ((_ 1 1 r i j k) (u (- (+ (* r r) (* j j)) (+ (* i i) (* k k)))))
    ((_ 1 2 r i j k) (u (* 2 (- (* j k) (* r i)))))
    ((_ 2 0 r i j k) (u (* 2 (- (* i k) (* r j)))))
    ((_ 2 1 r i j k) (u (* 2 (+ (* r i) (* j k)))))
    ((_ 2 2 r i j k) (u (- (+ (* r r) (* k k)) (+ (* i i) (* j j)))))))

;; Convert a quaternion to a rotation matrix represented by a SRFI-25
;; array backed by an f64vector -- i.e. a matrix of doubles.  The
;; quaternion is first normalized via `unit-quaternion'.
(define (quaternion->rotation-matrix q::quaternion)
  ::gnu.lists.Array
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    (share-array
     (f64vector
      (m 0 0 r i j k)
      (m 0 1 r i j k)
      (m 0 2 r i j k)
      (m 1 0 r i j k)
      (m 1 1 r i j k)
      (m 1 2 r i j k)
      (m 2 0 r i j k)
      (m 2 1 r i j k)
      (m 2 2 r i j k))
     (shape 0 3 0 3)
     (lambda (i j) (+ (* 3 i) j)))))

;; Convert a rotation matrix to a rotation quaternion.
(define (rotation-matrix->quaternion m::gnu.lists.Array) ::quaternion
  (let ((m00 (array-ref m 0 0))
        (m11 (array-ref m 1 1))
        (m22 (array-ref m 2 2)))
    (let ((trace (+ m00 m11 m22 1)))
      (cond ((> trace *epsilon*)
             (let ((s (/ 1/2 (sqrt trace))))
               (make-rectangular
                (/ 1/4 s)
                (* s (- (array-ref m 2 1) (array-ref m 1 2)))
                (* s (- (array-ref m 0 2) (array-ref m 2 0)))
                (* s (- (array-ref m 1 0) (array-ref m 0 1))))))
            ((and (> m00 m11) (> m00 m22))
             (let ((s (* 2 (sqrt (+ 1 m00 (- m11) (- m22))))))
               (make-rectangular
                (/ (- (array-ref m 2 1) (array-ref m 1 2)) s)
                (* 1/4 s)
                (/ (+ (array-ref m 0 1) (array-ref m 1 0)) s)
                (/ (+ (array-ref m 0 2) (array-ref m 2 0)) s))))
            ((> m11 m22)
             (let ((s (* 2 (sqrt (+ 1 m11 (- m00) (- m22))))))
               (make-rectangular
                (/ (- (array-ref m 0 2) (array-ref m 2 0)) s)
                (/ (+ (array-ref m 0 1) (array-ref m 1 0)) s)
                (* 1/4 s)
                (/ (+ (array-ref m 1 2) (array-ref m 2 1)) s))))
            (else
             (let ((s (* 2 (sqrt (+ 1 m22 (- m00) (- m11))))))
               (make-rectangular
                (/ (- (array-ref m 1 0) (array-ref m 0 1)) s)
                (/ (+ (array-ref m 0 2) (array-ref m 2 0)) s)
                (/ (+ (array-ref m 1 2) (array-ref m 2 1)) s)
                (* 1/4 s))))))))

;;; Axis / Angle
;;; rotation-axis
;;; rotation-angle
;;; rotation-axis/angle
;;; make-axis/angle
;;; rotx roty rotz

;; Accessor for the axis of rotation of a quaternion.
(define (rotation-axis q::quaternion)
  (let ((u (unit-vector q)))
    (if (= 0 u) +i u)))

;; Accessor for the angle of rotation of a quaternion.
(define (rotation-angle q::quaternion) ::real
  (* 2 (atan (magnitude (vector-part q)) (real-part q))))

;; Return the axis and angle of a quaternion as multiple values.
(define (rotation-axis/angle q::quaternion)
  (values (rotation-axis q) (rotation-angle q)))

;; Construct a rotation quaternion from an axis and angle.  The axis
;; can either be a vector quaternion or three reals (x, y, and z).
(define-procedure make-axis/angle
  (lambda (axis::quaternion angle::real)
    name: 'make-axis/angle
    ::quaternion
    (let ((halfangle (/ angle 2)))
      (+ (cos halfangle)
         (* (unit-vector axis) (sin halfangle)))))
  (lambda (axis-x::real axis-y::real axis-z::real angle::real)
    name: 'make-axis/angle
    ::quaternion
    (make-axis/angle
     (make-rectangular 0 axis-x axis-y axis-z) angle)))

;; Construct a quaternion representing a rotation about the X-axis.
(define (rotx angle::real) ::quaternion
  (let ((halfangle (/ angle 2)))
    ;; (+ (cos halfangle) (* +i (sin halfangle)))
    (make-rectangular (cos halfangle) (sin halfangle))))

;; Construct a quaternion representing a rotation about the Y-axis.
(define (roty angle::real) ::quaternion
  (let ((halfangle (/ angle 2)))
    ;; (+ (cos halfangle) (* +j (sin halfangle)))
    (make-rectangular (cos halfangle) 0 (sin halfangle) 0)))

;; Construct a quaternion representing a rotation about the Z-axis.
(define (rotz angle::real) ::quaternion
  (let ((halfangle (/ angle 2)))
    ;; (+ (cos halfangle) (* +k (sin halfangle)))
    (make-rectangular (cos halfangle) 0 0 (sin halfangle))))

;;; Intrinsic Angle Sets (Euler and Tait-Bryan)
;;; intrinsic-xxx make-intrinsic-xxx
;;; euler-xxx make-euler-xxx
;;; tait-bryan-xxx make-tait-bryan-xxx

;; Euler sets (same first and third axis)

;; Decompose a rotation quaternion into XYX Euler angles as multiple
;; values.
(define (intrinsic-xyx q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; cb = m00
    (let ((beta (acos (m 0 0 r i j k)))) ; in [0,pi]
      ;; Note: with sb calculated as (hypot m01 m02) and therefore
      ;; sb >= 0,
      ;; (acos cb) == (atan sb cb) because (atan + _) is in [0,pi],
      ;; same range as (acos _)
      (if (or (< beta *epsilon*)
              (> beta (- M:PI *epsilon*)))
          ;; sin(beta) = 0; define alpha := 0, gamma=atan(-m12,m11)
          (values 0.0 beta (atan (- (m 1 2 r i j k)) (m 1 1 r i j k)))
          ;; otherwise alpha=atan(m10/sb,-m20/sb), gamma=atan(m01/sb,m02/sb)
          ;; but since sb >= 0 (see above), we can safely drop the /sb
          ;; without changing the atan results
          (values (atan (m 1 0 r i j k) (- (m 2 0 r i j k)))
                  beta
                  (atan (m 0 1 r i j k) (m 0 2 r i j k)))))))

;; Decompose a rotation quaternion into XZX Euler angles as multiple values.
(define (intrinsic-xzx q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; cb = m00
    (let ((beta (acos (m 0 0 r i j k)))) ; in [0,pi]
      ;; Note: with sb calculated as (hypot m10 m20) and therefore
      ;; sb >= 0,
      ;; (acos cb) == (atan sb cb) because (atan + _) is in [0,pi],
      ;; same range as (acos _)
      (if (or (< beta *epsilon*)
              (> beta (- M:PI *epsilon*)))
          ;; sin(beta) = 0; define alpha := 0, gamma=atan(m21,m22)
          (values 0.0 beta (atan (m 2 1 r i j k) (m 2 2 r i j k)))
          ;; otherwise alpha=atan(m20/sb,m10/sb), gamma=atan(m02/sb,-m01/sb)
          ;; but since sb >= 0 (see above), we can safely drop the /sb
          ;; without changing the atan results
          (values (atan (m 2 0 r i j k) (m 1 0 r i j k))
                  beta
                  (atan (m 0 2 r i j k) (- (m 0 1 r i j k))))))))

;; Decompose a rotation quaternion into YXY Euler angles as multiple
;; values.
(define (intrinsic-yxy q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; cb = m11
    (let ((beta (acos (m 1 1 r i j k)))) ; in [0,pi]
      ;; Note: with sb calculated as (hypot m01 m21) and therefore
      ;; sb >= 0,
      ;; (acos cb) == (atan sb cb) because (atan + _) is in [0,pi],
      ;; same range as (acos _)
      (if (or (< beta *epsilon*)
              (> beta (- M:PI *epsilon*)))
          ;; sin(beta) = 0; define alpha := 0, gamma=atan(m02,m00)
          (values 0.0 beta (atan (m 0 2 r i j k) (m 0 0 r i j k)))
          ;; otherwise alpha=atan(m01/sb,m21/sb), gamma=atan(m10/sb,-m12/sb)
          ;; but since sb >= 0 (see above), we can safely drop the /sb
          ;; without changing the atan results
          (values (atan (m 0 1 r i j k) (m 2 1 r i j k))
                  beta
                  (atan (m 1 0 r i j k) (- (m 1 2 r i j k))))))))

;; Decompose a rotation quaternion into YZY Euler angles as multiple
;; values.
(define (intrinsic-yzy q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; cb = m11
    (let ((beta (acos (m 1 1 r i j k)))) ; in [0,pi]
      ;; Note: with sb calculated as (hypot m10 m12) and therefore
      ;; sb >= 0,
      ;; (acos cb) == (atan sb cb) because (atan + _) is in [0,pi],
      ;; same range as (acos _)
      (if (or (< beta *epsilon*)
              (> beta (- M:PI *epsilon*)))
          ;; sin(beta) = 0; define alpha := 0, gamma=atan(-m20,m22)
          (values 0.0 beta (atan (- (m 2 0 r i j k)) (m 2 2 r i j k)))
          ;; otherwise alpha=atan(m21/sb,-m01/sb), gamma=atan(m12/sb,m10/sb)
          ;; but since sb >= 0 (see above), we can safely drop the /sb
          ;; without changing the atan results
          (values (atan (m 2 1 r i j k) (- (m 0 1 r i j k)))
                  beta
                  (atan (m 1 2 r i j k) (m 1 0 r i j k)))))))

;; Decompose a rotation quaternion into ZXZ Euler angles as multiple
;; values.
(define (intrinsic-zxz q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; cb = m22
    (let ((beta (acos (m 2 2 r i j k)))) ; in [0,pi]
      ;; Note: with sb calculated as (hypot m20 m21) and therefore
      ;; sb >= 0,
      ;; (acos cb) == (atan sb cb) because (atan + _) is in [0,pi],
      ;; same range as (acos _)
      (if (or (< beta *epsilon*)
              (> beta (- M:PI *epsilon*)))
          ;; sin(beta) = 0; define alpha := 0, gamma=atan(-m01,m00)
          (values 0.0 beta (atan (- (m 0 1 r i j k)) (m 0 0 r i j k)))
          ;; otherwise alpha=atan(m02/sb,-m12/sb), gamma=atan(m20/sb,m21/sb)
          ;; but since sb >= 0 (see above), we can safely drop the /sb
          ;; without changing the atan results
          (values (atan (m 0 2 r i j k) (- (m 1 2 r i j k)))
                  beta
                  (atan (m 2 0 r i j k) (m 2 1 r i j k)))))))

;; Decompose a rotation quaternion into ZYZ Euler angles as multiple
;; values.
(define (intrinsic-zyz q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; cb = m22
    (let ((beta (acos (m 2 2 r i j k)))) ; in [0,pi]
      ;; Note: with sb calculated as (hypot m02 m12) and therefore
      ;; sb >= 0,
      ;; (acos cb) == (atan sb cb) because (atan + _) is in [0,pi],
      ;; same range as (acos _)
      (if (or (< beta *epsilon*)
              (> beta (- M:PI *epsilon*)))
          ;; sin(beta) = 0; define alpha := 0, gamma=atan(m10,m11)
          (values 0.0 beta (atan (m 1 0 r i j k) (m 1 1 r i j k)))
          ;; otherwise alpha=atan(m12/sb,m02/sb), gamma=atan(m21/sb,-m20/sb)
          ;; but since sb >= 0 (see above), we can safely drop the /sb
          ;; without changing the atan results
          (values (atan (m 1 2 r i j k) (m 0 2 r i j k))
                  beta
                  (atan (m 2 1 r i j k) (- (m 2 0 r i j k))))))))

;; Alias these six intrinsic-aba decomposition functions as euler-aba
(define-alias euler-xyx intrinsic-xyx)
(define-alias euler-xzx intrinsic-xzx)
(define-alias euler-yxy intrinsic-yxy)
(define-alias euler-yzy intrinsic-yzy)
(define-alias euler-zxz intrinsic-zxz)
(define-alias euler-zyz intrinsic-zyz)

;; Tait-Bryan sets (distinct first and third axes)

;; Decompose a rotation quaternion into XYZ Tait-Bryan angles as
;; multiple values.
(define (intrinsic-xyz q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; sb = m02
    (let ((beta (asin (m 0 2 r i j k)))) ; in [-pi/2,pi/2]
      ;; Note: with cb calculated as (hypot m10 m11) and therefore
      ;; cb >= 0,
      ;; (asin sb) == (atan sb cb) because (atan _ +) is in [-pi/2,pi/2],
      ;; same range as (asin _)
      (if (> (abs beta) (- (/ M:PI 2) *epsilon*))
          ;; cos(beta) = 0; define alpha := 0, gamma=atan(m10,m11)
          (values 0.0 beta (atan (m 1 0 r i j k) (m 1 1 r i j k)))
          ;; otherwise alpha=atan(-m12/cb,m22/cb), gamma=atan(-m01/cb,m00/cb)
          ;; but since cb >= 0 (see above), we can safely drop the /cb
          ;; without changing the atan results
          (values (atan (- (m 1 2 r i j k)) (m 2 2 r i j k))
                  beta
                  (atan (- (m 0 1 r i j k)) (m 0 0 r i j k)))))))

;; Decompose a rotation quaternion into XZY Tait-Bryan angles as
;; multiple values.
(define (intrinsic-xzy q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; sb = -m01
    (let ((beta (asin (- (m 0 1 r i j k))))) ; in [-pi/2,pi/2]
      ;; Note: with cb calculated as (hypot m00 m02) and therefore
      ;; cb >= 0,
      ;; (asin sb) == (atan sb cb) because (atan _ +) is in [-pi/2,pi/2],
      ;; same range as (asin _)
      (if (> (abs beta) (- (/ M:PI 2) *epsilon*))
          ;; cos(beta) = 0; define alpha := 0, gamma=atan(-m20,m22)
          (values 0.0 beta (atan (- (m 2 0 r i j k)) (m 2 2 r i j k)))
          ;; otherwise alpha=atan(m21/cb,m11/cb), gamma=atan(m02/cb,m00/cb)
          ;; but since cb >= 0 (see above), we can safely drop the /cb
          ;; without changing the atan results
          (values (atan (m 2 1 r i j k) (m 1 1 r i j k))
                  beta
                  (atan (m 0 2 r i j k) (m 0 0 r i j k)))))))

;; Decompose a rotation quaternion into YXZ Tait-Bryan angles as
;; multiple values.
(define (intrinsic-yxz q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; sb = -m12
    (let ((beta (asin (- (m 0 1 r i j k))))) ; in [-pi/2,pi/2]
      ;; Note: with cb calculated as (hypot m10 m11) and therefore
      ;; cb >= 0,
      ;; (asin sb) == (atan sb cb) because (atan _ +) is in [-pi/2,pi/2],
      ;; same range as (asin _)
      (if (> (abs beta) (- (/ M:PI 2) *epsilon*))
          ;; cos(beta) = 0; define alpha := 0, gamma=atan(-m01,m00)
          (values 0.0 beta (atan (- (m 0 1 r i j k)) (m 0 0 r i j k)))
          ;; otherwise alpha=atan(m02/cb,m22/cb), gamma=atan(m10/cb,m11/cb)
          ;; but since cb >= 0 (see above), we can safely drop the /cb
          ;; without changing the atan results
          (values (atan (m 0 2 r i j k) (m 2 2 r i j k))
                  beta
                  (atan (m 1 0 r i j k) (m 1 1 r i j k)))))))

;; Decompose a rotation quaternion into YZX Tait-Bryan angles as
;; multiple values.
(define (intrinsic-yzx q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; sb = m10
    (let ((beta (asin (m 1 0 r i j k)))) ; in [-pi/2,pi/2]
      ;; Note: with cb calculated as (hypot m00 m20) and therefore
      ;; cb >= 0,
      ;; (asin sb) == (atan sb cb) because (atan _ +) is in [-pi/2,pi/2],
      ;; same range as (asin _)
      (if (> (abs beta) (- (/ M:PI 2) *epsilon*))
          ;; cos(beta) = 0; define alpha := 0, gamma=atan(m21,m22)
          (values 0.0 beta (atan (m 2 1 r i j k) (m 2 2 r i j k)))
          ;; otherwise alpha=atan(-m20/cb,m00/cb), gamma=atan(-m12/cb,m11/cb)
          ;; but since cb >= 0 (see above), we can safely drop the /cb
          ;; without changing the atan results
          (values (atan (- (m 2 0 r i j k)) (m 0 0 r i j k))
                  beta
                  (atan (- (m 1 2 r i j k)) (m 1 1 r i j k)))))))

;; Decompose a rotation quaternion into ZXY Tait-Bryan angles as
;; multiple values.
(define (intrinsic-zxy q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; sb = m21
    (let ((beta (asin (m 2 1 r i j k)))) ; in [-pi/2,pi/2]
      ;; Note: with cb calculated as (hypot m01 m11) and therefore
      ;; cb >= 0,
      ;; (asin sb) == (atan sb cb) because (atan _ +) is in [-pi/2,pi/2],
      ;; same range as (asin _)
      (if (> (abs beta) (- (/ M:PI 2) *epsilon*))
          ;; cos(beta) = 0; define alpha := 0, gamma=atan(m02,m00)
          (values 0.0 beta (atan (m 0 2 r i j k) (m 0 0 r i j k)))
          ;; otherwise alpha=atan(-m01/cb,m11/cb), gamma=atan(-m20/cb,m22/cb)
          ;; but since cb >= 0 (see above), we can safely drop the /cb
          ;; without changing the atan results
          (values (atan (- (m 0 1 r i j k)) (m 1 1 r i j k))
                  beta
                  (atan (- (m 2 0 r i j k)) (m 2 2 r i j k)))))))

;; Decompose a rotation quaternion into ZYX Tait-Bryan angles as
;; multiple values.
(define (intrinsic-zyx q::quaternion)
  (let* ((uq (unit-quaternion q))
         (r ::double (real-part uq))
         (i ::double (imag-part uq))
         (j ::double (jmag-part uq))
         (k ::double (kmag-part uq)))
    ;; sb = -m20
    (let ((beta (asin (- (m 2 0 r i j k))))) ; in [-pi/2,pi/2]
      ;; Note: with cb calculated as (hypot m00 m10) and therefore
      ;; cb >= 0,
      ;; (asin sb) == (atan sb cb) because (atan _ +) is in [-pi/2,pi/2],
      ;; same range as (asin _)
      (if (> (abs beta) (- (/ M:PI 2) *epsilon*))
          ;; cos(beta) = 0; define alpha := 0, gamma=atan(-m12,m11)
          (values 0.0 beta (atan (- (m 1 2 r i j k)) (m 1 1 r i j k)))
          ;; otherwise alpha=atan(m10/cb,m00/cb), gamma=atan(m21/cb,m22/cb)
          ;; but since cb >= 0 (see above), we can safely drop the /cb
          ;; without changing the atan results
          (values (atan (m 1 0 r i j k) (m 0 0 r i j k))
                  beta
                  (atan (m 2 1 r i j k) (m 2 2 r i j k)))))))

;; Alias these six intrinsic-abc decomposition functions as tait-bryan-abc
(define-alias tait-bryan-xyz intrinsic-xyz)
(define-alias tait-bryan-xzy intrinsic-xzy)
(define-alias tait-bryan-yxz intrinsic-yxz)
(define-alias tait-bryan-yzx intrinsic-yzx)
(define-alias tait-bryan-zxy intrinsic-zxy)
(define-alias tait-bryan-zyx intrinsic-zyx)

;; Macro to create function r->r->r->q which will construct a rotation
;; quaternion from three intrinsic-rotation angles.
(define-syntax (define-make-intrinsic form)
  (syntax-case form ()
    ((_ a b c)
     (let ((astr (symbol->string (syntax a)))
           (bstr (symbol->string (syntax b)))
           (cstr (symbol->string (syntax c)))
           (rot (lambda (axis)
                  (case axis
                    (("x") 'rotx)
                    (("y") 'roty)
                    (("z") 'rotz)
                    (else (error 'rot "axis" axis))))))
       (let ((fname (string->symbol
                     &{make-intrinsic-&[astr]&[bstr]&[cstr]}))
             (rota (rot astr))
             (rotb (rot bstr))
             (rotc (rot cstr)))
         #`(define (#,(datum->syntax form fname)
                    alpha::real beta::real gamma::real)
             ::quaternion
             (unit-quaternion
              (* (#,(datum->syntax form rota) alpha)
                 (#,(datum->syntax form rotb) beta)
                 (#,(datum->syntax form rotc) gamma)))))))))

;; The six Euler angle intrinsic-rotation constructors.
(define-make-intrinsic x y x)
(define-make-intrinsic x z x)
(define-make-intrinsic y x y)
(define-make-intrinsic y z y)
(define-make-intrinsic z x z)
(define-make-intrinsic z y z)

;; The six Tait-Bryan angle intrinsic-rotation constructors.
(define-make-intrinsic x y z)
(define-make-intrinsic x z y)
(define-make-intrinsic y x z)
(define-make-intrinsic y z x)
(define-make-intrinsic z x y)
(define-make-intrinsic z y x)

;; Alias the six Euler constructors.
(define-alias make-euler-xyx make-intrinsic-xyx)
(define-alias make-euler-xzx make-intrinsic-xzx)
(define-alias make-euler-yxy make-intrinsic-yxy)
(define-alias make-euler-yzy make-intrinsic-yzy)
(define-alias make-euler-zxz make-intrinsic-zxz)
(define-alias make-euler-zyz make-intrinsic-zyz)

;; Alias the six Tait-Bryan constructors.
(define-alias make-tait-bryan-xyz make-intrinsic-xyz)
(define-alias make-tait-bryan-xzy make-intrinsic-xzy)
(define-alias make-tait-bryan-yxz make-intrinsic-yxz)
(define-alias make-tait-bryan-yzx make-intrinsic-yzx)
(define-alias make-tait-bryan-zxy make-intrinsic-zxy)
(define-alias make-tait-bryan-zyx make-intrinsic-zyx)

;;; Extrinsic Angle Sets
;;; extrinsic-xxx make-extrinsic-xxx
;;; rpy make-rpy

;; Fixed frame rotation abc is the same as body frame rotation cba, so
;; rather than duplicate the logic to compute the angles, just call
;; the corresponding body frame (intrinsic) procedure and reverse the
;; order of its results.

;; Macro to create these q->values decomposition functions in terms of
;; their intrinsic-rotation counterparts.
(define-syntax (define-extrinsic form)
  (syntax-case form ()
    ((_ c b a)
     (let ((cstr (symbol->string (syntax c)))
           (bstr (symbol->string (syntax b)))
           (astr (symbol->string (syntax a))))
       (let ((exname (string->symbol
                      &{extrinsic-&[cstr]&[bstr]&[astr]}))
             (inname (string->symbol
                      &{intrinsic-&[astr]&[bstr]&[cstr]})))
         #`(define (#,(datum->syntax form exname) q::quaternion)
             (let-values (((alpha beta gamma)
                           (#,(datum->syntax form inname) q)))
               (values gamma beta alpha))))))))

;; The twelve extrinsic-angle decomposition functions.
(define-extrinsic x y x)
(define-extrinsic x y z)
(define-extrinsic x z x)
(define-extrinsic x z y)
(define-extrinsic y x y)
(define-extrinsic y x z)
(define-extrinsic y z x)
(define-extrinsic y z y)
(define-extrinsic z x y)
(define-extrinsic z x z)
(define-extrinsic z y x)
(define-extrinsic z y z)

;; Macro to create three-argument extrinsic-rotation constructor
;; functions.
(define-syntax (define-make-extrinsic form)
  (syntax-case form ()
    ((_ c b a)
     (let ((cstr (symbol->string (syntax c)))
           (bstr (symbol->string (syntax b)))
           (astr (symbol->string (syntax a)))
           (rot (lambda (axis)
                  (case axis
                    (("x") 'rotx)
                    (("y") 'roty)
                    (("z") 'rotz)
                    (else (error 'rot "axis" axis))))))
       (let ((fname (string->symbol
                     &{make-extrinsic-&[cstr]&[bstr]&[astr]}))
             (rotc (rot cstr))
             (rotb (rot bstr))
             (rota (rot astr)))
         #`(define (#,(datum->syntax form fname)
                    gamma::real beta::real alpha::real)
             ::quaternion
             (unit-quaternion
              (* (#,(datum->syntax form rota) alpha)
                 (#,(datum->syntax form rotb) beta)
                 (#,(datum->syntax form rotc) gamma)))))))))

;; The twelve extrinsic-angle constructor functions.
(define-make-extrinsic x y x)
(define-make-extrinsic x y z)
(define-make-extrinsic x z x)
(define-make-extrinsic x z y)
(define-make-extrinsic y x y)
(define-make-extrinsic y x z)
(define-make-extrinsic y z x)
(define-make-extrinsic y z y)
(define-make-extrinsic z x y)
(define-make-extrinsic z x z)
(define-make-extrinsic z y x)
(define-make-extrinsic z y z)

;; Alias the extrinsic-xyz functions as "rpy".
(define-alias rpy extrinsic-xyz)
(define-alias make-rpy make-extrinsic-xyz)

;;; Rotation operations:
;;; make-rotation-procedure
;;; rotate-vector

;; Construct a procedure which will rotate vector quaternions by the
;; given rotation quaternion.  The returned function closes over the
;; rotation quaternion and its conjugate (both are needed for the
;; rotation operation), so this can be more efficient than repeatedly
;; calling `rotate-vector' with the same rotation quaternion.
(define (make-rotation-procedure rotation::quaternion) ::procedure
  (let* ((uq (unit-quaternion rotation))
         (uq* (conjugate uq)))
    (lambda (vec::quaternion)
      (if (vector-quaternion? vec)
          (vector-part (* uq vec uq*))
          (error "vec must be vector quaternion")))))

;; Rotate a vector quaternion by a given rotation.
(define (rotate-vector rotation::quaternion vec::quaternion)
  ::quaternion
  (if (vector-quaternion? vec)
      (let* ((uq (unit-quaternion rotation))
             (uq* (conjugate uq)))
        (vector-part (* uq vec uq*)))
      (error 'rotate-vector "vec must be vector quaternion")))
