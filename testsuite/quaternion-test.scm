(test-begin "quaternion")

(import (kawa quaternions))

(test-equal 0 (+ +i+j+k -i-j-k))
(test-equal -1 (* +i +i))
(test-equal -1 (* +j +j))
(test-equal -1 (* +k +k))
(test-equal -1 (* +i +j +k))

(test-equal 1+i+j+k (+ 1 +i +j +k))
(test-equal 1-i-j-k (- 1 +i +j +k))

(test-equal 1 (/ 1+i+j+k 1+i+j+k))

(test-equal +k (* (/ -i) +j))
(test-equal -k (* +j (/ -i)))

(test-eqv #t (quaternion? 0))
(test-eqv #t (quaternion? -i))
(test-eqv #t (quaternion? 1+2i-3j+4k))
(test-eqv #f (quaternion? 10.0m))
(test-eqv #f (quaternion? "x"))
(test-eqv #t (quaternion? (java.lang.Double:valueOf 5.5)))

(test-begin "real-part")
(test-equal 0 (real-part 0))
(test-equal 0 (real-part -i))
(test-equal 1 (real-part 1+2i-3j+4k))
(test-end "real-part")

(test-begin "imag-part")
(test-equal 0 (imag-part 0))
(test-equal -1 (imag-part -i))
(test-equal 2 (imag-part 1+2i-3j+4k))
(test-end "imag-part")

(test-begin "jmag-part")
(test-equal 0 (jmag-part 0))
(test-equal 0 (jmag-part -i))
(test-equal -3 (jmag-part 1+2i-3j+4k))
(test-end "jmag-part")

(test-begin "kmag-part")
(test-equal 0 (kmag-part 0))
(test-equal 0 (kmag-part -i))
(test-equal 4 (kmag-part 1+2i-3j+4k))
(test-end "kmag-part")

(test-begin "vector-part")
(test-equal 0 (vector-part 0))
(test-equal -i (vector-part -i))
(test-equal +2i-3j+4k (vector-part 1+2i-3j+4k))
(test-end "vector-part")

(test-begin "unit-vector")
(test-equal 0 (unit-vector 0))
(test-equal -i (unit-vector -i))
(test-equal (* (/ (sqrt 3)) +i+j+k) (unit-vector 3+3i+3j+3k))
(test-equal 1.0 (magnitude (unit-vector 1+2i+3j+4k)))
(test-end "unit-vector")

(test-begin "vector-quaternion?")
(test-eqv #t (vector-quaternion? 0))
(test-eqv #t (vector-quaternion? +i+j+k))
(test-eqv #f (vector-quaternion? 1-i))
(test-end "vector-quaternion?")

(test-begin "make-vector-quaternion")
(test-equal +i+2j+3k (make-vector-quaternion 1 2 3))
(test-end "make-vector-quaternion")

(test-begin "vector-quaternion->list")
(test-equal '(1 2 3) (vector-quaternion->list +i+2j+3k))
(test-end "vector-quaternion->list")

(test-begin "unit-quaternion")
(test-equal 0 (unit-quaternion 0))
(test-equal +i (unit-quaternion 0.0+0.00001i+0.0j+0.0k))
(test-assert (= 1/2+1/2i+1/2j+1/2k (unit-quaternion 1+i+j+k)))
(test-end "unit-quaternion")

(test-begin "magnitude")
(test-eqv 5.0 (magnitude 3+4i))
(test-eqv 5.0 (magnitude 3-4j))
(test-eqv 5.0 (magnitude 4+3k))
(test-eqv 5.0 (magnitude +3i-4j))
(test-eqv 5.0 (magnitude -4i+3k))
(test-eqv 5.0 (magnitude -3j-4k))
(test-eqv 5.0 (magnitude +5k))
(test-end "magnitude")

(test-begin "angle")
(test-eqv 1.5 (angle 3@1.5))
(test-eqv 1.5 (angle 3@1.5%0.2))
(test-eqv 1.5 (angle 3@1.5&-0.4))
(test-eqv 1.5 (angle 3@1.5%0.2&-0.4))
(test-assert (= (/ java.lang.Math:PI 2) (angle +i)))
(test-end "angle")

(test-begin "colatitude")
(test-eqv 0 (colatitude 3@1.5))
(test-eqv 0.2 (colatitude 3@1.5%0.2))
(test-eqv 0 (colatitude 3@1.5&-0.4))
(test-eqv 0.2 (colatitude 3@1.5%0.2&-0.4))
(test-end "colatitude")

(test-begin "longitude")
(test-eqv 0 (longitude 3@1.5))
(test-eqv 0.0 (longitude 3@1.5%0.2))
(test-eqv 0 (longitude 3@1.5&-0.4))
(test-eqv -0.4 (longitude 3@1.5%0.2&-0.4))
(test-end "longitude")

(test-begin "make-rectangular")
(test-equal 1+2i+3j+4k (make-rectangular 1 2 3 4))
(test-equal 1.5-2i (make-rectangular 1.5 -2))
(test-equal 1.5-2i (make-rectangular 1.5 -2 0 0))
(test-end "make-rectangular")

(test-begin "make-polar")
(test-equal 1@1.5 (make-polar 1 1.5))
(test-equal 1@1.5 (make-polar 1 1.5 0 0))
(test-equal 1@1.5 (make-polar 1 1.5 0 0.3))
(test-equal 1@1.5%-0.2 (make-polar 1 1.5 -0.2 0))
(test-equal 1@1.5%-0.2&0.4 (make-polar 1 1.5 -0.2 0.4))
(test-end "make-polar")

(test-begin "dot-product")
(test-error #t (dot-product 1+2i+3j+4k +i+j+k))
(test-equal (- (real-part (* +i+j+k +2i-3j-4k))) (dot-product +i+j+k +2i-3j-4k))
(test-equal -5 (dot-product +i+j+k +2i-3j-4k))
(test-equal 0 (dot-product +i +j+k))
(test-end "dot-product")

(test-begin "cross-product")
(test-equal +k (cross-product +i +j))
(test-equal -3i+6j-3k (cross-product +i+2j+3k +4i+5j+6k))
(test-equal 0 (cross-product -i-j-k +3i+3j+3k))
(test-end "cross-product")

(test-begin "conjugate")
(test-equal 1-i (conjugate 1+i))
(test-equal 1+2i-3j+4k (conjugate 1-2i+3j-4k))
(test-end "conjugate")

(test-begin "expt")
(test-eqv #t (real-valued? (expt +i +i)))
(test-equal (expt +i +i) (expt +j +j))
(test-equal (expt +i +i) (expt +k +k))
(test-assert (= (java.lang.Math:exp (/ (- java.lang.Math:PI) 2))
                (expt +i +i)))
(test-end "expt")

(test-begin "sqrt")
(test-approximate (make-rectangular (/ (sqrt 2)) 0 0 (/ (sqrt 2)))
                  (sqrt +k)
                  0.0000000001+0.0000000001i+0.0000000001j+0.0000000001k)
(test-end "sqrt")

(import (kawa rotations))

(test-begin "rotation-matrix")
(let* ((q 1/2+1/2i+1/2j+1/2k) ; 120 degrees about (1,1,1)
       (m1 (quaternion->rotation-matrix q))
       (m2 (quaternion->rotation-matrix (* q q))) ; 240 degrees
       (m3 (quaternion->rotation-matrix (* q q q)))) ; 360 degrees
  ;; for a 120-degree rotation about (1,1,1),  +X->+Y, +Y->+Z, +Z->+X
  ;; m1 is #2a((0 0 1) (1 0 0) (0 1 0))
  (test-equal '(0.0 1.0 0.0) (list (m1 0 0) (m1 1 0) (m1 2 0))) ; col 0
  (test-equal '(0.0 0.0 1.0) (list (m1 0 1) (m1 1 1) (m1 2 1))) ; col 1
  (test-equal '(1.0 0.0 0.0) (list (m1 0 2) (m1 1 2) (m1 2 2))) ; col 2
  ;; for 240 degrees, +X->+Z, +Y->+X, +Z->+Y
  (test-equal '(0.0 0.0 1.0) (list (m2 0 0) (m2 1 0) (m2 2 0))) ; col 0
  (test-equal '(1.0 0.0 0.0) (list (m2 0 1) (m2 1 1) (m2 2 1))) ; col 1
  (test-equal '(0.0 1.0 0.0) (list (m2 0 2) (m2 1 2) (m2 2 2))) ; col 2
  ;; for 360 degrees, matrix is identity -- but we're on the other
  ;; side of the hypersphere: q^3 == -1
  (test-equal -1 (* q q q))
  (test-equal '(1.0 0.0 0.0) (list (m3 0 0) (m3 1 0) (m3 2 0))) ; col 0
  (test-equal '(0.0 1.0 0.0) (list (m3 0 1) (m3 1 1) (m3 2 1))) ; col 1
  (test-equal '(0.0 0.0 1.0) (list (m3 0 2) (m3 1 2) (m3 2 2)))) ; col 2
(test-end "rotation-matrix")

(test-begin "rotation-axis/angle")
(test-equal +i (rotation-axis (rotx 0.2)))
(test-equal +i (rotation-axis 1))
(test-equal 0.0 (rotation-angle 1))
(test-approximate 0.2 (rotation-angle (rotx 0.2)) 0.0000000001)
(test-approximate (rotx 0.2) (make-axis/angle 1 0 0 0.2) 0.0000000001+0.0000000001i)
(test-approximate (roty 0.3) (make-axis/angle 0 1 0 0.3) 0.0000000001+0.0000000001j)
(test-approximate (rotz 0.4) (make-axis/angle 0 0 1 0.4) 0.0000000001+0.0000000001k)
(test-end "rotation-axis/angle")

(test-begin "angle-sets")
(for-each
 (lambda (f)
   (test-equal 1 (f 0 0 0)))
 (list
  make-intrinsic-xyx make-intrinsic-xzx make-intrinsic-yxy
  make-intrinsic-yzy make-intrinsic-zxz make-intrinsic-zyz
  make-intrinsic-xyz make-intrinsic-xzy make-intrinsic-yxz
  make-intrinsic-yzx make-intrinsic-zxy make-intrinsic-zyx
  make-extrinsic-xyx make-extrinsic-xyz make-extrinsic-xzx
  make-extrinsic-xzy make-extrinsic-yxy make-extrinsic-yxz
  make-extrinsic-yzx make-extrinsic-yzy make-extrinsic-zxy
  make-extrinsic-zxz make-extrinsic-zyx make-extrinsic-zyz))

(let-values (((a b c) (rpy (make-rpy 0.1 -0.2 0.4))))
  (test-approximate  0.1 a 0.0000000001)
  (test-approximate -0.2 b 0.0000000001)
  (test-approximate  0.4 c 0.0000000001))
(test-approximate (make-euler-xyx 0.1 0.2 0.3)
                  (make-extrinsic-xyx 0.3 0.2 0.1)
                  0.0000000001+0.0000000001i+0.0000000001j+0.0000000001k)
(test-approximate (make-tait-bryan-yxz 0.1 0.2 0.3)
                  (make-extrinsic-zxy 0.3 0.2 0.1)
                  0.0000000001+0.0000000001i+0.0000000001j+0.0000000001k)
(for-each
 (lambda (f g)
   (let-values (((a b c) (f (g 1 0 2))))
     (test-approximate 0 a 1e-10)
     (test-approximate 0 b 1e-10)
     (test-approximate 3 c 1e-10)))
 (list euler-xyx euler-xzx euler-yxy euler-yzy euler-zxz euler-zyz)
 (list make-euler-xyx make-euler-xzx make-euler-yxy make-euler-yzy
       make-euler-zxz make-euler-zyz))
(test-end "angle-sets")

(test-begin "rotate-vector")
(let ((r (make-rotation-procedure 1/2+1/2i+1/2j+1/2k))
      (eps 0.0000000001+0.0000000001i+0.0000000001j+0.0000000001k))
  (test-approximate +j (r +i) eps)
  (test-approximate +k (r +j) eps)
  (test-approximate +i (r +k) eps)
  (test-approximate +j (rotate-vector (rotz (java.lang.Math:toRadians 90)) +i) eps)
  (let* ((a -0.105)
         (q (make-axis/angle +j a))
         (v (rotate-vector q (make-vector-quaternion 0 0 1))))
    (test-approximate (cos a) (imag-part v) eps)
    (test-equal 0.0 (jmag-part v))
    (test-approximate (sin a) (kmag-part v) eps)))
(test-end "rotate-vector")

(test-end)
