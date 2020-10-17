(defpackage #:tile-testing
  (:use :cl))

(in-package :tile-testing)

(define-condition invalid-coordinates-error (error)
  ((x :initarg :x :accessor x)
   (x :initarg :y :accessor y)
   (x :initarg :z :accessor z)))

(defun valid-coordinates (x y z)
  (unless (= 0 (+ x y z))
    (error 'invalid-coordinates-error :x x :y y :z z)))

(defun valid-direction (x y z)
  (or (and (= x 1) (= y -1) (= z 0))
      (and (= x 1) (= y 0) (= z -1))
      (and (= x 0) (= y 1) (= z -1))
      (and (= x -1) (= y 1) (= z 0))
      (and (= x -1) (= y 0) (= z 1))
      (and (= x 0) (= y -1) (= z 1))
      ;; (error 'invalid-direction-error :x :y :z)
      ))

(defclass tile ()
  ((x :initarg :x
      :accessor x
      :initform 0)
   (y :initarg :y
      :accessor y
      :initform 0)
   (z :initarg :z
      :accessor z
      :initform 0)
   (perlin :initarg :perlin
	   :accessor perlin
	   :initform 0)))

(defun movement (tile dir)
  (let ((x  (x tile))
	(y  (y tile))
	(z  (z tile)))
    (case dir
      (west
       (aref *gameboard* (- x 1) (+ Y 1) z))
      (east
       (aref *gameboard* (+ x 1) (- Y 1) z))
      (northwest
       (aref *gameboard* x (+ y 1) (- z 1)))
      (northeast
       (aref *gameboard* (+ x 1) y (- z 1)))
      (southwest
       (aref *gameboard* (- x 1) y (+ 1 z)))
      (southeast
       (aref *gameboard* x (- y 1) (+ z 1))))))

(defgeneric distance-between-tiles (tile-a tile-b))
(defmethod distance-between-tiles ((a tile) (b tile))
  (distance-between-tiles (list (x a) (y a) (z a))
			  (list (x b) (y b) (z b))))
(defmethod distance-between-tiles ((a tile) (b cons))
  (distance-between-tiles (list (x a) (y a) (z a)) b))
(defmethod distance-between-tiles ((a cons) (b tile))
  (distance-between-tiles a (list (x b) (y b) (z b))))
(defmethod distance-between-tiles ((a cons) (b cons))
  (/ (+ (abs (- (car a) (car b)))
	(abs (- (cadr a) (cadr b)))
	(abs (- (caddr a) (caddr b))))
     2))

(defparameter *hash-table* (make-hash-table :test 'equalp))

(ql:quickload :black-tie)

(defmethod gennoise ((tile tile))
  (let ((x (x tile))
	(y (y tile))
	(z (z tile)))
    (* (black-tie:perlin-noise-sf (* x 10.1) (* y 10.1) (* z 10.1)) 0.6)))

(defmethod gennoise ((c cons))
  (let ((x (first c))
	(y (second c))
	(z (third c)))
    (* (black-tie:perlin-noise-sf (* x 10.1) (* y 10.1) (* z 10.1)) 0.6)))

(let ((max 0)
      (min 0))
  (defun generate-perlin (x y z seed)
    (let ((n (* (black-tie:perlin-noise-sf (* x 10.1) (* y 100.1) (* z 100.1)) 0.6)))
      (when (> n max) (setf max n))
      (when (< n min) (setf min n))
      n)))

(defun generate-hexagonal-map (radius seed )
  (labels ()
    (let ((root-tile (make-instance 'tile :x 0 :y 0 :z 0
				    :perlin 0.0)))
      (setf (gethash '(0 0 0)  *hash-table*) root-tile)
      )))

(defparameter *directions*
  '((1 -1 0)
    (1 0 -1)
    (0 1 -1)
    (-1 1 0)
    (-1 0 1)
    (0 -1 1)))

;; (defun make-board (radius)
;;   (make-array (list radius radius radius)))

;; (defparameter *gameboard* (make-board 5))
