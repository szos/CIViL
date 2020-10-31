(defpackage #:civil-tiles
  (:use :cl))

;; (rdclass cube ((box ((linex () ((x :accessor x-line)))
;; 		     (liney () ((y :accessor y-line)))))
;; 	       (linez () ((z :accessor z-line)))))
;; =>
;; (progn (defclass linez () ((z :accessor z-line)))
;;        (defclass liney () ((y :accessor y-line)))
;;        (defclass linex () ((x :accessor x-line)))
;;        (defclass box (linex liney) ())
;;        (defclass cube (box linez) ()))

(defun parse-class (class)
  ;; (labels ((gendef (c)
  ;; 	     `(defclass ,(car name) ,))))
  (destructuring-bind (name superclasses &optional slots &rest options) class
    (declare (ignorable name slots options))
    (loop for class in superclasses
	  if (symbolp class)
	    collect class into supers
	  else ; if (listp class)
	  collect (car class) into supers
	  collect (parse-class class)
	    into super-defs
	  finally (return (values supers super-defs)))))

;; (destructuring-bind (name superclases &optional slots &rest options)
;;     '(box ((linex () ((x :accessor x-line)))
;; 	   (liney () ((y :accessor y-line)))))
;;   (loop for class in superclasses
;; 	if (symbolp class)
;; 	  collect class into supers
;; 	else ; if (listp class)
;; 	collect (car class) into supers
;;         collect (parse-class class) into super-defs
;; 	finally (values supers super-defs)))

(defmacro defclass-and-superclasses (name superclasses slots &body options)
  (let ((x (gensym)))
    `(let ((,x nil))
       (labels ((ps (x)
		  (when x
		    (destructuring-bind (name supers slots) x
                      
		      (loop for )))))))))

;; yield = gloabl-mod-mixin<--wonder-mixins<--leader-mixin<--resource<--feature<--terrain-specifier<--base-terrain(<--type)

;; every class in the calculation train has two slots holding lists of whats acceptable above and below that
;; stop in the train. so a terrain specifier will have a list of acceptable base terrains, and a list of acceptable
;; features. Some elements might need to look down further, ie resources might need to look down to base terrain.
;; This might mean we have a diamond problem:

;;             base-terrain
;;            /     |    \
;;terrain-spec      |     feature
;;            \     |    /
;;             \    |   /
;;              resource
       
;; we can solve this by using our validators, ie (terrain-spec tile) returns the type of terrain spec present,
;; so we can write a validator for resources that checks the tile for whether or not it can take that type of tile

;; But we do want to do this with mixins instead of lists, we just have to create our own destructuring-bind for
;; mixins... we want to use mixins cause it allows total freedom with how to calculate the value for that stage
;; of calculation... and allow customization with :around/:before/:after methods.

;; (defmacro define-tile-mixin ((mixin-category type superclasses &optional slots &rest options)
;; 			     ))

(defclass coordinate-tile ()
  ((x :initarg :x
      :accessor x-coordinate
      :initform 0)
   (y :initarg :y
      :accessor y-coordinate
      :initform 0)
   (z :initarg :z
      :accessor z-coordinate
      :initform 0)))

(defgeneric print-tile (tile &optional stream)
  (:documentation "Print tile to stream using format.")
  (:method :before (tile &optional stream)
    (format stream "("))
  (:method (tile &optional stream)
    (unless stream ""))
  (:method :after (tile &optional stream)
    (format stream ")")))

(defmacro define-print-tile ((type &key (tile-var 'tile) (stream-var 'stream) doc) format-string &body format-args)
  "defines a method to generate a printed representation of tile type. The string MUST be contained in parens."
  (let ((r (gensym)))
    `(defmethod print-tile ((,tile-var ,type) &optional ,stream-var)
       (let ((,r (format ,stream-var ,format-string ,@format-args)))
	 (if (and stream (next-method-p))
	     (call-next-method)
	     (concatenate 'string ,r (if (next-method-p) (call-next-method) "")))))))

(define-print-tile (coordinate-tile :documentation "generate printed representation of tile coordinates")
		   "(coordinate-tile (x ~a) (y ~a) (z ~a))"
  (x-coordinate tile) (y-coordinate tile) (z-coordinate tile))

(defgeneric print-tile-for-client (tile)
  (:documentation "return a printed representation of the tile suitable for sending to a client")
  (:method ((tile coordinate-tile))
    (format nil "(:coordinates x ~a y ~a z ~a) ~a" (x-coordinate tile) (y-coordinate tile) (z-coordinate tile)
	    (call-next-method)))
  (:method (tile) ""))


(defgeneric tile-contains (thing tile))
(defmethod tile-contains (thing tile) nil)

(defmacro defmixin (name superclasses slots &rest options)
  `(progn (defclass ,name ,superclasses ,slots ,@options)
	  (defmethod tile-contains :around ((thing (eql ',name)) (tile ,name)) t)))

(defclass %mixin () ())
(defmethod tile-contains :around ((thing (eql 'mixin)) (tile %mixin)) t)

;;;;;;;;;;;;;;;;;;;;
;;; Base Terrain ;;;
;;;;;;;;;;;;;;;;;;;;

