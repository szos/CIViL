(in-package :civil-tiles)

;;;;;;;;;;;;;;;;;;;;;;
;;; Tile Root Type ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;; we want to put wetness and warmth specifiers in the %tile class, which will be used to determine the type of tile

(defclass %tile ()
  ((warmth :initarg :warmth
	   :accessor warmth
	   :initform 0)))

(defclass land-tile (%tile)
  ((wetness-min :initarg :wetness-min
		:accessor wetness-min
		:documentation "The minimum wetness for this tile type"
		:initform 0)
   (wetness-max :initarg :wetness-max
		:accessor wetness-max
		:documentation "The maximum wetness for this tile type"
		:initform 0)))

(defclass ocean-tile (%tile) ())

(defgeneric tile-root-type (tile))
(defmethod tile-root-type (tile) nil)

(defmacro define-tile-root (name &optional (superclasses '(land-tile)) slots
			    &body options)
  `(progn (defclass ,name ,superclasses ,slots ,@options)
	  (defmethod tile-root-type ((tile ,name)) ',name)
	  (define-print-tile (,name) "(tile-root ~a (superclasses ~a) ~a (warmth ~a) ~a)"
	    ',name ',superclasses ,(if slots
				       `(format nil "(slots ~a)" slots)
				       "")
	    (warmth tile)
	    ,(if (member 'land-tile superclasses)
		 `(format nil "(wetness (min ~a) (max ~a))" (wetness-min tile) (wetness-max tile))
		 ""))))

;;;;;;;;;;;;;;;;;;
;;; Land Tiles ;;;
;;;;;;;;;;;;;;;;;;

(define-tile-root polar (land-tile) ()                      ; coldest, y=8, warmth = 0
  (:default-initargs :warmth 0 :wetness-min 43 :wetness-max 57)) 

(define-tile-root tundra (land-tile) ()                     ; colder, y=7, warmth = 0
  (:default-initargs :warmth 1 :wetness-min 35 :wetness-max 65))

(define-tile-root boreal-forest (land-tile) ()              ; cold, y=6
  (:default-initargs :warmth 2 :wetness-min 25 :wetness-max 75))

(define-tile-root cold-desert (land-tile) ()                ; warm, y=4-5
  (:default-initargs :warmth 3 :wetness-min 12 :wetness-max 37))

(define-tile-root prairie (land-tile) ()                    ; warm, y=4-5
  (:default-initargs :warmth 3 :wetness-min 37 :wetness-max 62))

(define-tile-root temperate-deciduous-forest (land-tile) () ; warm, y=4-5
  (:default-initargs :warmth 3 :wetness-min 62 :wetness-max 88))

(define-tile-root warm-desert (land-tile) ()                ; hot, y=0-3
  (:default-initargs :warmth 4 :wetness-min 0 :wetness-max 22))

(define-tile-root tropical-grassland (land-tile) ()         ; hot, y=0-3
  (:default-initargs :warmth 4 :wetness-min 22 :wetness-max 43))

(define-tile-root savanna (land-tile) ()                    ; hot, y=0-3
  (:default-initargs :warmth 4 :wetness-min 43 :wetness-max 58))

(define-tile-root tropical-deciduous-forest (land-tile) ()  ; hot, y=0-3
  (:default-initargs :warmth 4 :wetness-min 58 :wetness-max 73))

(define-tile-root tropical-rainforest (land-tile) ()        ; hot, y=0-3
  (:default-initargs :warmth 4 :wetness-min 73 :wetness-max 100))

;;;;;;;;;;;;;;;;;;;
;;; Ocean Tiles ;;;
;;;;;;;;;;;;;;;;;;;

(define-tile-root open-ocean (ocean-tile) ()
  (:default-initargs :warmth 2))

(define-tile-root coastal-waters (ocean-tile) ())

(define-tile-root estuary (costal-waters) ())


