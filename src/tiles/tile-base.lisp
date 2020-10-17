
(in-package :civ-tiles)

(defmacro when-let ((&rest forms) &body body)
  `(let ,forms
     (when (and ,@(loop for v in forms collect (if (listp v) (car v) v)))
       ,@body)))

(defun symbol-concatenate (symbols)
  (intern (format nil "~{~a~^-~}" symbols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tile Directional Macros ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro east (tile) `(xpos ,tile))
(defmacro west (tile) `(xneg ,tile))
(defmacro northeast (tile) `(wpos ,tile))
(defmacro northwest (tile) `(vpos ,tile))
(defmacro southeast (tile) `(vneg ,tile))
(defmacro southwest (tile) `(wneg ,tile))

(defun make-kword (symbol)
  (values (intern (symbol-name symbol) "KEYWORD")))

(defun rotate-cl (sym)
  (case sym (xpos 'vneg) (vneg 'wneg) (wneg 'xneg) (xneg 'vpos) (vpos 'wpos) (wpos 'xpos)))
(defun rotate-cc (sym)
  (case sym (xpos 'wpos) (wpos 'vpos) (vpos 'xneg) (xneg 'wneg) (wneg 'vneg) (vneg 'xpos)))

(let ((x 0))
  (defun gen-tile-id ()
    (prog1 x (incf x)))
  (defun peek-tile-id () x))

(defclass %tile ()
  ((id :initarg :id
       :accessor tile-id
       :initform (gen-tile-id)
       :documentation "a unique ID number")
   (road-key :initarg :road-numerical-key
	     :accessor road-numerical-key
	     :initform 0
	     :documentation "A numerical key for tracking roads for every side. Mathematically similar to chmod numerical permissions")
   (x :initarg :x
      :accessor x
      :initform 0)
   (y :initarg :y
      :accessor y
      :initform 0)
   (z :initarg :z
      :accessor z
      :initform 0)
   (xpos :initarg :xpos
	 :accessor xpos
	 :initform nil
	 :documentation "Tile to the x positive")
   (wpos :initarg :wpos
	 :accessor wpos
	 :initform nil
	 :documentation "Tile to the w positive")
   (vpos :initarg :vpos
	 :accessor vpos
	 :initform nil
	 :documentation "Tile to the v positive")
   (xneg :initarg :xneg
	 :accessor xneg
	 :initform nil
	 :documentation "Tile to the x negative")
   (wneg :initarg :wneg
	 :accessor wneg
	 :initform nil
	 :documentation "Tile to the w negative")
   (vneg :initarg :vneg
	 :accessor vneg
	 :initform nil
	 :documentation "Tile to the v negative"))
  (:documentation "This is the basic tile class. all tiles must descend from this class. 
Must have a unique id. "))

(defun road-keys->number (&rest keys)
  "takes a list of keys and returns a number. "
  (apply #'+ (loop for key in keys collect (case key
					     ((:xpos) 1)
					     ((:wpos) 2)
					     ((:vpos) 4)
					     ((:xneg) 8)
					     ((:wneg) 16)
					     ((:vneg) 32)))))

(defun road-number->keys (n)
  (labels ((recursive-solution (x)
	     (cond ((> x 32) (cons :vneg (recursive-solution (- x 32))))
		   ((> x 16) (cons :wneg (recursive-solution (- x 16))))
		   ((> x 8)  (cons :xneg (recursive-solution (- x 8))))
		   ((> x 4)  (cons :vpos (recursive-solution (- x 4))))
		   ((> x 2)  (cons :wpos (recursive-solution (- x 2))))
		   ((> x 1)  (cons :xpos (recursive-solution (- x 1)))))))
    (recursive-solution n)))

(defun road-contains (road-number road-key)
  (member road-key (road-number->keys road-number)))

(defclass tile-base-yields (%tile)
  ((base-food :initarg :base-food
	      :allocation :class
	      :initform 0)
   (base-production :accessor base-production
		    :initarg :base-production
		    :allocation :class
		    :initform 0)
   (base-gold :accessor base-gold
	      :initarg :base-gold
	      :allocation :class
	      :initform 0)
   (base-faith :accessor base-faith
	       :initarg :base-faith
	       :allocation :class
	       :initform 0)
   (base-science :accessor base-science
		 :initarg :base-science
		 :allocation :class
		 :initform 0)
   (base-culture :accessor base-culture
		 :initarg :base-culture
		 :allocation :class
		 :initform 0)))

(defclass basic-tile (tile-base-yields)
  ((food :accessor food
	 :initarg :food
	 :initform 0)
   (production :accessor production
	       :initarg :production
	       :initform 0)
   (gold :accessor gold
	 :initarg :gold
	 :initform 0)
   (faith :accessor faith
	  :initarg :faith
	  :initform 0)
   (science :accessor science
	    :initarg :science
	    :initform 0)
   (culture :accessor culture
	    :initarg :culture
	    :initform 0)
   (allowed-features-key :initarg :features-key
			 :accessor features-key
			 :initform nil))
  (:documentation "basic tile class, contains the resources of the tile"))

(defmethod print-object ((obj basic-tile) s)
  (format s "#<BASIC-TILE: ~a>" (tile-id obj)))

;;;;;;;;;;;;;;
;;; MIXINS ;;;
;;;;;;;;;;;;;;

(defclass basic-tile-mixin () ())
(defclass feature-mixin (basic-tile-mixin) ())

(defgeneric contains-feature (tile)
  (:documentation "Check whether a feature is present in a tile. "))
(defmethod contains-feature ((tile feature-mixin)) t)
(defmethod contains-feature ((tile basic-tile)) nil)

(defgeneric yields (tile))
(defmethod yields ((tile basic-tile))
  (values (food-yield tile)
	  (production-yield tile)
	  (gold-yield tile)
	  (faith-yield tile)
	  (science-yield tile)
	  (culture-yield tile)))

(defmethod culture-yield ((tile basic-tile))
  (base-culture tile))

(defmethod science-yield ((tile basic-tile))
  (base-science tile))

(defmethod faith-yield ((tile basic-tile))
  (base-faith tile))

(defmethod gold-yield ((tile basic-tile))
  (base-gold tile))

(defmethod production-yield ((tile basic-tile))
  (base-production tile))

(defmethod food-yield ((tile basic-tile))
  (food tile))

;;; appeal bonuses

(defgeneric tile-appeal (tile))
(defgeneric adjacent-tiles-appeal-bonus (tile))

(defmethod adjacent-tiles-appeal-bonus ((tile basic-tile))
  (declare (ignore tile))
  0)
(defmethod tile-appeal ((tile basic-tile))
  (declare (ignore tile))
  0)
(defmethod tile-appeal :around ((tile basic-tile))
  (let ((surrounding-tiles (list (xpos tile) (wpos tile) (vpos tile)
				 (xneg tile) (wneg tile) (vneg tile))))
    (apply '+ (cons (call-next-method)
		    (loop for til in surrounding-tiles
			  collect (adjacent-tiles-appeal-bonus til))))))

;;; attack/defense bonuses

(defgeneric defense-bonus (tile))
(defmethod defense-bonus ((tile basic-tile))
  (declare (ignore tile))
  0)

(defgeneric attack-bonus (tile))
(defmethod attack-bonus ((tile basic-tile))
  (declare (ignore tile))
  0)
