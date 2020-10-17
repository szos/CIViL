;;;; tiles.lisp

(in-package :civ-tiles)

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

(let ((x 0))
  (defun gen-tile-id ()
    (prog1 x (incf x))))

(defclass %tile ()
  ((id :initarg :id
       :accessor tile-id
       :initform (gen-tile-id)
       :documentation "a unique ID number")
   (road-key :initarg :road-numerical-key
	     :accessor road-numerical-key
	     :initform 0
	     :documentation "A numerical key for tracking roads for every side. Mathematically similar to chmod numerical permissions")
   (xpos :initarg :xpos
	 :accessor xpos
	 :documentation "Tile to the x positive")
   (wpos :initarg :wpos
	 :accessor wpos
	 :documentation "Tile to the w positive")
   (vpos :initarg :vpos
	 :accessor vpos
	 :documentation "Tile to the v positive")
   (xneg :initarg :xneg
	 :accessor xneg
	 :documentation "Tile to the x negative")
   (wneg :initarg :wneg
	 :accessor wneg
	 :documentation "Tile to the w negative")
   (vneg :initarg :vneg
	 :accessor vneg
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

;;;;;;;;;;;;;;
;;; MIXINS ;;;
;;;;;;;;;;;;;;


;;;; we define a class which conatiains a list of all possible combos. we need a
;;;; way to track the mixins. the problem: we dont want to define two classes
;;;; with the same behavior (ex. grasslands+woods+hills and grasslands+hills+woods)
;;;; cause it will gunk up the class list. so how do we do this? we want a
;;;; validator and a database, so we can send in a list of a class and any
;;;; mixins, and it will return the appropriate class. lets write it:

;;;; ok, you know what, this is stupid. we should just use dynamic mixins.

(defparameter *allowed-mixins* '((grassland woods floodplains)
				 (plains woods floodplains rainforest)))

(defun add-tile-feature (tile-instance feature)
  (let ((allowable-features (cdr (assoc (features-key tile-instance) *allowed-mixins*))))
    (when (and (member feature allowable-features) (not (contains-feature tile-instance)))
      (apply 'dynamic-mixins:ensure-mix tile-instance feature))))

(defun rem-tile-feature (tile-instance feature)
  (when (contains-feature tile-instance)
    (apply 'dynamic-mixins:delete-from-mix tile-instance feature)))

(flet ((parse-opts (opts)
	 (loop for el in opts
	       if (and (listp el) (keywordp (car el)) (equal (car el) :documentation))
		 collect (cdr el) into documentation
	       else if (and (listp el) (keywordp (car el)) (equal (car el) :method))
		      collect el into methods
	       else collect el into rest
	       finally (return (values (car documentation) methods rest))))
       (parse-methods (method-list)
	 (loop for method in method-list
	       collect (destructuring-bind (i name qual/args args/bod &rest body) method
			 (declare (ignore i))
			 `(defmethod ,name ,(if (keywordp qual/args)
						qual/args
						(cond ((symbolp qual/args) (list (list qual/args name)))
						      ((symbolp (car qual/args))
						       (list (list (car qual/args) name)))
						      (t qual/args)))
			    ,(if (keywordp qual/args)
				 (cond ((symbolp args/bod) (list (list args/bod name)))
				       ((symbolp (car args/bod))
					(list (list (car args/bod) name)))
				       (t args/bod))
				 args/bod)
			    ,@body)))))
  (defmacro define-mixin (name (&key (food 0 food-provided-p) (production 0 production-provided-p)
				   (faith 0 faith-provided-p) (culture 0 culture-provided-p)
				   (science 0 science-provided-p) (gold 0 gold-provided-p)
				   (superclasses '(basic-tile-mixin)) slots)
			   &body options)
    (multiple-value-bind (class-doc methods others) (parse-opts options)
      (declare (ignorable others))
      `(progn
	 (defclass ,name ,superclasses ,slots
	   ,@(when class-doc `((:documentation ,@class-doc))))
	 ,@(parse-methods methods)
	 ,@(when food-provided-p
	     `((defmethod ,(symbol-concatenate '(food yield)) ((tile ,name))
		 (+ ,food (call-next-method)))))
	 ,@(when gold-provided-p
	     `((defmethod ,(symbol-concatenate '(gold yield)) ((tile ,name))
		 (+ ,gold (call-next-method)))))
	 ,@(when science-provided-p
	     `((defmethod ,(symbol-concatenate '(science yield)) ((tile ,name))
		 (+ ,science (call-next-method)))))
	 ,@(when culture-provided-p
	     `((defmethod ,(symbol-concatenate '(culture yield)) ((tile ,name))
		 (+ ,culture (call-next-method)))))
	 ,@(when faith-provided-p
	     `((defmethod ,(symbol-concatenate '(faith yield)) ((tile ,name))
		 (+ ,faith (call-next-method)))))
	 ,@(when production-provided-p
	     `((defmethod ,(symbol-concatenate '(production yield)) ((tile ,name))
		 (+ ,production (call-next-method))))))))
  (defmacro define-tile (name (&key (food 0 food-provided-p) (production 0 production-provided-p)
				 (faith 0 faith-provided-p) (culture 0 culture-provided-p)
				 (science 0 science-provided-p) (gold 0 gold-provided-p)
				 (superclasses '(basic-tile)) slots ;; (tile-type 'land)
				 (tile-variants '((hilly :production 1))))
			 &body options-and-mixins)
    (multiple-value-bind (class-doc methods others) (parse-opts options-and-mixins)
      (declare (ignorable others))
      `(progn
	 (defclass ,name ,superclasses ,slots
	   ,@(when class-doc `((:documentation ,@class-doc))))
	 ,(when tile-variants
	    `(progn ,@(loop for variant in tile-variants
			    collect (destructuring-bind (varname &key production food gold science culture faith
								   slots extra-superclasses (tile-features name))
					variant
				      `(progn (defclass ,(symbol-concatenate (list varname name)) 
						  ,(cons name extra-superclasses)
						,slots
						(:default-initargs :features-key ,tile-features)
						(:documentation ,(concatenate 'string "Hilly variant of "
									      (symbol-name name)
									      " tile.")))
					      ,@(when production
						  `((defmethod production-yield ((tile ,(symbol-concatenate
											 (list varname name))))
						      (+ ,production (call-next-method)))))
					      ,@(when gold
						  `((defmethod gold-yield ((tile ,(symbol-concatenate
										   (list varname name))))
						      (+ ,gold (call-next-method)))))
					      ,@(when food
						  `((defmethod food-yield ((tile ,(symbol-concatenate
										   (list varname name))))
						      (+ ,food (call-next-method)))))
					      ,@(when science
						  `((defmethod science-yield ((tile ,(symbol-concatenate
										      (list varname name))))
						      (+ ,science (call-next-method)))))
					      ,@(when culture
						  `((defmethod culture-yield ((tile ,(symbol-concatenate
										      (list varname name))))
						      (+ ,culture (call-next-method)))))
					      ,@(when faith
						  `((defmethod faith-yield ((tile ,(symbol-concatenate
										    (list varname name))))
						      (+ ,faith (call-next-method))))))))))
	 ,@(parse-methods methods)
	 ,@(when food-provided-p
	     `((defmethod ,(symbol-concatenate '(food yield)) ((tile ,name))
		 (+ ,food (call-next-method)))))
	 ,@(when gold-provided-p
	     `((defmethod ,(symbol-concatenate '(gold yield)) ((tile ,name))
		 (+ ,gold (call-next-method)))))
	 ,@(when science-provided-p
	     `((defmethod ,(symbol-concatenate '(science yield)) ((tile ,name))
		 (+ ,science (call-next-method)))))
	 ,@(when culture-provided-p
	     `((defmethod ,(symbol-concatenate '(culture yield)) ((tile ,name))
		 (+ ,culture (call-next-method)))))
	 ,@(when faith-provided-p
	     `((defmethod ,(symbol-concatenate '(faith yield)) ((tile ,name))
		 (+ ,faith (call-next-method)))))
	 ,@(when production-provided-p
	     `((defmethod ,(symbol-concatenate '(production yield)) ((tile ,name))
		 (+ ,production (call-next-method)))))
	 ,@(let ((a))
	     (prog1
		 (loop for o in others
		       do (push (if (equal (car o) 'def) (cadr o) (car o))
				a)
		       if (equal (car o) 'def)
			 collect `(define-mixin ,@(cdr o)))
	       (push (cons name a) *allowed-mixins*)))))))

(define-tile plains (:food 1 :production 1)
  (:documentation "grassland tile, increases food and production yield by 1 each")
  (:method test-method x (+ 1 (call-next-method)))
  (:method test-method :before x (+ 1 (call-next-method)))
  (def rainforest (:production 1) ; define the hills mixin, cause it hasnt been done before
    (:documentation "rainforest mixin") 
    (:method test-method (+ 1 (call-next-method))))
  (woods)) ; use the woods plugin, which has already been defined

;; (define-mixin (hills :production 1) (:documentation "hills"
;; 				     :method food-yield x (+ 2 (call-next-method))))
;; (define-mixin hills :production 1
;;   (:documentation "hills")
;;   (:method food-yield x (+ 2 (call-next-method))))

(defun remove-key (key list)
  (let ((x (cadr (member key list))))
    (values (if x (remove x (remove key list)) list)
	    x)))

(defun remove-keys (keys list)
  (let ((nlist (remove-key (car keys) list)))
    (if (and (cdr keys) nlist)
	(remkeys (cdr keys) nlist)
	nlist)))

(defun get-methods (list)
  (split-sequence:split-sequence :method (remove-key :documentation list)))

(defun generate-methods (list obj-type)
  (multiple-value-bind (methods-list documentation) (remove-key :documentation list)
    (declare (ignorable documentation))
    (values
     (loop for method in (remove nil (split-sequence:split-sequence :method methods-list))
	   collect (destructuring-bind (name qual/args args/bod1 &rest body) method
		     (if (keywordp qual/args)
			 (let ((args (cond ((symbolp args/bod1) `((,args/bod1 ,obj-type)))
					   ((symbolp (car args/bod1)) `((,(car args/bod1) ,obj-type)))
					   (t args/bod1))))
			   `(defmethod ,name ,qual/args ,args ,@body))
			 (let ((args (cond ((symbolp qual/args) `((,qual/args ,obj-type)))
					   ((symbolp (car qual/args)) `((,(car qual/args) ,obj-type)))
					   (t qual/args))))
			   `(defmethod ,name ,args ,args/bod1 ,@body)))))
     documentation)))
