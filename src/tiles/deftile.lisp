
(in-package :civ-tiles)

(defparameter *allowed-features* '()
  "an alist of allowable features per tile.")

(defun add-tile-feature (tile-instance feature)
  "add the specified feature to the tile. If a feature is already present, do nothing."
  (let ((allowable-features (cdr (assoc (features-key tile-instance) *allowed-features*))))
    (when (and (member feature allowable-features) (not (contains-feature tile-instance)))
      (apply 'dynamic-mixins:ensure-mix tile-instance feature))))

(defun rem-tile-feature (tile-instance feature)
  "remove the specified feature from the tile. If no feature is present, do nothing"
  (when (contains-feature tile-instance)
    (apply 'dynamic-mixins:delete-from-mix tile-instance feature)))

(flet ((parse-opts (opts)
	 (loop for el in opts
	       if (and (listp el) (keywordp (car el)) (equal (car el) :documentation))
		 collect (cdr el) into documentation
	       else if (and (listp el) (keywordp (car el)) (equal (car el) :method))
		      collect el into methods
	       else if (and (listp el) (keywordp (car el)) (equal (car el) :tile-features))
		      collect el into variants
	       else collect el into rest
	       finally (return (values (car documentation) methods variants rest))))
       (parse-methods (method-list)
	 (loop for method in method-list
	       collect (destructuring-bind (i name qual/args args/bod &rest body) method
			 (declare (ignore i))
			 `(defmethod ,name ,(if (keywordp qual/args)
						qual/args
						(cond ((symbolp qual/args)
						       (list (list qual/args name)))
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
				  (superclasses '(basic-tile-mixin)) slots
				  )
			  &body options)
    (multiple-value-bind (class-doc methods variants others) (parse-opts options)
      (declare (ignorable variants others))
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
				 appeal adjacent-appeal defense  
				 (tile-variants '((hilly :production 1 :defense 3))))
			 &body options-and-mixins)
    (multiple-value-bind (class-doc methods variant-features others) (parse-opts options-and-mixins)
      (declare (ignorable variant-features others))
      `(progn
	 (defclass ,name ,superclasses ,slots
	   ,@(when class-doc `((:documentation ,@class-doc))))
	 ,(when tile-variants
	    `(progn ,@(loop for variant in tile-variants
			    collect (destructuring-bind (varname &key production food gold science culture faith
								   appeal adjacent-appeal defense slots
								   extra-superclasses (tile-features name))
					variant
				      (when variant-features
					(push (cons (symbol-concatenate (list varname name))
						    variant-features)
					      *allowed-features*))
				      `(progn (defclass ,(symbol-concatenate (list varname name)) 
						  ,(cons name extra-superclasses)
						,slots
						(:default-initargs :features-key ,tile-features)
						(:documentation ,(concatenate 'string "Hilly variant of "
									      (symbol-name name)
									      " tile, provides 1 production")))
					      ,@(when defense
						  `((defmethod defense-bonus
							((tile ,(symbol-concatenate (list varname name))))
						      (+ ,defense (call-next-method)))))
					      ,@(when appeal
						  `((defmethod tile-appeal
							((tile ,(symbol-concatenate (list varname name))))
						      (+ ,appeal (call-next-method)))))
					      ,@(when adjacent-appeal
						  `((defmethod adjacent-tiles-appeal-bonus
							((tile ,(symbol-concatenate (list varname name))))
						      (+ ,appeal (call-next-method)))))
					      ,@(when appeal
						  `((defmethod tile-appeal
							((tile ,(symbol-concatenate (list varname name))))
						      (+ ,appeal (call-next-method)))))
					      ,@(when adjacent-appeal
						  `((defmethod adjacent-tile-appeal-bonus
							((tile ,(symbol-concatenate (list varname name))))
						      (+ ,adjacent-appeal (call-next-method)))))
					      ,@(When production
						  `((defmethod production-yield
							((tile ,(symbol-concatenate (list varname name))))
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
		       when (equal (car o) 'def)
			 collect `(define-mixin ,@(cdr o)))
	       (push (cons name a) *allowed-features*)))))))

;; (define-tile grassland (:food 2)
;;   (:documentation "grassland tile, increases food by 2")
;;   (:tile-features hilly (woods))
;;   (def woods (:production 1)
;;     (:documentation "woods mixin"))
;;   (def marsh (:food 1
;; 		    ;; :defense-penalty -2
;; 		    ;; :adjacent-appeal -2
;; 		    ))
;;   (def floodplains (:food 3)))

;; (defmacro test-args (n (&optional (y 1) &key x)  &body bod)
;;   `(progn
;;      (print ,n)
;;      (print ,x)
;;      (print ,y)
;;      (print ',bod)))

;; (test-args n (:x 2) x y)
