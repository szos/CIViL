
(in-package :civ-tiles)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binding Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun basic-bind-tiles (tile1 tile1-direction tile2 tile2-direction)
  "creates a basic sytax for binding two tiles together. tile1 binds tile2 to the direction specified, and vice
versa for tile2 and tile2-direction. By specifying directions for each tile traditional text adventure mazes can 
be constructed"
  (case tile1-direction
    (xpos (setf (xpos tile1) tile2))
    (wpos (setf (wpos tile1) tile2))
    (vpos (setf (vpos tile1) tile2))
    (xneg (setf (xneg tile1) tile2))
    (wneg (setf (wneg tile1) tile2))
    (vneg (setf (vneg tile1) tile2)))
  (case tile2-direction
    (xpos (setf (xpos tile2) tile1))
    (wpos (setf (wpos tile2) tile1))
    (vpos (setf (vpos tile2) tile1))
    (xneg (setf (xneg tile2) tile1))
    (wneg (setf (wneg tile2) tile1))
    (vneg (setf (vneg tile2) tile1))))

(defun bind-tiles (root-tile &key xpos xneg wpos wneg vpos vneg)
  "Takes a tile to bind to and a 0-6 directional key(s) which creates a relational binding between 
the tiles. For example if we call this on tile-a with :xpos tile-b, the xpositive of tile-a will 
point to tile-b, while the xnegative of tile-b points to tile-a. "
  (when xpos (basic-bind-tiles root-tile 'xpos xpos 'xneg))
  (when xneg (basic-bind-tiles root-tile 'xneg xneg 'xpos))
  (when wpos (basic-bind-tiles root-tile 'wpos wpos 'wneg))
  (when wneg (basic-bind-tiles root-tile 'wneg wneg 'wpos))
  (when vpos (basic-bind-tiles root-tile 'vpos vpos 'vneg))
  (when vneg (basic-bind-tiles root-tile 'vneg vneg 'vpos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Map Classes and Methods ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass map-plane ()
  ((root-tile :initarg :root-tile
	      :accessor root-tile
	      :initform (make-instance 'basic-tile))))
(defgeneric generate-map (map-instance radius-or-x-y-pair))

(defmethod generate-map ((map-instance (eql 'hex-map)) radius)
  (generate-map (make-instance 'hex-map) radius))
(defmethod generate-map ((map-instance (eql 'square-map)) x-y-pair)
  (generate-map (make-instance 'square-map)) x-y-pair)
(defmethod generate-map ((map-instance (eql 'cylindrical-map)) circumference-height-pair)
  (generate-map (make-instance 'cylindrical-map) circumference-height-pair))

;;;;;;;;;;;;;;;;;;;
;;; Square Maps ;;;
;;;;;;;;;;;;;;;;;;;

;;; known working function
(defun generate-square-map (x y)
  "generates a square map with a height of y and a width of x. Returns lowermost leftmost tile."
  (labels ((recursive-xpos-build (tile length)
	     (unless (= length 1)
	       (let ((new-tile (make-instance 'basic-tile))
		     (wneg (vneg tile))
		     (vneg (and (vneg tile)
				(xpos (vneg tile)))))
		 (bind-tiles new-tile :xneg tile :wneg wneg :vneg vneg)
		 (recursive-xpos-build new-tile (- length 1)))))
	   (recursive-y-build (tile length height &optional (flag t))
	     (recursive-xpos-build tile length)
	     (unless (= height 1)
	       ;; (recursive-xpos-build tile length)
	       (let ((new-tile (make-instance 'basic-tile)))
		 (if flag
		     (bind-tiles new-tile :wneg tile :vneg (xpos tile))
		     (bind-tiles new-tile :vneg tile))
		 (recursive-y-build new-tile length (- height 1) (not flag))))))
    (let ((root-tile (make-instance 'basic-tile)))
      (recursive-y-build root-tile x y)
      root-tile)))

(defclass square-map (map-plane) ())

(defmethod generate-map ((map-instance square-map) (xy-pair cons))
  (labels ((recursive-xpos-build (tile length)
	     (unless (= length 1)
	       (let ((new-tile (make-instance 'basic-tile))
		     (wneg (vneg tile))
		     (vneg (and (vneg tile)
				(xpos (vneg tile)))))
		 (bind-tiles new-tile :xneg tile :wneg wneg :vneg vneg)
		 (recursive-xpos-build new-tile (- length 1)))))
	   (recursive-y-build (tile length height &optional (flag t))
	     (recursive-xpos-build tile length)
	     (unless (= height 1)
	       ;; (recursive-xpos-build tile length)
	       (let ((new-tile (make-instance 'basic-tile)))
		 (if flag
		     (bind-tiles new-tile :wneg tile :vneg (xpos tile))
		     (bind-tiles new-tile :vneg tile))
		 (recursive-y-build new-tile length (- height 1) (not flag))))))
    (recursive-y-build (root-tile map-instance) (car xy-pair) (cdr xy-pair)))
  map-instance)

(defclass cylindrical-map (square-map) ())

(defmethod generate-map ((map-instance cylindrical-map) (circumference-height-pair cons))
  (labels ((bind-final-xpos-to-root (tile root inner-offset-flag)
             (cond ((xpos tile)
		    (bind-final-xpos-to-root (xpos tile) root inner-offset-flag))
		   (inner-offset-flag ; (or (vpos root) (wneg root))
		    (bind-tiles tile :xpos root :wpos (vpos root) :vneg (wneg root)))
		   (t
		    (bind-tiles tile :xpos root))))
	   (bind-all-rows (root-tile &optional (f nil f-provided-p))
	     (let ((offset? (if f-provided-p f (vpos root-tile))))
	       (bind-final-xpos-to-root root-tile root-tile offset?)
               (when (or (vpos root-tile) (wpos root-tile))
		 (bind-all-rows (if offset? (vpos root-tile) (wpos root-tile))
				(not offset?))))))
    (bind-all-rows (root-tile (call-next-method)))
    map-instance))

;; (defun make-square-map-to-cylinder (root-tile)
;;   "takes the lower left of a square map and converts it to a cylinder around the x axis. "
;;   (labels ((bind-final-xpos-to-root (tile root inner-offset-flag)
;;              (cond ((xpos tile)
;; 		    (bind-final-xpos-to-root (xpos tile) root inner-offset-flag))
;; 		   (inner-offset-flag ; (or (vpos root) (wneg root))
;; 		    (bind-tiles tile :xpos root :wpos (vpos root) :vneg (wneg root)))
;; 		   (t
;; 		    (bind-tiles tile :xpos root))))
;; 	   (bind-all-rows (root-tile &optional (f nil f-provided-p))
;; 	     (let ((offset? (if f-provided-p f (vpos root-tile))))
;; 	       (bind-final-xpos-to-root root-tile root-tile offset?)
;;                (when (or (vpos root-tile) (wpos root-tile))
;; 		 (bind-all-rows (if offset? (vpos root-tile) (wpos root-tile))
;; 				(not offset?))))))
;;     (bind-all-rows root-tile)
;;     root-tile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HEXAGONAL CELL MAPS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hex-map (map-plane)
  ((tbl :initarg :map-table
	:accessor map-table
	:initform (make-hash-table :test 'equalp))))

(defun generate-hex-map (radius)
  (when (= radius 0) (return-from generate-hex-map))
  (labels ((genall (r tbl)
	     (loop for x from (- 0 r) to r
		   do (loop for y from (- 0 r) to r
			    do (loop for z from (- 0 r) to r
			     	     when (and (valid-coordinates x y z)
			     		       (not (gethash (list x y z) tbl)))
			     	       do (setf (gethash (list x y z) tbl)
						(make-instance 'basic-tile :x x :y y :z z)))))))
    (let ((map (make-instance 'hex-map :map-table (make-hash-table :test 'equalp :size (number-of-tiles radius)))))
      (genall (- radius 1) (map-table map))
      map)))

(defmethod coordinates ((tile %tile))
  (with-slots (x y z) tile
    (list x y z)))

(defun basic-move (coords dir &optional (number-of-tiles 1))
  (case dir
    ((xpos east)
     `(,(+ (car coords) number-of-tiles) ,(- (cadr coords) number-of-tiles) ,(caddr coords)))
    ((xneg west)
     `(,(- (car coords) number-of-tiles) ,(+ (cadr coords) number-of-tiles) ,(caddr coords)))
    ((vpos northwest)
     `(,(car coords) ,(+ (cadr coords) number-of-tiles) ,(- (caddr coords) NUMBER-OF-TILES)))
    ((vneg southeast)
     `(,(car coords) ,(- (cadr coords) number-of-tiles) ,(+ (caddr coords) NUMBER-OF-TILES)))
    ((wpos northeast)
     `(,(+ (car coords) number-of-tiles) ,(cadr coords) ,(- (caddr coords) number-of-tiles)))
    ((wneg southwest)
     `(,(- (car coords) number-of-tiles) ,(cadr coords) ,(+ (caddr coords) number-of-tiles)))))

;; 3n^2-3n+1

(defmethod number-of-tiles ((radius number))
  (- (* 3 (expt radius 2)) (* 3 radius) -1) )

(defmacro traversal-binder (tile radius dir+ dir- &key first final)
  (alexandria:with-gensyms (t1 t2 r ignore tvar1 tvar2 tvar3)
    `(traverse ,tile ,radius ',dir+ ',dir-
	       (lambda (,t1 ,r &rest ,ignore) 
		 (declare (ignore ,ignore))
		 ,@(when first
		     `((bind-tiles ,t1 ,(make-kword dir+) (make-instance 'basic-tile))))
		 ,(if final
		      `(unless (= ,r ,radius)
			 (traverse ,t1 ,r ',(rotate-cl dir+) ',(rotate-cl dir-)
				   (lambda (,t2 &rest ,ignore)
				     (declare (ignore ,ignore))
				     (bind-tiles ,t2 ,(make-kword (rotate-cl dir+)) (make-instance 'basic-tile))
				     (when-let ((,tvar1 (,dir+ ,t2)))
				       (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir+)) ,tvar1))
				     (when-let ((,tvar2 (,(rotate-cc dir-) ,t2)))
				       (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword dir-) ,tvar2)
				       (when-let ((,tvar3 (,(rotate-cl dir+) ,tvar2)))
					 (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir-)) ,tvar3))))))
		      `(traverse ,t1 ,r ',(rotate-cl dir+) ',(rotate-cl dir-)
				 (lambda (,t2 &rest ,ignore)
				   (declare (ignore ,ignore))
				   (bind-tiles ,t2 ,(make-kword (rotate-cl dir+)) (make-instance 'basic-tile))
				   (when-let ((,tvar1 (,dir+ ,t2)))
				     (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir+)) ,tvar1))
				   (when-let ((,tvar2 (,(rotate-cc dir-) ,t2)))
				     (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword dir-) ,tvar2)
				     (when-let ((,tvar3 (,(rotate-cl dir+) ,tvar2)))
				       (bind-tiles (,(rotate-cl dir+) ,t2)
						   ,(make-kword (rotate-cc dir-)) ,tvar3))))))))))

(defmacro build-traversal-for-hex (tile radius d+ d-)
  `(progn (traversal-binder ,tile ,radius ,d+ ,d- :first t)
	  (traversal-binder ,tile ,radius
			    ,(rotate-cl d+)
			    ,(rotate-cl d-))
	  (traversal-binder ,tile ,radius
			    ,(rotate-cl (rotate-cl d+))
			    ,(rotate-cl (rotate-cl d-)))
	  (traversal-binder ,tile ,radius
			    ,(rotate-cl (rotate-cl (rotate-cl d+)))
			    ,(rotate-cl (rotate-cl (rotate-cl d-))))
	  (traversal-binder ,tile ,radius
			    ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl d+))))
			    ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl d-)))))
	  (traversal-binder ,tile ,radius
			    ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl (rotate-cl d+)))))
			    ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl (rotate-cl d-)))))
			    :final t)))
(defmacro build-hex (tile radius)
  `(build-traversal-for-hex ,tile ,radius xpos xneg))

(labels ((traverse (tile r inc dec &optional apply-to-tile-radius-inc-and-dec)
	     (when apply-to-tile-radius-inc-and-dec
	       (apply apply-to-tile-radius-inc-and-dec (list tile r inc dec)))
	     (cond ((or (= r 0) (not (funcall inc tile))) tile)
		   (t (traverse (funcall inc tile) (- r 1) inc dec apply-to-tile-radius-inc-and-dec)))))
  (defmethod generate-map ((map-instance hex-map) (radius integer))
    (if (or (= radius 0) (= radius 1))
	map-instance
	(let ((cr (max 0 (- radius 1))))
	  (build-hex (root-tile map-instance) cr)))
    map-instance))

(macrolet ((traversal-binder (tile radius dir+ dir- &key first final)
	     (alexandria:with-gensyms (t1 t2 r ignore tvar1 tvar2 tvar3)
	       `(traverse ,tile ,radius ',dir+ ',dir-
			  (lambda (,t1 ,r &rest ,ignore) 
			    (declare (ignore ,ignore))
			    ,@(when first
				`((bind-tiles ,t1 ,(make-kword dir+) (make-instance 'basic-tile))))
			    ,(if final
				 `(unless (= ,r ,radius)
				    (traverse ,t1 ,r ',(rotate-cl dir+) ',(rotate-cl dir-)
					      (lambda (,t2 &rest ,ignore)
						(declare (ignore ,ignore))
						(bind-tiles ,t2 ,(make-kword (rotate-cl dir+)) (make-instance 'basic-tile))
						(when-let ((,tvar1 (,dir+ ,t2)))
						  (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir+)) ,tvar1))
						(when-let ((,tvar2 (,(rotate-cc dir-) ,t2)))
						  (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword dir-) ,tvar2)
						  (when-let ((,tvar3 (,(rotate-cl dir+) ,tvar2)))
						    (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir-)) ,tvar3))))))
				 `(traverse ,t1 ,r ',(rotate-cl dir+) ',(rotate-cl dir-)
					    (lambda (,t2 &rest ,ignore)
					      (declare (ignore ,ignore))
					      (bind-tiles ,t2 ,(make-kword (rotate-cl dir+)) (make-instance 'basic-tile))
					      (when-let ((,tvar1 (,dir+ ,t2)))
						(bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir+)) ,tvar1))
					      (when-let ((,tvar2 (,(rotate-cc dir-) ,t2)))
						(bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword dir-) ,tvar2)
						(when-let ((,tvar3 (,(rotate-cl dir+) ,tvar2)))
						  (bind-tiles (,(rotate-cl dir+) ,t2)
							      ,(make-kword (rotate-cc dir-)) ,tvar3))))))))))
	   (build-traversal-for-hex (tile radius d+ d-)
	     `(progn (traversal-binder ,tile ,radius ,d+ ,d- :first t)
		     (traversal-binder ,tile ,radius
				       ,(rotate-cl d+)
				       ,(rotate-cl d-))
		     (traversal-binder ,tile ,radius
				       ,(rotate-cl (rotate-cl d+))
				       ,(rotate-cl (rotate-cl d-)))
		     (traversal-binder ,tile ,radius
				       ,(rotate-cl (rotate-cl (rotate-cl d+)))
				       ,(rotate-cl (rotate-cl (rotate-cl d-))))
		     (traversal-binder ,tile ,radius
				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl d+))))
				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl d-)))))
		     (traversal-binder ,tile ,radius
				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl (rotate-cl d+)))))
				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl (rotate-cl d-)))))
				       :final t)))
	   (build-hex (tile radius)
	     `(build-traversal-for-hex ,tile ,radius xpos xneg)))
  ;; (defmacro traversal-binder (tile radius dir+ dir- &key first final))
  ;; (defmacro build-traversal-for-hex (tile radius d+ d-))
  ;; (defmacro build-hex (tile radius))
  (labels ((traverse (tile r inc dec &optional apply-to-tile-radius-inc-and-dec)
	     (when apply-to-tile-radius-inc-and-dec
	       (apply apply-to-tile-radius-inc-and-dec (list tile r inc dec)))
	     (cond ((or (= r 0) (not (funcall inc tile))) tile)
		   (t (traverse (funcall inc tile) (- r 1) inc dec apply-to-tile-radius-inc-and-dec)))))
    (defmethod generate-map ((map-instance hex-map) (radius integer))
      (if (or (= radius 0) (= radius 1))
	  map-instance
	  (let ((cr (max 0 (- radius 1))))
	    (build-hex (root-tile map-instance) cr)))
      map-instance)))

;;; variant that works with the x y z coordinate system as well.
;; (defun gen-coords (dir tile)
;;   "returns the coordinates of the tile in direction `dir` from tile `tile`"
;;   (let ((x (x tile))
;; 	(y (y tile))
;; 	(z (z tile)))
;;     (case dir
;;       ((west xneg)
;;        `(:x ,(- x 1) :y ,(+ Y 1) :z ,z))
;;       ((east xpos)
;;        `(:x ,(+ x 1) :y ,(- Y 1) :z ,z))
;;       ((northwest vpos)
;;        `(:x ,x :y ,(+ y 1) :z ,(- z 1)))
;;       ((northeast wpos)
;;        `(:x ,(+ x 1) :y ,y :z ,(- z 1)))
;;       ((southwest wneg)
;;        `(:x ,(- x 1) :y ,y :z ,(+ 1 z)))
;;       ((southeast vneg)
;;        `(:x ,x :y ,(- y 1) :z ,(+ z 1)))
;;       (otherwise '(:x nil :y nil :z nil)))))

;; (macrolet ((traversal-binder (tile radius dir+ dir- &key first final)
;; 	     (alexandria:with-gensyms (t1 t2 r ignore tvar1 tvar2 tvar3)
;; 	       `(traverse ,tile ,radius ',dir+ ',dir-
;; 			  (lambda (,t1 ,r &rest ,ignore) 
;; 			    (declare (ignore ,ignore))
;; 			    ,@(when first
;; 				`((bind-tiles ,t1 ,(make-kword dir+) (apply 'make-instance 'basic-tile
;; 									    (gen-coords ',dir+ ,t1)))))
;; 			    ,(if final
;; 				 `(unless (= ,r ,radius)
;; 				    (traverse ,t1 ,r ',(rotate-cl dir+) ',(rotate-cl dir-)
;; 					      (lambda (,t2 &rest ,ignore)
;; 						(declare (ignore ,ignore))
;; 						(bind-tiles ,t2 ,(make-kword (rotate-cl dir+))
;; 							    (apply 'make-instance 'basic-tile
;; 								   (gen-coords ',(rotate-cl dir+) ,t2)))
;; 						(when-let ((,tvar1 (,dir+ ,t2)))
;; 						  (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir+)) ,tvar1))
;; 						(when-let ((,tvar2 (,(rotate-cc dir-) ,t2)))
;; 						  (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword dir-) ,tvar2)
;; 						  (when-let ((,tvar3 (,(rotate-cl dir+) ,tvar2)))
;; 						    (bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir-)) ,tvar3))))))
;; 				 `(traverse ,t1 ,r ',(rotate-cl dir+) ',(rotate-cl dir-)
;; 					    (lambda (,t2 &rest ,ignore)
;; 					      (declare (ignore ,ignore))
;; 					      (bind-tiles ,t2 ,(make-kword (rotate-cl dir+))
;; 							  (apply 'make-instance 'basic-tile
;; 								 (gen-coords ',(rotate-cl dir+) ,t2)))
;; 					      (when-let ((,tvar1 (,dir+ ,t2)))
;; 						(bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword (rotate-cc dir+)) ,tvar1))
;; 					      (when-let ((,tvar2 (,(rotate-cc dir-) ,t2)))
;; 						(bind-tiles (,(rotate-cl dir+) ,t2) ,(make-kword dir-) ,tvar2)
;; 						(when-let ((,tvar3 (,(rotate-cl dir+) ,tvar2)))
;; 						  (bind-tiles (,(rotate-cl dir+) ,t2)
;; 							      ,(make-kword (rotate-cc dir-)) ,tvar3))))))))))
;; 	   (build-traversal-for-hex (tile radius d+ d-)
;; 	     `(progn (traversal-binder ,tile ,radius ,d+ ,d- :first t)
;; 		     (traversal-binder ,tile ,radius
;; 				       ,(rotate-cl d+)
;; 				       ,(rotate-cl d-))
;; 		     (traversal-binder ,tile ,radius
;; 				       ,(rotate-cl (rotate-cl d+))
;; 				       ,(rotate-cl (rotate-cl d-)))
;; 		     (traversal-binder ,tile ,radius
;; 				       ,(rotate-cl (rotate-cl (rotate-cl d+)))
;; 				       ,(rotate-cl (rotate-cl (rotate-cl d-))))
;; 		     (traversal-binder ,tile ,radius
;; 				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl d+))))
;; 				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl d-)))))
;; 		     (traversal-binder ,tile ,radius
;; 				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl (rotate-cl d+)))))
;; 				       ,(rotate-cl (rotate-cl (rotate-cl (rotate-cl (rotate-cl d-)))))
;; 				       :final t)))
;; 	   (build-hex (tile radius)
;; 	     `(build-traversal-for-hex ,tile ,radius xpos xneg)))
;;   ;; (defmacro traversal-binder (tile radius dir+ dir- &key first final))
;;   ;; (defmacro build-traversal-for-hex (tile radius d+ d-))
;;   ;; (defmacro build-hex (tile radius))
;;   (labels ((traverse (tile r inc dec &optional apply-to-tile-radius-inc-and-dec)
;; 	     (when apply-to-tile-radius-inc-and-dec
;; 	       (apply apply-to-tile-radius-inc-and-dec (list tile r inc dec)))
;; 	     (cond ((or (= r 0) (not (funcall inc tile))) tile)
;; 		   (t (traverse (funcall inc tile) (- r 1) inc dec apply-to-tile-radius-inc-and-dec)))))
;;     (defmethod generate-map ((map-instance hex-map) (radius integer))
;;       (if (or (= radius 0) (= radius 1))
;; 	  map-instance
;; 	  (let ((cr (max 0 (- radius 1))))
;; 	    (build-hex (root-tile map-instance) cr)))
;;       map-instance)))

(defmethod find-tile-coordinates ((tile basic-tile) (x number) (y number) &optional z)
  (let ((lx (x tile))
	(ly (y tile))
	(lz (z tile)))
    (if )))

(defmethod move (tile direction ammount)
  (case direction
    (x (setf (x tile) (+ (x tile) ammount)
	     (z tile) (- (z tile) ammount)))
    (y )
    (z ) )
  (when (and x (not (or y z)))
    (or (and (plusp x) (setf )))
    (cond ((plusp x))))
  (cond ))

(defun gen-tile-map ()
  (let ((root-tile (make-instance 'basic-tile)))
    ))

(defmethod apply-to-every-tile ((map hex-map) f)
  ())

;; (defun gen-hex (radius)
;;   "generates a hexagonal map of radius radius. Returns the center tile. "
;;   (when (= radius 0) (return-from gen-hex nil))
;;   (labels ((traverse (tile r inc dec &optional apply-to-tile-radius-inc-and-dec)
;; 	     (when apply-to-tile-radius-inc-and-dec
;; 	       (apply apply-to-tile-radius-inc-and-dec (list tile r inc dec)))
;; 	     (cond ((or (= r 0) (not (funcall inc tile))) tile)
;; 		   (t (traverse (funcall inc tile) (- r 1) inc dec apply-to-tile-radius-inc-and-dec)))))
;;     (let ((root-tile (make-instance 'basic-tile))
;; 	  (computational-radius (max 0 (- radius 1))))
;;       (cond ((= radius 1) root-tile)
;; 	    (t (traverse root-tile computational-radius 'xpos 'xneg ; build the xpos/vneg triangle
;; 			 (lambda (tl r i d)
;; 			   (declare (ignorable tl r i d))
;; 			   (bind-tiles tl :xpos (make-instance 'basic-tile))
;; 			   (traverse tl r 'vneg 'vpos
;; 				     (lambda (tl r i d)
;; 				       (declare (ignorable tl r i d))
;; 				       (bind-tiles tl :vneg (make-instance 'basic-tile))
;; 				       (when (xpos tl) (bind-tiles (vneg tl) :wpos (xpos tl)))
;; 				       (when (wneg tl)
;; 					 (bind-tiles (vneg tl) :xneg (wneg tl))
;; 					 (when (vneg (wneg tl))
;; 					   (bind-tiles (vneg tl) :wneg (vneg (wneg tl)))))))))
;; 	       (traverse root-tile computational-radius 'vneg 'vpos ; build the vneg/wneg triangle
;; 			 (lambda (tl r i d)
;; 			   (declare (ignorable tl r i d))
;; 			   (traverse tl r 'wneg 'wpos
;; 				     (lambda (tl r i d)
;; 				       (declare (ignorable tl r i d))
;; 				       (bind-tiles tl :wneg (make-instance 'basic-tile))
;; 				       (when-let ((vngt (vneg tl)))
;; 					 (bind-tiles (wneg tl) :xpos vngt))
;; 				       (when-let ((xngt (xneg tl)))
;; 					 (bind-tiles (wneg tl) :vpos xngt)
;; 					 (when-let ((wxngt (wneg (xneg tl))))
;; 					   (bind-tiles (wneg tl) :xneg wxngt)))))))
;; 	       (traverse root-tile computational-radius 'wneg 'wpos ; build the wneg/xneg triangle
;; 			 (lambda (tl r i d)
;; 			   (declare (ignorable tl r i d))
;; 			   (traverse tl r 'xneg 'xpos
;; 				     (lambda (tl r i d)
;; 				       (declare (ignorable tl r i d))
;; 				       (bind-tiles tl :xneg (make-instance 'basic-tile))
;; 				       (when-let ((b (wneg tl)))
;; 					 (bind-tiles (xneg tl) :vneg b))
;; 				       (when-let ((v (vpos tl)))
;; 					 (bind-tiles (xneg tl) :wpos v)
;; 					 (when-let ((vw (xneg (vpos tl))))
;; 					   (bind-tiles (xneg tl) :vpos vw)))))))
;; 	       (traverse root-tile computational-radius 'xneg 'xpos ; build the xneg/vpos triangle
;; 			 (lambda (tl r i d)
;; 			   (declare (ignorable tl r i d))
;; 			   (traverse tl r 'vpos 'vneg
;; 				     (lambda (tl r i d)
;; 				       (declare (ignorable tl r i d))
;; 				       (bind-tiles tl :vpos (make-instance 'basic-tile))
;; 				       (when-let ((xn (xneg tl)))
;; 					 (bind-tiles (vpos tl) :wneg xn))
;; 				       (when-let ((wn (wpos tl)))
;; 					 (bind-tiles (vpos tl) :xpos wn)
;; 					 (when-let ((vpwp (vpos (wpos tl))))
;; 					   (bind-tiles (vpos tl) :wpos vpwp)))))))
;; 	       (traverse root-tile computational-radius 'vpos 'vneg ; build the vpos/wpos triangle
;; 			 (lambda (tl r i d)
;; 			   (declare (ignorable tl r i d))
;; 			   (traverse tl r 'wpos 'wneg
;; 				     (lambda (tl r i d)
;; 				       (declare (ignorable tl r i d))
;; 				       (bind-tiles tl :wpos (make-instance 'basic-tile))
;; 				       (when-let ((vp (vpos tl)))  (bind-tiles (wpos tl) :xneg vp))
;; 				       (when-let ((xp (xpos tl)))
;; 					 (bind-tiles (wpos tl) :vneg xp)
;; 					 (when-let ((wpxp (wpos (xpos tl))))
;; 					   (bind-tiles (wpos tl) :xpos wpxp)))))))
;; 	       (traverse root-tile computational-radius 'wpos 'wneg ; build the wpos/xpos triangle
;; 			 (lambda (tl r i d)                         ; we need special handling of the xpos skeleton
;; 			   (declare (ignorable tl r i d))
;; 			   (unless (= r computational-radius)
;; 			     (traverse tl r 'xpos 'xneg
;; 				       (lambda (tl r i d)
;; 					 (declare (ignorable tl r i d))
;; 					 (bind-tiles tl :xpos (make-instance 'basic-tile))
;; 					 (when-let ((wp (wpos tl)))  (bind-tiles (xpos tl) :vpos wp))
;; 					 (when-let ((vn (vneg tl)))
;; 					   (bind-tiles (xpos tl) :wneg vn)
;; 					   (when-let ((xpvn (xpos vn)))
;; 					     (bind-tiles (xpos tl) :vneg xpvn))))))))
;; 	       root-tile)))))


