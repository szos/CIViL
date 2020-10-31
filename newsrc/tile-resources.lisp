(in-package :civil-tiles)

(defclass resource (%mixin)
  ())
(defmethod tile-contains :around ((thing (eql 'resource)) (tile resource)) t)
(defgeneric tile-resource (obj))
(defmethod tile-resource (obj))

(defmacro define-resource (name &optional (superclasses '(bonus-resource)) slots &rest options)
  `(progn (defclass ,name ,superclasses ,slots ,@options)
	  (defmethod tile-resource ((obj ,name)) ',name)))

(defgeneric satisfies-resource-placement-constraints (tile resource))

(defclass bonus-resource (resource) ())
(define-resource bananas (bonus-resource) ()) ; only found on rainforest tiles
(defmethod satisfies-resource-placement-constraints (tile (resource bananas))
  (equal (feature tile) 'rainforest))
(define-resource copper (bonus-resource) ()) ; found on any tile with hills
(defmethod satisfies-resource-placement-constraints (tile (resource copper))
  (equal (terrain-spec tile) 'hills))
(define-resource cattle (bonus-resource) ()) ; found on flat grassland tiles
(defmethod satisfies-resource-placement-constraints (tile (resource cattle))
  (and (equal (terrain-spec tile) 'null-terrain-spec) (equal (base-terrain tile) 'grassland)))
(define-resource crabs (bonus-resource) ()) ; found only on costal tiles
;; (defmethod satisfies-resource-placement-constraints (tile (resource )))
(define-resource deer (bonus-resource) ()) ; found on tundra tiles OR in woods
(defmethod satisfies-resource-placement-constraints (tile (resource deer))
  (or (terrainp tile 'tundra) (featurep tile 'woods)))
(define-resource fish (bonus-resource) ()) ; found on costal and reef tiles
;; (defmethod satisfies-resource-placement-constraints (tile (resource fish))
;;   (or ()))
(define-resource maize (bonus-resource) ()) ; found on grassland and plains tiles that are plain or contain floodplains
(defmethod satisfies-resource-placement-constraints (tile (resource maize))
  (let ((g/p (or (terrainp tile 'grassland) (terrainp tile 'plains)))
	(f (or (featurep tile 'floodplains) (featurep tile 'null-feature))))
    (and g/p f)))
(define-resource rice (bonus-resource) ()) ; found on flat grassland tiles and tiles with marshes
(defmethod satisfies-resource-placement-constraints (tile (resource rice))
  (or (and (terrainp tile 'grassland) (terrain-spec-p tile 'null-terrain-spec))
      (featurep tile 'marsh)))
(define-resource sheep (bonus-resource) ()) ; found on any hills tile that has no woods, except for snow.
(defmethod satisfies-resource-placement-constraints (tile (resource sheep))
  (and (not (terrainp tile 'snow))
       (and (terrain-spec-p tile 'hills)
	    (not (featurep tile 'woods)))))
(define-resource stone (bonus-resource) ()) ; found on grasslands
(defmethod satisfies-resource-placement-constraints (tile (resource stone))
  (terrainp tile 'grasslands))
(define-resource wheat (bonus-resource) ()) ; found on flat plains
(defmethod satisfies-resource-placement-constraints (tile (resource wheat))
  (and (terrainp tile 'plains) (equal (feature tile) 'null-feature)))

(defclass luxury-resource (resource) ())
(define-resource amber (luxury-resource) ()) ; found on woods, rainforest, and coast tiles
;; (defmethod satisfies-resource-placement-constraints (tile (resource amber))
;;   (or ))
(define-resource citrus (luxury-resource) ()) ; found on grasslands or plains tiles
(defmethod satisfies-resource-placement-constraints (tile (resource citrus))
  (or (terrainp tile 'grassland) (terrainp tile 'plains)))
(define-resource cloves (luxury-resource) ()) ; only obtainable through trading (??)
;; (defmethod satisfies-resource-placement-constraints (tile (resource cloves)))
(define-resource cocoa (luxury-resource) ()) ; found only on rainforest tiles
(defmethod satisfies-resource-placement-constraints (tile (resource cocoa))
  (featurep tile 'rainforest))
(define-resource coffee (luxury-resource) ()) ; found in wet places only, so we need to specify that with the root type
;; (defmethod satisfies-resource-placement-constraints (tile (resource coffee))
;;   )
(define-resource cosmetics (luxury-resource) ()) ; great person???
(define-resource cotton (luxury-resource) ()) ; found on «open terrain» on grasslands and plains, or on floodplains
;; (defmethod satisfies-resource-placement-constraints (tile (resource cotton)))
(define-resource diamonds (luxury-resource) ()) ; found on hills or rainforests
(defmethod satisfies-resource-placement-constraints (tile (resource diamonds))
  (or (terrain-spec-p tile 'hills) (featurep tile 'rainforest)))
(define-resource dyes (luxury-resource) ()) ; found in woods or rainforest
(defmethod satisfies-resource-placement-constraints (tile (resource dyes))
  (or (featurep tile 'woods) (featurep tile 'rainforest)))
(define-resource furs (luxury-resource) ()) ; found on tundra, or sometimes elsewhere if theres woods.
(defmethod satisfies-resource-placement-constraints (tile (resource furs))
  (or (terrainp tile 'tundra) (featurep tile 'woods)))
(define-resource gold-ore (luxury-resource) ()) ; found on desert and plains tiles with or without hills
(defmethod satisfies-resource-placement-constraints (tile (resource gold-ore))
  (let ((gp (or (terrainp tile 'desert) (terrainp tile 'plains)))
	(h (or (terrain-spec-p tile 'hills) (equal (terrain-spec tile) 'null-terrain-spec))))
    (and gp h)))
(define-resource gypsum (luxury-resource) ()) ; found on plains or rarely on desert with hills
(defmethod satisfies-resource-placement-constraints (tile (resource gypsum))
  (or (terrainp tile 'plains)
      (and (terrainp tile 'desert) (terrain-spec-p tile 'hills))))
(define-resource honey (luxury-resource) ()) ; found on grassland and plains
(defmethod satisfies-resource-placement-constraints (tile (resource honey))
  (or (terrainp tile 'grassland) (terrainp tile 'plains)))
(define-resource incense (luxury-resource) ()) ; found on flat plains or desert tiles
(defmethod satisfies-resource-placement-constraints (tile (resource incense))
  (or (terrainp tile 'desert)
      (and (terrainp tile 'plains) (equal (terrain-spec tile) 'null-terrain-spec))))
(define-resource ivory (luxury-resource) ()) ; found on plains and desert, also in woods and rainforest
(defmethod satisfies-resource-placement-constraints (tile (resource ivory))
  (or (terrainp tile 'plains)
      (terrainp tile 'desert)
      (featurep tile 'woods)
      (featurep tile 'rainforest)))
(define-resource jade (luxury-resource) ()) ; found on any flat land (not snow/tundra)
(defmethod satisfies-resource-placement-constraints (tile (resource jade))
  (and (not (or (terrainp tile 'snow) (terrainp tile 'tundra)))
       (equal (terrain-spec tile) 'null-terrain-spec)))
(define-resource jeans (luxury-resource) ()) ; great person ???
;; (defmethod satisfies-resource-placement-constraints (tile (resource jeans)))
(define-resource marble (luxury-resource) ()) ; found on grassland and plains with hills
(defmethod satisfies-resource-placement-constraints (tile (resource marble))
  (and (terrain-spec-p tile 'hills)
       (or (terrainp tile 'grassland) (terrainp tile 'plains))))
(define-resource mercury (luxury-resource) ()) ; found on open plains
(defmethod satisfies-resource-placement-constraints (tile (resource mercury))
  (and (equal (terrain-spec tile) 'null-terrain-spec)
       (terrainp tile 'plains)))
(define-resource olives (luxury-resource) ()) ; found on grassland
(defmethod satisfies-resource-placement-constraints (tile (resource olives)) (terrainp tile 'grassland))
(define-resource pearls (luxury-resource) ()) ; found on costal water or fjords
;; (defmethod satisfies-resource-placement-constraints (tile (resource pearls)))
(define-resource perfume (luxury-resource) ()) ; great person???
(define-resource salt (luxury-resource) ()) ; found on flat terrain, mainly plains tundra desert
(define-resource silk (luxury-resource) ()) ; found in woods
(define-resource silver (luxury-resource) ()) ; found in desert and tundra tiles
(define-resource spices (luxury-resource) ()) ; SUPER RARE found in rainforest and woods
(define-resource sugar (luxury-resource) ()) ; Found in marshes, sometimes on floodplains
(define-resource tea (luxury-resource) ()) ; found on grassland tiles
(define-resource tobacco (luxury-resource) ()) ; found on grassland, plains, or in woods and rainforests
(define-resource toys (luxury-resource) ()) ; great person???
(define-resource truffles (luxury-resource) ()) ; found on woods rainforests or marshes
(define-resource turtles (luxury-resource) ()) ; found on reef tiles
(define-resource whales (luxury-resource) ()) ; found on costal tiles
(define-resource wine (luxury-resource) ()) ; found on grassland plains and woods. 

(defclass strategic-resource (resource) ())
(define-resource aluminum (strategic-resourc) ())  ; found on plains and desert
(define-resource coal (strategic-resourc) ()) ; found on hills
(define-resource horses (strategic-resourc) ()) ; found on flatland grassland and plains
(define-resource iron (strategic-resourc) ()) ; found on hills
(define-resource niter (strategic-resourc) ()) ; found on flatland
(define-resource oil (strategic-resourc) ()) ; found on land and at sea
(define-resource uranium (strategic-resourc) ()) ; found everywhere

(defun add-resource-to-tile (tile resource)
  (if (satisfies-resource-placement-constraints tile resource)
      (%mix tile resource)
      (error 'resource-placement-constraint-error tile resource)))

