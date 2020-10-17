
(in-package :civ-tiles)

(defclass city ()
  ((city-center :initarg :city-center
		:accessor city-center
		:documentation "the tile that the city was founded on")
   (city-tiles :initarg :city-tiles
	       :accessor city-tiles
	       :documentation "A list of tiles that are part of the city, alongside a 3 key 
direction for getting there from the city center. It is stored in the format of:
  (tile ((dir1 val) (dir2 val) (dir3 val)))")))


