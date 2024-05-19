;;;; player.lisp

(in-package #:kimpy)

(defclass player (object)
  ((collided
    :initform nil
    :accessor collided
    :documentation "Whether or not the player has been collided with, T or nil")
   (weapon
    :initarg :weapon
    :initform nil
    :accessor player-weapon
    :documentation "The players weapon - if nil then no weapon")))

(defun make-player (&key (w nil) (h nil) (x 0) (y 0) (xvel 0) (yvel 0) (img-path nil) (img-data-path nil))
  (let* ((anim
	   (when (and img-path img-data-path) (make-new-anim img-path img-data-path)))
	 (width  (if w w (frame-w (current-frame anim))))
	 (height (if h h (frame-h (current-frame anim)))))
    (make-instance 'player :xpos x :ypos y :width width :height height
			   :xvel xvel :yvel yvel :animation anim)))

(defmethod draw-obj ((obj player) (cam object) &key (pen *outline-pen*))
  (call-next-method)
  (when (player-weapon obj)
    (draw-obj (player-weapon obj) cam))
  (with-accessors ((x xpos) (y ypos) (pic img)
		   (w width) (h height)) obj
    (when (collided obj)
      (let ((draw-x (- x (xpos cam)))
	    (draw-y (- y (ypos cam))))
	(with-pen pen
	  (rect draw-x draw-y w h))))))

(defmethod update-player ((ply player))
  (update-anim (anim ply))
  (handle-move ply))

;;; Weapon stuff that the player holds - maybe seperate into another file
(defclass weapon (object)
  ((parent
    :initarg :parent
    :initform (error "must supply a parent object to attach weapon to")
    :accessor parent
    :documentation "The parent object to attach the weapon to")))

(defmethod draw-obj ((obj weapon) (cam object) &key (pen *default-pen*))
  (with-accessors ((parent parent) (w width) (h height)) obj
    (let* ((player-x (mid-x-rel parent cam)) (player-y (mid-y-rel parent cam))
	   (delta-x (- player-x (mouse-x *game-info*)))
	   (delta-y (- player-y (mouse-y *game-info*)))
	   (delta (if (or (= 0 delta-x) (= 0 delta-y))
		      0 (/ (float delta-y) (float delta-x)))))
      (with-translate (player-x player-y)
	(with-rotate ((degrees (atan delta-y delta-x)))
	  (with-pen pen
	    (rect 0 0 w h)))))))
