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

(defun make-player (&key (w nil) (h nil) (x 0) (y 0) (xvel 0) (yvel 0) (img-path nil) (img-data-path nil)
		      (x-dir nil) (y-dir nil))
  (let* ((anim
	   (when (and img-path img-data-path) (make-new-anim img-path img-data-path)))
	 (width  (if w w (frame-w (current-frame anim))))
	 (height (if h h (frame-h (current-frame anim)))))
    (make-instance 'player :xpos x :ypos y :width width :height height
			   :xvel xvel :yvel yvel :animation anim :x-dir x-dir :y-dir y-dir)))

(defmethod draw-obj ((obj player) (cam object) &key (pen *outline-pen*))
  (call-next-method)
  (when (player-weapon obj)
    (draw-obj (player-weapon obj) cam))
  (with-accessors ((pic img) (w width) (h height)) obj
    (when (collided obj)
      (with-pen pen
	(rect (x-pos-rel obj cam) (y-pos-rel obj cam) w h)))))

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
  (with-accessors ((parent parent) (w width) (h height) (animation anim)) obj
    (let* ((player-x (right-x-rel parent cam)) (player-y (mid-y-rel parent cam))
	   (delta-x (- player-x (mouse-x *game-info*)))
	   (delta-y (- player-y (mouse-y *game-info*)))
	   (x-rot-offset (- 0 w)) (y-rot-offset (- 0 (/ h 2))))
      ;; (with-pen pen (rect player-x player-y w h))
      (with-translate (player-x player-y)
	(with-rotate ((degrees (atan delta-y delta-x)))
	  (if animation
	      (draw-anim-frame animation :x x-rot-offset :y y-rot-offset :w w :h h)
	      (with-pen pen
		(rect x-rot-offset y-rot-offset w h))))))))

(defun make-weapon (&key (w nil) (h nil) (img-path nil) (img-data-path nil) (parent nil))
  (let ((anim
	  (when (and img-path img-data-path) (make-new-anim img-path img-data-path))))
    (make-instance 'weapon :width w :height h :animation anim :parent parent)))
