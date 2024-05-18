;;;; player.lisp

(in-package #:kimpy)

(defclass player (object)
  ((collided
    :initform nil
    :accessor collided
    :documentation "Whether or not the player has been collided with, T or nil")))

(defun make-player (&key (w nil) (h nil) (x 0) (y 0) (xvel 0) (yvel 0) (img-path nil) (img-data-path nil))
  (let* ((anim
	   (when (and img-path img-data-path) (make-new-anim img-path img-data-path)))
	 (width  (if w w (frame-w (current-frame anim))))
	 (height (if h h (frame-h (current-frame anim)))))
    (make-instance 'player :xpos x :ypos y :width width :height height
			   :xvel xvel :yvel yvel :animation anim)))

(defmethod draw-obj ((obj player) (cam object) &key (pen *outline-pen*))
  (call-next-method)
  (with-accessors ((x xpos) (y ypos) (pic img)
		   (w width) (h height)) obj
    (when (collided obj)
      (let ((draw-x (- x (xpos cam)))
	    (draw-y (- y (ypos cam))))
	(with-pen (make-pen :stroke +red+ :weight 2)
	  (rect draw-x draw-y w h))))))

(defmethod update-player ((ply player))
  (update-anim (anim ply))
)
