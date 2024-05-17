;;;; player.lisp

(in-package #:kimpy)

(defclass player (object)
  ((collided
    :initform nil
    :accessor collided
    :documentation "Whether or not the player has been collided with, T or nil")))

(defun make-player (&key (w nil) (h nil) (x 0) (y 0) (xvel 0) (yvel 0) (img-path nil))
  (let ((img
	  (when img-path (load-resource img-path))))
    (make-instance 'player :xpos x :ypos y :width w :height h
			   :xvel xvel :yvel yvel
			   :img img)))

(defmethod draw-obj ((obj player))
  (call-next-method)
  (with-accessors ((x xpos) (y ypos) (pic img)
		   (w width) (h height)) obj
    (when (collided obj)
      (with-pen (make-pen :stroke +red+ :weight 2)
	  (rect x y w h)))))
