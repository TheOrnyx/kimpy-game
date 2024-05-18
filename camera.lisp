;;;; camera.lisp

(in-package #:kimpy)

(defclass camera (object)
  ((focused-player
    :initarg :player
    :initform (error "Must specify a player object to focus on")
    :accessor focused-player
    :type object
    :documentation "The player to focus the camera on")
   (inner-bound
    :initarg :inner-bound
    :initform (error "Must provide an inner boundary object")
    :accessor inner-bound-obj
    :type object
    :documentation "The inner boundary object to use for testing player range")
   ))

(defun make-camera (player width height inner-scale &key (xpos 0) (ypos 0))
  "Create and return a new instance of camera"
  (make-instance 'camera :player player :width width :height height
			 :xpos xpos :ypos ypos
			 :inner-bound (make-instance 'object :width  (* width inner-scale)
							     :height (* height inner-scale)))) ; add X


(defmethod draw-object ((cam camera) (obj object))
  (when (colliding-p obj cam)
    (draw-obj obj cam)))

(defmethod focus-camera ((cam camera))
  "refocus the gamera on the focused player."
  (with-accessors ((plyer focused-player) (w width) (h height) (inner-obj inner-bound-obj)) cam
    (when t ;(not (colliding-p inner-obj plyer))
      (with-accessors ((inner-x xpos) (inner-y ypos) (inner-w width) (inner-h height)) inner-obj
	(setf (xpos cam) (- (mid-x plyer) (/ w 2.0))
	      (ypos cam) (- (mid-y plyer) (/ h 2.0))
	      inner-x (- (mid-x plyer) (/ inner-w 2.0))
	      inner-y (- (mid-y plyer) (/ inner-h 2.0))
	      )))))

(defmethod draw-obj ((cam camera) (camr object) &key (pen *outline-pen*))
  "Draw a debug boundary for the camera to test stuff"
  ;; (call-next-method)
  (draw-obj (inner-bound-obj cam) cam :pen pen))
  
