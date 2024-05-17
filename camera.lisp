;;;; camera.lisp

(defclass camera (object)
  ((focused-player
    :initarg :player
    :initform (error "Must specify a player object to focus on")
    :accessor focused-player
    :type object
    :documentation "The player to focus the camera on")))

(defun make-camera (player width height &key (xpos 0) (ypos 0))
  "Create and return a new instance of camera"
  (make-instance 'camera :player player :width width :height height
			 :xpos xpos :ypos ypos))

(defmethod draw-object ((cam camera) (obj object))
  (when (colliding-p obj cam)
    (draw-obj obj cam)))

(defmethod focus-camera ((cam camera))
  "refocus the gamera on the focused player."
  (with-accessors ((plyer focused-player) (w width) (h height)) cam
      (setf (xpos cam) (- (mid-x plyer) (/ w 2.0))
	    (ypos cam) (- (mid-y plyer) (/ h 2.0)))))

(defmethod draw-obj ((cam camera) (camr camera))
  "Draw a debug boundary for the camera to test stuff"
  (with-accessors ((x xpos) (y ypos) (pic img) (w width) (h height)) cam
    (with-pen (make-pen :stroke +red+ :weight 3)
      (rect x y w h))))
  
