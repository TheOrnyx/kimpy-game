;;;; object.lisp

(in-package #:kimpy)

(defparameter *default-pen* (make-pen :fill +red+ :stroke +black+ :weight 3)
  "The default pen for objects with no img")
(defparameter *outline-pen* (make-pen :stroke +blue+ :weight 3) "Default pen that just strokes")

(defclass object ()
  ((xpos
    :initarg :xpos
    :initform 0
    :accessor xpos
    :documentation "The objects x-position")
   (ypos
    :initarg :ypos
    :initform 0
    :accessor ypos
    :documentation "The objects y-position")
   (xvel
    :initarg :xvel
    :initform 0
    :accessor xvel
    :documentation "The objects x-velocity")
   (yvel
    :initarg :yvel
    :initform 0
    :accessor yvel
    :documentation "The objects y-velocity")
   (width
    :initarg :width
    :initform (error ":width must be specified")
    :accessor width
    :documentation "The objects width")
   (height
    :initarg :height
    :initform (error ":height must be specified")
    :accessor height
    :documentation "The objects height")
   (img ; TODO - probably not needed anymore, remove
    :initarg :img
    :initform nil
    :accessor img
    :documentation "The image used to draw the object - if nil then draw red rectangle")
   (anim
    :initarg :animation
    :initform nil
    :accessor anim
    :documentation "The animation for the object - if nil then just draw a red rectangle")
   (move-x-dir
    :initarg :x-dir
    :initform nil
    :accessor move-x-dir
    :documentation "The X direction for this object to move - can either be nil for still or :left/:right")
   (move-y-dir
    :initarg :y-dir
    :initform nil
    :accessor move-y-dir
    :documentation "The y direction to move - can either be nil to stay still or :up/:down")))

(defun make-obj (&key (w nil) (h nil) (x 0) (y 0) (xvel 0) (yvel 0) (img-path nil) (img-data-path nil)
		   (x-dir nil) (y-dir nil))
  (let ((anim
	  (when (and img-path img-data-path) (make-new-anim img-path img-data-path))))
    (make-instance 'object :xpos x :ypos y :width w :height h
			   :xvel xvel :yvel yvel :x-dir x-dir :y-dir y-dir
			   :animation anim)))

(defgeneric update-obj (obj)
  (:documentation "Update the object and stuff")
  (:method ((obj object))
    (handle-move obj)))

(defgeneric btm-y (obj)
  (:documentation "Get the bottom y value of the object")
  (:method (obj)
    (+ (ypos obj) (height obj))))

(defgeneric right-x (obj)
  (:documentation "Get the right xpos value of the object")
  (:method (obj)
    (+ (xpos obj) (width obj))))

(defgeneric mid-x (obj)
  (:documentation "Get the middle x position of the object")
  (:method ((obj object))
    (+ (xpos obj) (/ (width obj) 2.0))))

(defgeneric mid-y (obj)
  (:documentation "Get the middle y position of the object")
  (:method ((obj object))
    (+ (ypos obj) (/ (height obj) 2.0))))

(defgeneric move-right (obj &key vel)
  (:documentation "Increment the objects x position by it's velocity or by a provided one")
  (:method ((obj object) &key (vel (xvel obj)))
    (with-accessors ((x xpos)) obj
      (incf x vel))))
(defgeneric move-left (obj &key vel)
  (:documentation "Decrement the objects x position by it's velocity or by a provided one")
  (:method ((obj object) &key (vel (xvel obj)))
    (with-accessors ((x xpos)) obj
      (decf x vel))))

(defgeneric move-down (obj &key vel)
  (:documentation "Increment the objects y position by it's velocity or by a provided one")
  (:method ((obj object) &key (vel (yvel obj)))
    (with-accessors ((y ypos)) obj
      (incf y vel))))
(defgeneric move-up (obj &key vel)
  (:documentation "Decrement the objects y position by it's velocity or by a provided one")
  (:method ((obj object) &key (vel (yvel obj)))
    (with-accessors ((y ypos)) obj
      (decf y vel))))

(defgeneric handle-move (obj)
  (:documentation "Handle the movement of an object based on the move-dir slots")
  (:method ((obj object))
    (alexandria:switch ((move-x-dir obj)) ; handle x movment
      (:left (move-left obj))
      (:right (move-right obj)))
    (alexandria:switch ((move-y-dir obj)) ; handle y movement
      (:up (move-up obj))
      (:down (move-down obj)))))

(defgeneric colliding-p (obj other)
  (:documentation "Return T if the two objects are colliding or nil if not")
  (:method ((obj object) (other object))
    (with-accessors ((obj-x xpos) (obj-y ypos) (obj-right right-x) (obj-btm btm-y)) obj
      (with-accessors ((other-x xpos) (other-y ypos) (other-right right-x) (other-btm btm-y)) other
	(not (or (> obj-x other-right) (< obj-right other-x)
		 (< obj-btm other-y)   (> obj-y other-btm)))))))

(defgeneric x-pos-rel (obj cam)
  (:documentation "Get object x position relative to the camera")
  (:method ((obj object) (cam object))
    (- (xpos obj) (xpos cam))))

(defgeneric y-pos-rel (obj cam)
  (:documentation "Get object y position relative to the camera")
  (:method ((obj object) (cam object))
    (- (ypos obj) (ypos cam))))

(defgeneric mid-x-rel (obj cam)
  (:documentation "Get the relative middle x position of the obj")
  (:method ((obj object) (cam object))
    (+ (x-pos-rel obj cam) (/ (width obj) 2.0))))

(defgeneric mid-y-rel (obj cam)
  (:documentation "Get the relative middle y position of the obj")
  (:method ((obj object) (cam object))
    (+ (y-pos-rel obj cam) (/ (height obj) 2.0))))

(defgeneric right-x-rel (obj cam)
  (:documentation "Get the relative right x position of the obj")
  (:method ((obj object) (cam object))
    (+ (x-pos-rel obj cam) (width obj))))
(defgeneric btm-y-rel (obj cam)
  (:documentation "Get the relative bottom y position of the obj")
  (:method ((obj object) (cam object))
    (+ (y-pos-rel obj cam) (height obj))))


(defgeneric draw-obj (obj cam &key pen &allow-other-keys)
  (:documentation "Draw the given object"))

(defmethod draw-obj ((obj object) (cam object) &key (pen *default-pen*))
  (let ((draw-x (x-pos-rel obj cam))
	(draw-y (y-pos-rel obj cam))
	(animation (anim obj)))
    (if animation
	(draw-anim-frame animation :x draw-x :y draw-y)
	(with-pen pen
	  (rect draw-x draw-y (width obj) (height obj))))))

