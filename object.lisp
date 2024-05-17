;;;; object.lisp

(in-package #:kimpy)



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
   (img
    :initarg :img
    :initform nil
    :accessor img
    :documentation "The image used to draw the object - if nil then draw red rectangle"
    )
   ))

(defun make-obj (&key (w nil) (h nil) (x 0) (y 0) (xvel 0) (yvel 0) (img-path nil))
  (let ((img
	(when img-path (load-resource img-path))))
    (make-instance 'object :xpos x :ypos y :width w :height h
			   :xvel xvel :yvel yvel
			   :img img)))

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

(defgeneric colliding-p (obj other)
  (:documentation "Return T if the two objects are colliding or nil if not")
  (:method ((obj object) (other object))
    (with-accessors ((obj-x xpos) (obj-y ypos) (obj-right right-x) (obj-btm btm-y)) obj
      (with-accessors ((other-x xpos) (other-y ypos) (other-right right-x) (other-btm btm-y)) other
	(not (or (> obj-x other-right) (< obj-right other-x)
		 (< obj-btm other-y)   (> obj-y other-btm)))))))



(defgeneric draw-obj (obj cam)
  (:documentation "Draw the given object"))

(defmethod draw-obj ((obj object) (cam camera))
    (with-accessors ((x xpos) (y ypos) (pic img)
		     (w width) (h height)) obj
      (if pic
	  (image pic x y w h)
	  (with-pen (make-pen :fill +red+ :stroke +black+ :weight 2)
	    (rect x y w h)))))
