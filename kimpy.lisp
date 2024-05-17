;;;; kimpy.lisp

(in-package #:kimpy)
(defvar *kimpy*)

;; The variables for the like movement keys
(defparameter *right-key*	:scancode-d)
(defparameter *left-key*	:scancode-a)
(defparameter *up-key*		:scancode-w)
(defparameter *down-key*	:scancode-s)
(defparameter *x-move-dir* nil) ;; the x pos direction to move, can be :left, :right or nil for no movement
(defparameter *y-move-dir* nil) ;; the y pos direction to move, can be :up, :down or nil for no movement

(defun handle-move ()
  (alexandria:switch (*x-move-dir*)
    (:left  (move-left *kimpy*))
    (:right (move-right *kimpy*)))
  (alexandria:switch (*y-move-dir*)
    (:up    (move-up *kimpy*))
    (:down  (move-down *kimpy*))))

(defsketch game
    ((title "idk yet") (width 800) (height 800))
  (set-pen (make-pen :fill (rgb 0 0 0 0.0) :weight 4))
  (draw-obj *kimpy*)
  (handle-move))

(defmethod setup ((instance game) &key &allow-other-keys)
  (setf *kimpy* (make-obj :w 120 :h 120 :img-path "./data/sprites/kimpy/kimpy-draft1.png"
			  :x 20 :y 20 :xvel 5 :yvel 5)))

(defmethod kit.sdl2:mousebutton-event ((window game) state ts b x y)
  (with-slots (rectangles) window
    (when (eq state :mousebuttondown)
      (setf (xpos *kimpy*) x)
      (setf (ypos *kimpy*) y))))

(defmethod kit.sdl2:keyboard-event :after ((window game) state timestamp repeat-p keysym)
  (let ((key (sdl2:scancode keysym)))
    (if (eq state :keydown) ;; key pressed down
	(alexandria:switch (key)
	  (*right-key* (setf *x-move-dir* :right))
	  (*left-key*  (setf *x-move-dir* :left))
	  (*up-key*    (setf *y-move-dir* :up))
	  (*down-key*  (setf *y-move-dir* :down)))

	(alexandria:switch (key)
	  (*right-key* (when (eq *x-move-dir* :right) (setf *x-move-dir* nil)))
	  (*left-key*  (when (eq *x-move-dir* :left) (setf *x-move-dir* nil)))
	  (*up-key*    (when (eq *y-move-dir* :up) (setf *y-move-dir* nil)))
	  (*down-key*  (when (eq *y-move-dir* :down) (setf *y-move-dir* nil)))))))

(defun start-kimpy ()
  ;; (setf *kimpy* (make-obj :w 60 :h 60 :img-path "./data/sprites/kimpy/kimpy-draft1.png" :x 20 :y 20))
  (make-instance 'game))
