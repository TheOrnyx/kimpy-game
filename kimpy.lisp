;;;; kimpy.lisp

(in-package #:kimpy)
(defvar *kimpy* nil "The player character, kimpy :)")
(defvar *camera* nil "The main camera to use for the game")
(defparameter *object-list* '() "List of all the objects in the game") ;; TODO - change this to a vector, didn't know lisp had them >:(
(defconstant +win-width+ 800 "The windows width")
(defconstant +win-height+ 800 "The windows height")
(defconstant +frame-rate+ 60 "the games frame rate")

;; The variables for the like movement keys
(defparameter *right-key*	:scancode-d)
(defparameter *left-key*	:scancode-a)
(defparameter *up-key*		:scancode-w)
(defparameter *down-key*	:scancode-s)
(defparameter *x-move-dir* nil) ;; the x pos direction to move, can be :left, :right or nil for no movement
(defparameter *y-move-dir* nil) ;; the y pos direction to move, can be :up, :down or nil for no movement

(defun draw-debug-text ()
  "Draw some debug text about position and stuff for player and camera"
  (text (format nil "Player: x:~A, y:~A, midx:~A, midy:~A~%Camera: x:~A, y:~A, midx:~A, midy~A"
		(xpos *kimpy*) (ypos *kimpy*) (mid-x *kimpy*) (mid-y *kimpy*)
		(xpos *camera*) (ypos *camera*) (mid-x *camera*) (mid-y *camera*))
	0 0))
  
(defun draw-all-objs ()
  "Draw all the objects in the *object-list*."
  (iter (iter:for obj iter:in *object-list*)
    (draw-object *camera* obj)))
    ;; (draw-obj obj)))

;; TODO - maybe improve this later idk
(defun check-obj-collision-with-player ()
  "Check all of the object collisions with the player."
  (loop for obj in *object-list*
        do (if (colliding-p *kimpy* obj)
               (progn
                 (setf (collided *kimpy*) t)
                 (return t))
               (setf (collided *kimpy*) nil)))
  (collided *kimpy*))


(defun add-obj (obj)
  (check-type obj object)
  (setf *object-list* (append *object-list* (list obj))))

(defun handle-move ()
  (alexandria:switch (*x-move-dir*)
    (:left  (move-left *kimpy*))
    (:right (move-right *kimpy*)))
  (alexandria:switch (*y-move-dir*)
    (:up    (move-up *kimpy*))
    (:down  (move-down *kimpy*))))

(defsketch game
    ((title "idk yet") (width +win-width+) (height +win-height+))
  (set-pen (make-pen :fill (rgb 0 0 0 0.0) :weight 4))
  (handle-move)
  (focus-camera *camera*)
  (draw-all-objs)
  (draw-obj *camera* *camera*)
  (draw-obj *kimpy* *camera*)
  (update-player *kimpy*)
  (draw-debug-text)
  (check-obj-collision-with-player))

(defmethod setup ((instance game) &key &allow-other-keys)
  (setf *kimpy* (make-player :img-path "./data/sprites/kimpy/kimpy-animtest.png"
			     :img-data-path #P"./data/sprites/kimpy/kimpy-animtest.json"
			     :x 0 :y 0 :xvel 5 :yvel 5))
  (setf *camera* (make-camera *kimpy* +win-width+ +win-height+ 0.5))
  ;; (setf *camera* (make-camera *kimpy* 400 400 0.5))
  (add-obj (make-obj :w 120 :h 120 :x 200 :y 500)))

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
