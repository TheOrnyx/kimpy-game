;;;; kimpy.lisp

(in-package #:kimpy)
(defvar *kimpy* nil "The player character, kimpy :)")
(defvar *camera* nil "The main camera to use for the game")
(defparameter *object-list* '() "List of all the objects in the game") ;; TODO - change this to a vector, didn't know lisp had them >:(
(defconstant +win-width+ 800 "The windows width")
(defconstant +win-height+ 800 "The windows height")
(defconstant +frame-rate+ 60 "the games frame rate")
(defparameter *world-pen* (make-pen :fill (rgb-255 100 100 100) :stroke +white+ :weight 4))

;; The variables for the like movement keys
(defparameter *right-key*	:scancode-d)
(defparameter *left-key*	:scancode-a)
(defparameter *up-key*		:scancode-w)
(defparameter *down-key*	:scancode-s)
(defparameter *x-move-dir* nil) ;; the x pos direction to move, can be :left, :right or nil for no movement
(defparameter *y-move-dir* nil) ;; the y pos direction to move, can be :up, :down or nil for no movement

(defclass game-info ()
  ((mouse-info
    :initarg :m-info
    :initform (make-instance 'object :height 0 :width 0)
    :accessor mouse-info)
   (world-info
    :initarg :w-info
    :initform (make-instance 'object :height 2000 :width 2000)
    :accessor world-info
    :documentation "The info about the world, it's size etc")
   (world-bg
    :initarg :bg
    :initform nil
    :accessor world-bg)))

(defparameter *game-info*
  (make-instance 'game-info)
  "Instance of game representing the current info")

(defmethod draw-world ((info game-info) (cam camera) &key (pen *world-pen*))
  (with-accessors ((world world-info)) info
    (if (world-bg info)
	(image
	 (crop (world-bg info) 0 0 (width world) (height world))
	 (x-pos-rel world cam) (y-pos-rel world cam) (width world) (height world))
	(with-pen pen
	  (rect (x-pos-rel world cam) (y-pos-rel world cam) (width world) (height world))))))

(defmethod mouse-x ((info game-info))
  (xpos (mouse-info info)))
(defmethod (setf mouse-x) (val (info game-info))
  (setf (slot-value (mouse-info info) 'xpos) val))
(defmethod mouse-y ((info game-info))
  (ypos (mouse-info info)))
(defmethod (setf mouse-y) (val (info game-info))
  (setf (slot-value (mouse-info info) 'ypos) val))

(defun draw-debug-text ()
  "Draw some debug text about position and stuff for player and camera"
  (text (format nil ; TODO - this is rlly gross, fix later
"Player: x:~A, y:~A, x-dir:~A, y-dir:~A
Camera: x:~A, y:~A, midx:~A, midy~A
Mouse : X:~A, y:~A"
		(xpos *kimpy*) (ypos *kimpy*) (move-x-dir *kimpy*) (move-y-dir *kimpy*)
		(xpos *camera*) (ypos *camera*) (mid-x *camera*) (mid-y *camera*)
		(mouse-x *game-info*) (mouse-y *game-info*))
	0 0))

(defun update-all-objs ()
  "Update all the objects in the object list."
  (iter (iter:for obj iter:in *object-list*)
    (update-obj obj)))
  
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

(defsketch game
    ((title "idk yet") (width +win-width+) (height +win-height+))
  (set-pen (make-pen :fill (rgb 0 0 0 0.0) :weight 4))
  (background +black+)
  (draw-world *game-info* *camera*)
  ;; (handle-move)
  (focus-camera *camera*)
  (draw-all-objs)
  (draw-obj *camera* *camera*)
  (draw-obj *kimpy* *camera*)
  (update-player *kimpy*)
  (update-all-objs)
  (draw-debug-text)
  (check-obj-collision-with-player))

(defmethod setup ((instance game) &key &allow-other-keys)
  (setf *object-list* '()) ; reset the object list
  (setf *kimpy* (make-player :img-path "./data/sprites/kimpy/kimpy-animtest.png"
			     :img-data-path #P"./data/sprites/kimpy/kimpy-animtest.json"
			     :x 0 :y 0 :xvel 5 :yvel 5 ))
  (setf (player-weapon *kimpy*) (make-weapon :w 100 :h 50 :parent *kimpy*))
					     ;; :img-path "./data/sprites/kimpy/kimpy-knif.png"
					     ;; :img-data-path #P"./data/sprites/kimpy/kimpy-knif.json"))
  (setf *camera* (make-camera *kimpy* +win-width+ +win-height+ 0.5))
  ;; (setf *camera* (make-camera *kimpy* 400 400 0.5))
  (add-obj (make-obj :w 120 :h 120 :x 200 :y 500 :xvel 2 :x-dir :right))
  (setf *game-info* (make-instance 'game-info :bg (load-resource "./data/sprites/kimpy/tile-test1.png")))
  (background +black+))

(defmethod kit.sdl2:mousebutton-event ((window game) state ts b x y)
  (with-slots (rectangles) window
    (when (eq state :mousebuttondown)
      (setf (xpos *kimpy*) x)
      (setf (ypos *kimpy*) y))))

(defmethod kit.sdl2:mousemotion-event :after ((window game) timestamp mask x y xrel yrel)
  (setf (mouse-x *game-info*) x
	(mouse-y *game-info*) y))

(defmethod kit.sdl2:keyboard-event :after ((window game) state timestamp repeat-p keysym)
  (with-accessors ((x-dir move-x-dir) (y-dir move-y-dir)) *kimpy*
    (let ((key (sdl2:scancode keysym)))
      (if (eq state :keydown) ;; key pressed down
	  (alexandria:switch (key)
	    (*right-key* (setf (move-x-dir *kimpy*) :right))
	    (*left-key*  (setf (move-x-dir *kimpy*) :left))
	    (*up-key*    (setf (move-y-dir *kimpy*) :up))
	    (*down-key*  (setf (move-y-dir *kimpy*) :down)))

	  (alexandria:switch (key) ; key released
	    (*right-key* (when (eq x-dir :right) (setf x-dir nil)))
	    (*left-key*  (when (eq x-dir :left) (setf x-dir nil)))
	    (*up-key*    (when (eq y-dir :up) (setf y-dir nil)))
	    (*down-key*  (when (eq y-dir :down) (setf y-dir nil))))))))

(defun start-kimpy ()
  ;; (setf *kimpy* (make-obj :w 60 :h 60 :img-path "./data/sprites/kimpy/kimpy-draft1.png" :x 20 :y 20))
  (make-instance 'game))
