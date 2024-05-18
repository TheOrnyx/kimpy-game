;;;; Animation.lisp
;;;; a basic kinda class and stuff to seperate like spritesheets into a list of images

(defclass animation ()
  ((original-img
    :initarg :orig-img
    :initform (error "Must supply original image")
    :accessor orig-img
    :documentation "The original image to be cropped from")
   (animation-frames
    :initarg :frames
    :initform (error "Must supply a list of animation frames")
    :accessor anim-frames
    :documentation "The list of animation frames to use")
   (frame-count
    :initarg :frame-count
    :initform (error "Must supply a number of frames")
    :accessor frame-count
    :documentation "The number of frames in the animation")
   (current-frame-num
    :initarg :frame-start-num
    :initform 0
    :accessor current-frame-num
    :documentation "The frame num currently being used")
   (frames-since-switch
    :initform 0
    :accessor frames-since-switch
    :documentation "The number of frames since the last switch")))

(defclass animation-frame ()
  ((frame-num
    :initarg :frame-num
    :initform 0
    :accessor frame-num
    :documentation "The frame number for the animation frame")
   (frame-x
    :initarg :frame-x
    :initform 0
    :accessor frame-x
    :documentation "The frame x offset for the animation frame")
   (frame-y
    :initarg :frame-y
    :initform 0
    :accessor frame-y
    :documentation "The frame y offset for the animation frame")
   (frame-w
    :initarg :frame-w
    :initform 0
    :accessor frame-w
    :documentation "The frame width for the animation frame")
   (frame-h
    :initarg :frame-h
    :initform 0
    :accessor frame-h
    :documentation "The frame height for the animation frame")
   (frame-duration
    :initarg :frame-duration
    :initform 0
    :accessor frame-duration
    :documentation "The frames duration for the animation frame (in milliseconds)")
   (frames-till-switch
    :initarg :frames-till-switch
    :initform 0
    :accessor frames-till-switch)
   (frame-img
    :initarg :frame-img
    :initform (error "Must supply a frame image")
    :accessor frame-img
    :documentation "The cut up image for the frame")))

(defmethod current-frame ((anim animation))
  "Get the current animation frame"
  (aref (anim-frames anim) (current-frame-num anim)))

(defun num-frames-till-switch (duration)
  "The amount of frames till animation should switch"
  (let* ((frame-dur (/ 1.0 +frame-rate+))
	 (num-frames-till-switch (/ duration (* frame-dur 1000))))
    num-frames-till-switch))

(defmethod update-anim ((anim animation))
  "Update the animation and increase the current frame number etc"
  ;; TODO - add duration being a thing
  (with-accessors ((frame current-frame) (frames-switch frames-since-switch)
		   (cur-frame-num current-frame-num) (frame-count frame-count)) anim
    (when (>= (incf frames-switch 1) (frames-till-switch frame)) 
      (setf cur-frame-num (mod (1+ cur-frame-num) frame-count))
      (setf frames-switch 0))))

(defmethod draw-anim-frame ((anim animation) &key (x 0) (y 0)
					       (w (frame-w (current-frame anim)))
					       (h (frame-h (current-frame anim))))
  (with-accessors ((frame current-frame)) anim
    (if frame
	(image (frame-img frame) x y w h)
	(with-pen (make-pen :fill +red+ :stroke +black+ :weight 2)
	  (rect x y w h)))))

;;; The stuff for parsing and creating the animation

(defun make-anim-frame (orig-img frame-data)
  "Create and return an animation-frame object from the given frame-data hashtable and the original image"
  (let* ((duration (gethash "duration" frame-data))
	 (frame-number (gethash "filename" frame-data))
	 (frame-info (gethash "frame" frame-data))
	 (frame-h (gethash "h" frame-info))
	 (frame-w (gethash "w" frame-info))
	 (frame-x (gethash "x" frame-info))
	 (frame-y (gethash "y" frame-info))
	 (frame-cropped-img (crop orig-img frame-x frame-y frame-w frame-h)))
    (make-instance 'animation-frame :frame-duration duration :frame-y frame-y :frame-x frame-x
				    :frame-w frame-w :frame-h frame-h :frame-num frame-number
				    :frame-img frame-cropped-img
				    :frames-till-switch (num-frames-till-switch duration))))

;; TODO - check that using a vector is fine
(defun make-anim-frames (orig-img img-data-path)
  "Open the img-data-path json file and parse the frame information in it to cut up orig-img.
  Returns a list of animation-frames"
  (let* ((frame-data-vec (gethash "frames" (jzon:parse img-data-path)))
	 (frames-vec (make-array (array-total-size frame-data-vec) :fill-pointer 0)))
    (loop for frame across frame-data-vec
	  do (vector-push (make-anim-frame orig-img frame) frames-vec))
    frames-vec))

(defun make-new-anim (img-path img-data-path)
  "Create and return a new animation object. Parses all the data and stuff."
  (let* ((full-img (load-resource img-path))
	 (anim-frames (make-anim-frames full-img img-data-path)))
    (make-instance 'animation :frame-count (array-total-size anim-frames)
			      :frames anim-frames :orig-img full-img)))

