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
   :documentation "The frame num currently being used")))

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
    :documentation "The frames duration for the animation frame")
   (frame-img
    :initarg :frame-img
    :initform (error "Must supply a frame image")
    :accessor frame-img
    :documentation "The cut up image for the frame")))

(defmethod current-frame ((anim animation))
  "Get the current animation frame"
  (aref (anim-frames anim) (current-frame-num anim)))

(defmethod update-anim ((anim animation))
  "Update the animation and increase the current frame number etc"
  ;; TODO - add duration being a thing
  (setf (current-frame-num anim) (mod (1+ (current-frame-num anim)) (frame-count anim))))

(defun make-anim-frame (orig-img frame-data)
  "Create and return an animation-frame object from the given frame-data hashtable and the original image to cut from"
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
				    :frame-img frame-cropped-img)))

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


