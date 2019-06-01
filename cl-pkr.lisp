(sb-ext:unlock-package :sb-ext)
(ql:quickload '(ltk zpng inferior-shell qbase64 bordeaux-threads))

(load "./ltk-thread-patch.lisp")

(defpackage :cl-pkr
  (:use #:ltk #:common-lisp #:inferior-shell #:bordeaux-threads))

(in-package :cl-pkr)

(defparameter *display-size-ratio* 1)
(defparameter *x-hook-registered* nil)

#+darwin (load "./darwin.lisp")
#+linux (load "./linux.lisp")
#+win32 (load "./win32.lisp")

(setf ltk:*wish-args* '("-name" "Color Picker"))
#+darwin (setf ltk:*wish-pathname* "./tclkit-gui")
#+win32 (setf ltk:*wish-pathname* "/c/ActiveTcl/bin/wish")



(load "./common.lisp")

(let ((tip-index 0))
  (defun get-tip ()
    (nth tip-index
         #+darwin '("[Cmd + C] to Copy HEX"
                    "[Cmd + Shift + C] to Copy RGB"
                    "[Cmd + Option + C] to Copy HSL")
         #+(or linux win32) '("[Control + C] to Copy HEX"
                   "[Control + Shift + C] to Copy RGB"
                   "[Control + Alt + C] to Copy HSL")))
  (defun scramble-tip ()
    (setf tip-index (random 3))))


(defun make-larger-image (data ratio)
  (let* ((raw-photo (make-instance 'photo-image :data data))
         (photo (make-instance 'photo-image)))
    (format-wish "~A copy ~A -zoom ~A"
                 (widget-path photo)
                 (widget-path raw-photo)
                 ratio)
    (format-wish "image delete ~A" (widget-path raw-photo))
    photo))

(defun make-smith-image ()
  (make-instance 'photo-image
                 :file "./agent-smith.png"
                 :width 248
                 :height 248))

(defun make-blank-image ()
  (make-instance 'photo-image
                 :data (png->base64
                        (make-instance 'zpng:png :width 248 :height 248))
                 :width 248
                 :height 248))

(defun make-point-canvas ()
  (make-instance 'canvas
                 :background "white"
                 :highlightthickness 1
                 :highlightbackground "black"))

(defun make-sample-canvas ()
  (make-instance 'canvas
                 :background "white"
                 :highlightthickness 2
                 :highlightbackground "black"
                 :width 244 :height 84))

(defun make-color-label (text)
  (make-instance 'label
                 :text text :width 248 :padding 10))

(defun make-smith-label ()
  (make-instance 'label
                 :text (get-tip)
                 :width 248 :padding 10
		 ; :foreground "blue"
		 ))

(defun init-window (&optional (width  496) (height 248))
  ;; (configure *tk* :menu (make-menubar))
  (mapcar
   (lambda (func) (funcall func *tk* width height))
   '(maxsize minsize)))

(let ((last-event-time 0))
  (defun dispatch-system-motion-event ()
    (when (> (- (get-internal-real-time) last-event-time) 10)
      (sleep 1/100)
      (format-wish "event generate . <<SystemMotion>>")
      (setf last-event-time (get-internal-real-time)))))


(defmacro bind-hotkeys (hex-color rgb-color hsl-color)
  `(progn
     (bind *tk*
       #+darwin "<Command-c>"
       #+(or linux win32) "<Control-c>"
       (lambda (e) (declare (ignore e)) (x-copy ,hex-color) (scramble-tip)))
     (bind *tk*
       #+darwin "<Command-Shift-c>"
       #+(or linux win32) "<Control-C>"
       (lambda (e) (declare (ignore e)) (x-copy ,rgb-color) (scramble-tip)))
     (bind *tk*
       #+darwin "<Command-Option-c>"
       #+(or linux win32) "<Control-Alt-c>"
       (lambda (e) (declare (ignore e)) (x-copy ,hsl-color) (scramble-tip)))))

(defun color-picker ()
  (with-ltk ()
    (init-window)
    (let* ((focused nil)
           (hex-color nil) (rgb-color nil) (hsl-color nil)
           (agent-smith (make-smith-image))
           (blank-image (make-blank-image))
           (point-canvas (make-point-canvas))
           (sample-canvas (make-sample-canvas))
           (image-label (make-instance 'label :image blank-image))
           (hex-label (make-color-label "HEX: #FFFFFF"))
           (rgb-label (make-color-label "RGB: 255, 255, 255"))
           (hsl-label (make-color-label "HSL: 0, 0%, 100%"))
           (smith-label (make-smith-label)))
      (configure image-label :borderwidth 0)
      (place image-label 0 0)
      (place hex-label 248 0 :height 40)
      (place rgb-label 248 40 :height 40)
      (place hsl-label 248 80 :height 40)
      (place smith-label 248 120 :height 40)
      (labels ((smith-talk (text)
                 (setf (text smith-label) text)
                 (place point-canvas 0 0 :width 0 :height 0)
                 (configure image-label :image agent-smith))
               (mouse-move-callback ()
                 (format t "~%AUTO:~A" ltk::*buffer-for-atomic-output*)
                 (let* ((x
			 #+darwin (* *display-size-ratio* (screen-mouse-x))
			 #+win32 (screen-mouse-x))
                        (y
			 #+darwin (* *display-size-ratio* (screen-mouse-y))
			 #+win32 (screen-mouse-y))
                        (png (x-snapshot :x (- x 15) :y (- y 15) :width 31 :height 31))
                        (data (and png (zpng:data-array png)))
                        (colors (color->strs (pixel->color data 15 15)))
                        (base64 (png->base64 png)))
		   (format t "~%I AM HERE 0")
                   (setf hex-color (first colors)
                         rgb-color (second colors)
                         hsl-color (third colors)
                         (text hex-label) (concat "HEX: " hex-color)
                         (text rgb-label) (concat "RGB: " rgb-color)
                         (text hsl-label) (concat "HSL: " hsl-color)
			 ;; (text rgb-label) (concat "RGB: " rgb-color)
                         )
		   (format t "~%I AM HERE 1")
                   (if (or (< x 0) (< y 0))
                       (smith-talk "You've gone too far, Neo")
                       (progn
			 (setf (text smith-label) (get-tip))
			 (return-from mouse-move-callback)
			 (format t "~%I AM HERE 2")
			 (format t "~%LOCK0:~A~%" ltk::*wish-lock*)
                         (configure point-canvas :background hex-color)
			 (format t "~%LOCK1:~A~%" ltk::*wish-lock*)
			 (format t "~%I AM HERE 3")
                         (configure sample-canvas :background hex-color)
                         (place point-canvas 120 120 :width 8 :height 8)
                         (place sample-canvas 248 160)
                         (configure image-label :image (make-larger-image base64 8))
			 (format t "~%I AM HERE 4")))
		   (format t "~%I AM HERE 4"))))
        (bind-hotkeys hex-color rgb-color hsl-color)
	(bind *tk* "<<SystemMotion>>"
	      (lambda (e) (declare (ignore e))
                 (with-atomic (mouse-move-callback))))
        (if (x-able-to-hook)
            (progn
              (let ((top-wish *wish*))
                (make-thread (lambda ()
                               (x-register-hook
				(lambda ()
				  (setf *wish* top-wish)
				  (when (wish-stream *wish*)
				    (dispatch-system-motion-event))))))
                (setf *x-hook-registered* t)))
            (progn
              (bind *tk* "<Motion>"
                    (lambda (e) (declare (ignore e))
			    (dispatch-system-motion-event)))
              (bind *tk* "<FocusIn>"
                    (lambda (e) (declare (ignore e))
			    (setf focused t (text smith-label) (get-tip))
			    (configure image-label :image agent-smith)))
              (bind *tk* "<FocusOut>"
                    (lambda (e) (declare (ignore e))
			    (smith-talk "Focus ME! Neo, to the truth"))))))))
  (when *x-hook-registered* (x-unregister-hook)))

(defun dump ()
  (sb-ext:save-lisp-and-die
   "bin/color-picker"
   :compression t
   :toplevel (lambda ()
               (cl-pkr::color-picker))
   :executable t))
