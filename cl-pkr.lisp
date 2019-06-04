(sb-ext:unlock-package :sb-ext)
(ql:quickload '(ltk inferior-shell qbase64))

(defpackage :cl-pkr
  (:use #:ltk #:common-lisp #:inferior-shell))

(in-package :cl-pkr)

(load "./common.lisp")

#+darwin (load "./darwin.lisp")
#+linux (load "./linux.lisp")
#+win32 (load "./win32.lisp")

(defparameter *update-frequency* 10)

(setf *wish-args* '("-name" "Color Picker"))
(setf *wish-pathname* "./bin/tclkit-gui")

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

(defun make-larger-image (pixels ratio)
  (let* ((raw-photo (make-instance 'photo-image))
         (photo (make-instance 'photo-image)))
    (with-atomic 
        (image-setpixel raw-photo pixels 0 0)
      (format-wish "~A copy ~A -zoom ~A"
                   (widget-path photo)
                   (widget-path raw-photo)
                   ratio)
      (format-wish "image delete ~A" (widget-path raw-photo)))
    photo))

(defun make-blank-image ()
  (make-instance 'photo-image
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

(defun make-tip-label ()
  (make-instance 'label
                 :text (get-tip)
                 :width 248 :padding 10
                 :foreground "blue"))

(defun init-window (&optional (width  496) (height 248))
  (mapcar
   (lambda (func) (funcall func *tk* width height))
   '(maxsize minsize)))

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
    (let* ((hex-color nil) (rgb-color nil) (hsl-color nil)
           (blank-image (make-blank-image))
           (point-canvas (make-point-canvas))
           (sample-canvas (make-sample-canvas))
           (image-label (make-instance 'label :image blank-image))
           (hex-label (make-color-label "HEX: #FFFFFF"))
           (rgb-label (make-color-label "RGB: 255, 255, 255"))
           (hsl-label (make-color-label "HSL: 0, 0%, 100%"))
           (tip-label (make-tip-label))
           (old-x nil)
           (old-y nil))
      (configure image-label :borderwidth 0)
      (place image-label 0 0)
      (place hex-label 248 0 :height 40)
      (place rgb-label 248 40 :height 40)
      (place hsl-label 248 80 :height 40)
      (place tip-label 248 120 :height 40)
      (bind-hotkeys hex-color rgb-color hsl-color)
      (labels ((tip-talk (str &key (color "blue"))
                 (setf (text tip-label) str)
                 (configure tip-label :foreground color))
               (update ()
                 (let* ((x (screen-mouse-x))
                        (y (screen-mouse-y)))
                   (when (not (and (eq x old-x) (eq y old-y)))
                     (when (or (< x 0) (< y 0))
                       (tip-talk "Come Back to Main Screen, Please" :color "#EE0000")
                       (after *update-frequency* #'update)
                       (return-from update))
                       (let* ((pixels (x-snapshot :x x :y y :width 31 :height 31 :offset 15))
                              (colors (color->strs (pixel->color pixels 15 15))))
                         (tip-talk (get-tip))
                         (setf
                          old-x x
                          old-y y
                          hex-color (first colors)
                          rgb-color (second colors)
                          hsl-color (third colors)
                          (text hex-label) (concat "HEX: " hex-color)
                          (text rgb-label) (concat "RGB: " rgb-color)
                          (text hsl-label) (concat "HSL: " hsl-color))                         
                         (configure point-canvas :background hex-color)
                         (configure sample-canvas :background hex-color)
                         (place point-canvas 120 120 :width 8 :height 8)
                         (place sample-canvas 248 160)
                         (configure image-label :image (make-larger-image pixels 8)))))
                 (after *update-frequency* #'update)))
        (after *update-frequency* #'update)))))

(defun dump ()
  (sb-ext:save-lisp-and-die
   #-win32 "bin/color-picker"
   #+win32 "bin/color-picker.exe"
   #-win32 :compression
   #-win32 t
   #+win32 :application-type
   #+win32 :gui
   :toplevel (lambda ()
               (setf *wish-pathname*
                     (concat (namestring (uiop:getcwd)) "tclkit-gui"))
               (cl-pkr::color-picker))
   :executable t))
