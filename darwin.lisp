(ql:quickload '(:cffi :bordeaux-threads :inferior-shell))
(use-package '(:cffi :bordeaux-threads :inferior-shell))

(define-foreign-library application-services
  (:darwin "ApplicationServices.framework/ApplicationServices"))

(use-foreign-library application-services)

(defcfun ("CGMainDisplayID" cg-main-display-id)  :uint32)
(defcfun ("CGDisplayPixelsHigh" cg-display-pixels-high) :int (display :uint32))
(defcfun ("CGDisplayPixelsWide" cg-display-pixels-wide) :int (display :uint32))
(defcfun ("CGDisplayCreateImage" cg-display-create-image) :pointer (display :uint32))
(defcfun ("CGImageRelease" cg-image-release) :void (image :pointer))
(defcfun ("CGImageGetDataProvider" cg-image-get-data-provider) :pointer (image :pointer))
(defcfun ("CGDataProviderCopyData" cg-data-provider-copy-data) :pointer (provider :pointer))
(defcfun ("CGImageGetWidth" cg-image-get-width) :int (image :pointer))
(defcfun ("CGImageGetHeight" cg-image-get-height) :int (image :pointer))
(defcfun ("CFDataGetBytePtr" cf-data-get-byte-ptr) :pointer (data :pointer))
(defcfun ("CFDataGetLength" cf-data-get-length) :int (data :pointer))
(defcfun ("CFRelease" cf-release) :void (image :pointer))

(defun get-cg-image-ref ()
  (cg-display-create-image (cg-main-display-id)))

(defun get-cf-data-ref (image)
  (cg-data-provider-copy-data
   (cg-image-get-data-provider image)))

(defmacro with-cg-image-ref ((image) &body body)
  `(let ((,image (get-cg-image-ref)))
     (unwind-protect
          (progn ,@body)
       (cg-image-release ,image))))

(defmacro with-cf-data-ptr ((image data) &body body)
  `(let* ((data-ref (get-cf-data-ref ,image))
          (,data (cf-data-get-byte-ptr data-ref)))
     (unwind-protect
          (progn ,@body)
       (cf-release data-ref))))

(defmacro with-display-pixel-data ((data width height) &body body)
  `(with-cg-image-ref (img)
     (let ((,width (cg-image-get-width img))
           (,height (cg-image-get-height img)))
       (with-cf-data-ptr (img ,data)
         ,@body))))

(defun x-display-size ()
  (let ((display-id (cg-main-display-id)))
    (values
     (cg-display-pixels-wide display-id)
     (cg-display-pixels-high display-id))))

(defun raw-data->pixel-list (raw-data x0 y0 x1 y1 w)
  (loop for _y from y0 to y1
     collect
       (loop for _x from x0 to x1
          collect
            (let* ((cell-i (+ (* w _y)_x))
                   (value-i (* cell-i 4))
                   (r (mem-aref raw-data :uint8 value-i))
                   (g (mem-aref raw-data :uint8 (incf value-i)))
                   (b (mem-aref raw-data :uint8 (incf value-i)))
                   (a (mem-aref raw-data :uint8 (incf value-i))))
              (list b g r a)))))

;; for holy retina screen
(defparameter *display-size-ratio* 2)

(defun x-snapshot (&key (x 0) (y 0)
                     (width 1)
                     (height 1)
                     (offset 0))
  (and
   (>= x 0)
   (>= y 0)
   (multiple-value-bind (display-width) (x-display-size)
     (with-display-pixel-data (d w h)
       (setf *display-size-ratio* (/ w  display-width)
             x (- (* *display-size-ratio* x) offset)
             y (- (* *display-size-ratio* y) offset))
       (let* ((proper-x (if (> x w) w (if (> x 0) x 0)))
              (proper-y (if (> y h) h (if (> y 0) y 0)))
              (proper-width (if (> width w) w width))
              (proper-height (if (> height h) h height))
              (max-x (+ proper-x proper-width))
              (max-y (+ proper-y proper-height))
              (proper-max-x (if (> max-x w) w max-x))
              (proper-max-y (if (> max-y h) h max-y)))
         (raw-data->pixel-list
          d
          proper-x
          proper-y
          (1- proper-max-x)
          (1- proper-max-y)
          w))))))
