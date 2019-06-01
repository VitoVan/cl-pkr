(ql:quickload :cffi)
(use-package :cffi)

(define-foreign-library user32
    (:win32 (:or "user32.dll")))

(use-foreign-library user32)

(define-foreign-library gdi32
    (:win32 (:or "gdi32.dll")))

(use-foreign-library gdi32)

(defcfun ("SetProcessDPIAware" set-process-dpi-aware) :boolean)
(defcfun ("GetSystemMetrics" get-system-metrics) :int (nIndex :int))
(defcfun ("GetDC" get-dc) :pointer (hwnd :pointer))
(defcfun ("DeleteDC" delete-dc) :boolean (hdc :pointer))
(defcfun ("ReleaseDC" release-dc) :int (hWnd :pointer) (hdc :pointer))
(defcfun ("DeleteObject" delete-object) :boolean (ho :pointer))
(defcfun ("CreateCompatibleDC" create-compatible-dc) :pointer (hdc :pointer))
(defcfun ("CreateCompatibleBitmap" create-compatible-bitmap) :pointer
  (hdc :pointer)
  (cx :int)
  (cy :int))
(defcfun ("SelectObject" select-object) :pointer (hdc :pointer) (h :pointer))

(defcfun ("GetPixel" get-pixel) :uint32 (hdc :pointer) (x :int) (y :int))

(defcfun ("OpenClipboard" open-clipboard) :boolean (hWndNewOwner :pointer))
(defcfun ("CloseClipboard" close-clipboard) :boolean)
(defcfun ("EmptyClipboard" empty-clipboard) :boolean)
(defcfun ("SetClipboardData" set-clipboard-data) :pointer (uFormat :int) (hMem :pointer))
(defparameter *CF_BITMAP* 2)
(defparameter *SRCCOPY* 13369376)

(defcfun ("BitBlt" bit-blt) :boolean
  (hdc :pointer)
  (x :int)
  (y :int)
  (cx :int)
  (cy :int)
  (hdcSrc :pointer)
  (x1 :int)
  (y1 :int)
  (drop :uint32))

(defun x-unregister-hook ()
  (format t "~%UNREG-HOOK:~%"))
(defun x-able-to-hook ()
  nil)
(defun x-copy (text)
  (format t "~%COPY: ~A~%" text))
(defun x-register-hook (callback)
  (format t "~%REG-HOOK:~%"))

(defmacro with-screen-dc ((dc width height) &body body)
  `(let* ((screen-dc (get-dc (null-pointer)))
	  (,dc (create-compatible-dc screen-dc))
	  (,width (ltk:screen-width))
	  (,height (ltk:screen-height))
	  (bitmap (create-compatible-bitmap screen-dc ,width ,height))
	  (old-obj (select-object ,dc bitmap)))
     (unwind-protect
	  (progn
	    (bit-blt ,dc 0 0 ,width ,height screen-dc 0 0 *SRCCOPY*)
	    ,@body)
       (progn
	 (select-object ,dc old-obj)
	 (delete-dc ,dc)
	 (release-dc (null-pointer) screen-dc)
	 (delete-object bitmap)))))

(defun x-display-size ()
  (values
   (get-system-metrics 0)
   (get-system-metrics 1)))

(defun raw-dc->png-data (raw-dc png-data x0 y0 x1 y1)
  (loop for _y from y0 to y1
     collect
       (loop for _x from x0 to x1
          collect
            (let* ((decimal-pixel (get-pixel raw-dc _x _y))
		   (rgb-data (decimal->rgb decimal-pixel))
                   (r (nth 0 rgb-data))
                   (g (nth 1 rgb-data))
                   (b (nth 2 rgb-data))
                   (a 255))
              (let ((png-x (- _x x0))
                    (png-y (- _y y0)))
		;; (format t "~%X ~A, Y ~A, R ~A, G ~A, B ~A" _x _y r g b)
                (setf (aref png-data png-y png-x 0) b
                      (aref png-data png-y png-x 1) g
                      (aref png-data png-y png-x 2) r
                      (aref png-data png-y png-x 3) a))))))

(defun x-snapshot (&key (x 0) (y 0)
                     (width 1)
                     (height 1)
                     (delay 0)
                     path)
  (format t "~%SHOT~%:~A" (list x y width height delay path))
  (sleep delay)
  (and
   (>= x 0)
   (>= y 0)
   (multiple-value-bind (display-width display-height) (x-display-size)
     (let* ((image (make-instance
                    'zpng:png
                    :width (or width (* cl-pkr::*display-size-ratio* display-width))
                    :height (or height (* cl-pkr::*display-size-ratio* display-height))
                    :color-type :truecolor-alpha))
            (data (zpng:data-array image)))
       (with-screen-dc (dc w h) ; w is the real width of full screenshot
	 (setf cl-pkr::*display-size-ratio* (/ w display-width))
	 (let* ((proper-x (if (> x w) w x))
                (proper-y (if (> y h) h y))
                (proper-width (if (> width w) w width))
                (proper-height (if (> height h) h height))
                (max-x (+ proper-x proper-width))
                (max-y (+ proper-y proper-height))
                (proper-max-x (if (> max-x w) w max-x))
                (proper-max-y (if (> max-y h) h max-y)))
           (raw-dc->png-data dc data x y (1- proper-max-x) (1- proper-max-y))))
       (if path
           (let* ((ext (pathname-type path))
                  (path (if ext path (concatenate 'string path ".png")))
                  (png? (or (null ext) (equal ext "png"))))
             (cond
               (png? (zpng:write-png image path))
               (t (error "Only PNG file is supported"))))
           image)))))

(defun x-snapshot-r (&rest rest)
  (format t "~%SHOT: ~A~%" rest))

;; (set-process-dpi-aware)
