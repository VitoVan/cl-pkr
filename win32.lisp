(in-package #:cl-pkr)

(define-foreign-library user32
    (:win32 (:or "user32.dll")))

(use-foreign-library user32)

(define-foreign-library gdi32
    (:win32 (:or "gdi32.dll")))

(use-foreign-library gdi32)

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

;; https://docs.microsoft.com/en-us/windows/desktop/api/wingdi/nf-wingdi-bitblt
;; http://www.jasinskionline.com/windowsapi/ref/b/bitblt.html
;; Const SRCCOPY = &HCC0020
;; HEX Value CC0020 -> Decimal Value 13369376
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

(defun x-display-size ()
  (values
   (screen-width)
   (screen-width)))

(defmacro with-screen-dc ((dc width height) &body body)
  `(let* ((screen-dc (get-dc (null-pointer)))
	  (,dc (create-compatible-dc screen-dc))
	  (screen-size (multiple-value-list (x-display-size)))
	  (,width (car screen-size))
	  (,height (cadr screen-size))
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

(defun raw-dc->pixel-list (raw-dc x0 y0 x1 y1)
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
              (list b g r a)))))

(defun x-snapshot (&key (x 0) (y 0)
                     (width 1)
                     (height 1)
                     (offset 0))
  (and
   (>= x 0)
   (>= y 0)
   (with-screen-dc (dc w h) ; w is the real width of full screenshot
     (setf x (- x offset) y (- y offset))
     (let* ((proper-x (if (> x w) w (if (> x 0) x 0)))
	    (proper-y (if (> y h) h (if (> y 0) y 0)))
	    (proper-width (if (> width w) w width))
	    (proper-height (if (> height h) h height))
	    (max-x (+ proper-x proper-width))
	    (max-y (+ proper-y proper-height))
	    (proper-max-x (if (> max-x w) w max-x))
	    (proper-max-y (if (> max-y h) h max-y)))
       (raw-dc->pixel-list
	dc
	proper-x
	proper-y
	(1- proper-max-x)
	(1- proper-max-y))))))
