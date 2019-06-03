(ql:quickload :cffi)
(use-package :cffi)

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

(defun x-display-size ()
  (values
   (ltk:screen-width)
   (ltk:screen-width)))

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
                (setf (aref png-data png-y png-x 0) b
                      (aref png-data png-y png-x 1) g
                      (aref png-data png-y png-x 2) r
                      (aref png-data png-y png-x 3) a))))))

(defun x-snapshot (&key (x 0) (y 0)
                     (width 1)
                     (height 1)
                     (delay 0)
                     path)
  (format t "SHOT:~A~%" (list x y width height delay path))
  (sleep delay)
  (and
   (>= x 0)
   (>= y 0)
   (multiple-value-bind (display-width display-height) (x-display-size)
     (let* ((image (make-instance
                    'zpng:png
                    :width (or width display-width)
                    :height (or height display-height)
                    :color-type :truecolor-alpha))
            (data (zpng:data-array image)))
       (with-screen-dc (dc w h) ; w is the real width of full screenshot
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

(defcfun ("OpenClipboard" open-clipboard) :boolean (hWndNewOwner :pointer))
(defcfun ("CloseClipboard" close-clipboard) :boolean)
(defcfun ("EmptyClipboard" empty-clipboard) :boolean)
(defcfun ("SetClipboardData" set-clipboard-data) :pointer (u-format :int) (h-mem :pointer))
(defcfun ("GlobalAlloc" global-alloc) :pointer (u-flags :uint32) (dw-bytes :uint32))
(defcfun ("GlobalLock" global-lock) :pointer (h-mem :pointer))
(defcfun ("GlobalUnlock" global-unlock) :pointer (h-mem :pointer))
(defcfun ("memcpy" memcpy) :void (dest :pointer) (src :pointer) (count :uint32))
(defcfun ("strlen" strlen) :uint32 (str :pointer))

;; https://docs.microsoft.com/en-us/windows/desktop/dataxchg/standard-clipboard-formats
;; Constant: CF_TEXT, Value: 1, Description: Text format.
(defparameter *CF_TEXT* 1)

;; https://docs.microsoft.com/en-us/windows/desktop/api/winbase/nf-winbase-globalalloc
;; Constant: GMEM_MOVEABLE, Value: 0x0002, Description: Allocates movable memory.
(defparameter *GMEM_MOVEABLE* 2)

(defun x-copy (text)
  (unwind-protect
       (with-foreign-string (h-mem text)
	 (let* ((len (1+ (strlen h-mem)))
		(g-h-mem (global-alloc *GMEM_MOVEABLE* len)))
	   (memcpy (global-lock g-h-mem) h-mem len)
	   (global-unlock g-h-mem)
	   (open-clipboard (null-pointer))
	   (empty-clipboard)
	   (set-clipboard-data *CF_TEXT* g-h-mem)))
    (close-clipboard)))
