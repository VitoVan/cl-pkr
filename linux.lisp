(ql:quickload :clx)

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (unwind-protect
               ,@body
            (when ,force
              (xlib:display-force-output ,display)))
       (xlib:close-display ,display))))

(defmacro with-default-display-force ((display) &body body)
  `(with-default-display (,display :force t) ,@body))

(defmacro with-default-screen ((screen) &body body)
  (let ((display (gensym)))
    `(with-default-display (,display)
       (let ((,screen (xlib:display-default-screen ,display)))
         ,@body))))

(defmacro with-default-window ((window) &body body)
  (let ((screen (gensym)))
    `(with-default-screen (,screen)
       (let ((,window (xlib:screen-root ,screen)))
         ,@body))))

(defun x-display-size ()
  (with-default-screen (s)
    (values
     (xlib:screen-width s)
     (xlib:screen-height s))))

(defun raw-image->png (raw-data width height)
  (let* ((raw-len (length raw-data))
         (full-len (* width height 4))
         (filled-data
          (concatenate
           '(vector (unsigned-byte 8))
           raw-data
           (make-array (- full-len raw-len) :initial-element 255)))
         (png (make-instance 'zpng:png
                             :width width
                             :height height
                             :image-data filled-data
                             :color-type :truecolor-alpha))
         (png-data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref png-data y x 0) (aref png-data y x 2))
        (setf (aref png-data y x 3) 255)))
    png))

(defun x-snapshot (&key (x 0) (y 0)
                     (width 1)
                     (height 1)
                     (delay 0)
                     (offset 0)
                     path)
  (sleep delay)
  (and
   (>= x 0)
   (>= y 0)
   (multiple-value-bind (w h) (x-display-size)
     (with-default-window (window)
       (setf x (- x offset) y (- y offset))
       (let* ((proper-x (if (> x w) w (if (> x 0) x 0)))
              (proper-y (if (> y h) h (if (> y 0) y 0)))
              (proper-width (if (> width w) w width))
              (proper-height (if (> height h) h height))
              (max-x (+ proper-x proper-width))
              (max-y (+ proper-y proper-height))
              (proper-max-x (if (> max-x w) w max-x))
              (proper-max-y (if (> max-y h) h max-y))
              (image (progn
                       (raw-image->png
                        (xlib:get-raw-image window :x proper-x :y proper-y
                                            :width (- proper-max-x proper-x)
                                            :height (- proper-max-y proper-y)
                                            :format :z-pixmap)
                        width height))))
         (if path
             (let* ((ext (pathname-type path))
                    (path (if ext path (concatenate 'string path ".png")))
                    (png? (or (null ext) (equal ext "png"))))
               (cond
                 (png? (zpng:write-png image path))
                 (t (error "Only PNG file is supported"))))
             image)
         image)))))

(defun x-copy (text)
  (run/ss (format nil "bash -c \"echo -n '~A' | xsel --clipboard --input\"" text)))
