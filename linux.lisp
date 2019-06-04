(ql:quickload :clx)

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (unwind-protect
               (progn ,@body)
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

(defun raw-data->pixel-list (raw-data x0 y0 x1 y1 w)
  (loop for _y from 0 to (- y1 y0)
     collect
       (loop for _x from 0 to (- x1 x0)
          collect
            (let* ((cell-i (+ (* w _y)_x))
                   (value-i (* cell-i 4))
                   (r (aref raw-data value-i))
                   (g (aref raw-data (incf value-i)))
                   (b (aref raw-data (incf value-i)))
                   (a (aref raw-data (incf value-i))))
              (list b g r a)))))

(defun x-snapshot (&key (x 0) (y 0)
                     (width 1)
                     (height 1)
                     (offset 0))
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
              (proper-max-y (if (> max-y h) h max-y)))
         (raw-data->pixel-list
          (xlib:get-raw-image window :x proper-x :y proper-y
                              :width (- proper-max-x proper-x)
                              :height (- proper-max-y proper-y)
                              :format :z-pixmap)
          proper-x
          proper-y
          (1- proper-max-x)
          (1- proper-max-y)
          (- proper-max-x proper-x)))))))
