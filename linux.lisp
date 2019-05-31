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

(defun raw-image->png (data width height)
  (let* ((png (make-instance 'zpng:png :width width :height height
			     :color-type :truecolor-alpha
			     :image-data data))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
	;; diffs between RGB and BGR: https://goo.gl/si1Ft5
	(rotatef (aref data y x 0) (aref data y x 2))
	(setf (aref data y x 3) 255)))
    png))

(defun x-snapshot (&key (x 0) (y 0)
                     (width 1)
                     (height 1)
                     (delay 0)
                     path)
  (sleep delay)
  (and
   (>= x 0)
   (>= y 0)
   (multiple-value-bind (display-width display-height) (x-display-size)
     (with-default-window (w)
       (let ((image
              (raw-image->png
               (xlib:get-raw-image w :x x :y y
                              :width (or width display-width)
                              :height (or height display-height)
                              :width width
                              :height height
                              :format :z-pixmap)
               width height)))
         (if path
             (let* ((ext (pathname-type path))
                    (path (if ext path (concatenate 'string path ".png")))
                    (png? (or (null ext) (equal ext "png"))))
               (cond
                 (png? (zpng:write-png image path))
                 (t (error "Only PNG file is supported"))))
             image))))))

(defun x-copy (text)
  (run/ss (format nil "bash -c \"echo -n '~A' | xsel --clipboard --input\"" text)))
