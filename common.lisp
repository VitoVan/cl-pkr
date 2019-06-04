(defun concat (&rest rest)
  (apply #'concatenate 'string rest))

(defun pixel->color (pixel-list x y)
  (if pixel-list
      (funcall
       #'(lambda (data) (mapcar
                    #'(lambda (i) (nth i (nth x (nth y data))))
                    '(0 1 2 3)))
       pixel-list)
      '(255 255 255 255)))

(defun color-rgb-to-hsl (red green blue)
  "Convert RGB colors to their HSL representation.
RED, GREEN, and BLUE should each be numbers between 0.0 and 1.0,
inclusive.  Return a list (HUE SATURATION LUMINANCE), where
each element is between 0.0 and 1.0, inclusive."
  (let* ((r red)
         (g green)
         (b blue)
         (max (max r g b))
         (min (min r g b))
         (delta (- max min))
         (l (/ (+ max min) 2.0)))
    (if (= delta 0)
        (list 0 0 (ceiling (* 100 l)))
        (let* ((s (if (<= l 0.5) (/ delta (+ max min))
                      (/ delta (- 2.0 max min))))
               (rc (/ (- max r) delta))
               (gc (/ (- max g) delta))
               (bc (/ (- max b) delta))
               (h  (mod
                    (/
                     (cond
                       ((= r max)      (- bc gc))
                       ((= g max)      (+ 2.0 rc (- bc)))
                       (t              (+ 4.0 gc (- rc))))
                     6.0)
                    1.0)))
          (list (ceiling (* h 360)) (ceiling (* 100 s)) (ceiling (* 100 l)))))))

(defun color->strs (color)
  (setf color (subseq color  0 3))
  (list
   (apply #'format nil "#~2,'0X~2,'0X~2,'0X" color)
   (apply #'format nil "~A, ~A, ~A" color)
   (apply #'format nil "~A, ~A%, ~A%"
          (apply #'color-rgb-to-hsl
                 (mapcar #'(lambda (c) (/ c 255)) color)))))

(defun decimal->rgb (color)
  (if (> color 0)
      (let ((hex-str (concat (format nil "~X" color) "00000")))
	(loop for i from 0 to 5 by 2 collect
	     (parse-integer
	      (concatenate
	       'string
	       (string (char hex-str i))
	       (string (char hex-str (1+ i))))
	      :radix 16)))
      '(0 0 0)))

;; by Rainer Joswig @ Stack Overflow
;; https://stackoverflow.com/a/9549738
(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                  do (setf (nth n indices) j)
                  collect (if (= n depth)
                              (apply #'aref array indices)
                              (recurse (1+ n))))))
      (recurse 0))))

(defun x-copy (text)
  (ltk:with-atomic (ltk:format-wish "clipboard clear")
    (ltk:format-wish "clipboard append \"~A\"" text)))
