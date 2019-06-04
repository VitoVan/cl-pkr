(in-package #:cl-pkr)

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

;; Modified from Emacs/color.el (2018 with GPL 3.0)
;; So, I have to make the whole project under GPL 3.0, I think
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/color.el#L157
(defun color-rgb-to-hsl (red green blue)
  "Convert RGB colors to their HSL representation.
Modified from Emacs/color.el with some ceiling and multiplication call,
it's a short function, so you can compare the source yourself."
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
                 (mapcar #'(lambda (c) (/ c 255)) color)))
   (apply #'format nil "#~2,'0X~2,'0X~2,'0X"
          (mapcar #'(lambda (c) (- 255 c)) color))))

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

(defun x-copy (text)
  (with-atomic (format-wish "clipboard clear")
    (format-wish "clipboard append \"~A\"" text)))
