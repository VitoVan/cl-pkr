(in-package :ltk)

(defparameter *wish-lock* (bordeaux-threads:make-recursive-lock "WISH-LOCK"))

(defun flush-wish ()
  (bordeaux-threads:with-recursive-lock-held (*wish-lock*)
    (format t "~%FLUSHED: -------------------~%")
    (let ((buffer (nreverse (wish-output-buffer *wish*))))
      (when buffer
        (let ((len (loop for s in buffer summing (length s)))
              (*print-pretty* nil)
              (stream (wish-stream *wish*)))
          (declare (stream stream))
          (incf len (length buffer))
          (setf (wish-output-buffer *wish*) nil)
          (handler-bind ((stream-error (lambda (e) (handle-dead-stream e stream)))
                         #+lispworks
                         (comm:socket-error (lambda (e) (handle-dead-stream e stream)))
                         )
            (cond
              ((wish-remotep *wish*)
               (let ((content (format nil "~{~a~%~}" buffer)))
                 (format stream "~d ~a~%"(length content) content)
                 (dbg "~d ~a~%" (length content) content)))
              (*max-line-length*
               (when (or *debug-buffers*
                         *debug-tk*)
                 (format t "buffer size ~a~%" len) (finish-output))

               (dolist (string buffer)
                 (loop while (> (length string) *max-line-length*)
                    do
                      (let ((sub (subseq string 0 *max-line-length*)))
                        (setf string (subseq string *max-line-length*))
                        (format stream "bt \"~A\"~%" (tkescape2 sub))
                        (dbg "bt \"~A\"~%" (tkescape2 sub))))
                 (format stream "bt \"~A~%\"~%" (tkescape2 string))
                 (dbg "bt \"~A\"~%" (tkescape2 string)))
               (format stream "process_buffer~%")
               (dbg "process_buffer~%")
               )
              (t
               (format stream "bt {~D }~%" len)
               (dbg "bt {~D }~%" len)
               (dolist (string buffer)
                 (format stream "bt \"~A~%\"~%" (tkescape2 string))
                 (dbg "bt \"~A\"~%" (tkescape2 string)))
               (format stream "process_buffer~%")
               (dbg "process_buffer~%")))

            (finish-output stream)

            #+nil(loop for string in buffer
                    do (loop with end = (length string)
                          with start = 0
                          for amount = (min 1024 (- end start))
                          while (< start end)
                          do (let ((string (subseq string start (+ start amount))))
                               (format stream "buffer_text {~A}~%" string)
                               (dbg "buffer_text {~A}~%" string)
                               (incf start amount)))
                      (format stream "buffer_text \"\\n\"~%")
                      (dbg "buffer_text \"\\n\"~%")
                    finally (progn (format stream "process_buffer~%")
                                   (dbg "process_buffer~%")
                                   (finish-output stream)))

            (setf (wish-output-buffer *wish*) nil)))))))

(defun send-wish (text)
  (bordeaux-threads:with-recursive-lock-held (*wish-lock*)
    (format t "~%SEND:~%~A" text)
    (push text (wish-output-buffer *wish*))
    (unless *buffer-for-atomic-output*
      (flush-wish))))
