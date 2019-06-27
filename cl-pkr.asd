#+sbcl (sb-ext:unlock-package :sb-ext)
#+sbcl
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   #+sb-core-compression :compression
                   #+sb-core-compression 9
                   #+win32 :application-type #+win32 :gui))

(asdf:defsystem #:cl-pkr
  :description "Cross Platform Color Picker written in Common Lisp"
  :author "Vito Van"
  :license "GPL-3.0"
  :version "1.0.0"
  :depends-on (#:ltk
               #+(or linux darwin) #:unix-opts
               #+(or win32 darwin) #:cffi
               #+linux #:clx)
  :serial t
  :components ((:file "package")
               (:file "common")
               #+darwin (:file "darwin")
               #+linux (:file "linux")
               #+win32 (:file "win32")
               (:file "cl-pkr"))
  :build-operation "program-op"
  :build-pathname "bin/color-picker"
  :entry-point "cl-pkr:color-picker")
