rm -f bin/color-picker
sbcl --disable-debugger --load cl-pkr.asd --eval "(ql:quickload 'cl-pkr)" --eval "(asdf:make :cl-pkr)"
