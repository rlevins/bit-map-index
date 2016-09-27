;;;; package.lisp

(defpackage #:bit-map-index
  (:nicknames :bmi)
  (:use #:cl)
  (:export #:get-bitmap
	   #:make-bitmap-index
	   #:add-to-index
	   #:n-extend-bit-map-index
	   #:bitmap-index-size))


