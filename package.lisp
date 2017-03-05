;;;; package.lisp

(defpackage #:bit-map-index
  (:nicknames :bmi)
  (:use #:cl)
  (:export #:get-bitmap
	   #:get-bitmap-indexes
	   #:make-bitmap-index
	   #:add-to-index
	   #:remove-from-index
	   #:n-extend-bit-map-index
	   #:bitmap-index-size
	   #:bitmap-index-key-hash))


