;;;; bit-map-index.lisp

(in-package #:bit-map-index)

;;; "bit-map-index" goes here. Hacks and glory await!

(in-package :bit-map-index)

(defstruct (bitmap-index (:constructor mk-bitmap-index ))
  key-hash
  size)

(defun make-bitmap-index (size)
  (mk-bitmap-index :key-hash (make-hash-table :test #'equal)
		   :size size))

(defun n-extend-bit-map-index (new-size bit-map-index)
  "destructively extends bitmap index"
  (let ((hashtable (bitmap-index-key-hash bit-map-index))
	(new-hashtable (make-hash-table :test #'equal)))
    (setf (bitmap-index-size bit-map-index) new-size)
    (maphash #'(lambda (k v)
		 (setf (gethash k new-hashtable) (adjust-array v new-size)))
	     hashtable)
    (setf (bitmap-index-key-hash bit-map-index) new-hashtable)))

(defun add-to-index (index key bmi)
  "adds an object to an index returns the object if successful"
  (let ((bit-map (gethash key (bitmap-index-key-hash bmi))))
    (unless bit-map
      (setf bit-map (make-array (bitmap-index-size bmi) :element-type 'bit :adjustable t)))
    (setf (bit bit-map index) 1)
    (setf (gethash key (bitmap-index-key-hash bmi))
	  bit-map)))

(defun get-bitmap (bmi key default-bit-vector)
  (copy-seq (gethash key (bitmap-index-key-hash bmi)
	    default-bit-vector)))
