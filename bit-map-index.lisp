;;;; bit-map-index.lisp

(defpackage #:bit-map-index
  (:nicknames :bmi)
  (:use #:cl)
  (:export 
	   #:make-bitmap-index
	   #:add-to-index
	   #:add-to-index-extend
	   #:get-bitmap-indexes
	   #:get-bitmap
	   #:remove-from-index
	   #:n-extend-bit-map-index
	   #:*extension-factor*
	   #:bitmap-index-size
	   #:bitmap-index-key-hash))

(in-package #:bit-map-index)

;;; "bit-map-index" goes here. Hacks and glory await!

(defstruct (bitmap-index (:constructor mk-bitmap-index))
  "A BIT Map Index.  Assumes that the position in the bitmap of a 1 is
used as a reference into an array, id or other way to look up an object.
See example.lisp for a run-through of how to use "
  key-hash
  size)


(defparameter *extension-factor* 10
  "Default factor to exend bitmap-index by, i.e. 10x current size")

(defun make-bitmap-index (size &optional (test #'equal)
				 (index-type :not-compressed))
  "Makes a BIT-MAP-INDEX, TEST tests for equality in the KEY for the
BIT-MAP-INDEX,valid values of TEST are the same as for hashtables"
  (cond 
    ((eql index-type :not-compressed)
     (mk-bitmap-index :key-hash (make-hash-table :test test #+sbcl #+sbcl :synchronized t)
		      :size size))
    ((eql index-type :compressed)
     (error "Not implemented yet"))))

(defgeneric n-extend-bit-map-index (new-size bitmap-index)
  (:documentation "Destructively extends the bit-map-index, BITMAP-INDEX to the new size"))

(defmethod n-extend-bit-map-index (new-size (bitmap-index bitmap-index))
  "destructively extends bitmap index"
  (let ((hashtable (bitmap-index-key-hash bitmap-index))
	(new-hashtable (make-hash-table :test #'equal)))
    (setf (bitmap-index-size bitmap-index) new-size)
    (maphash #'(lambda (k v)
		 (setf (gethash k new-hashtable) (adjust-array v new-size)))
	     hashtable)
    (setf (bitmap-index-key-hash bitmap-index) new-hashtable)))

(defgeneric add-to-index (index-num key bmi)
  (:documentation "adds an object reference, INDEX-NUM to a BMI, a bit map index, by KEY returns the KEY if successful.  Will error if index is greater than bitmap-size.
See n-extend-bit-map-index and add-to-index-extend
"))

(defmethod add-to-index (index-num key (bmi bitmap-index))
  "adds an object reference, INDEX-NUM to a BMI, a bit map index, by KEY returns the KEY if successful"
  (with-slots (key-hash size)
      bmi
    (let ((bit-map (gethash key key-hash)))
      (log:debug size)
      (unless bit-map
	(setf bit-map (make-array (bitmap-index-size bmi) :element-type 'bit ;:adjustable t
				  )))
      (log:debug (array-total-size bit-map))
      (setf (bit bit-map index-num) 1)
      (setf (gethash key (bitmap-index-key-hash bmi))
	    bit-map))))



(defgeneric add-to-index-extend (index-num key bmi &optional extension-factor)
  (:documentation "adds an object reference, INDEX-NUM to a BMI, a bit map index, by KEY returns the KEY if successful.  Automatically resizes index by a multiple of EXTENSION-FACTOR, default factor is 10x"))

(defmethod add-to-index-extend (index-num key (bmi bitmap-index)
				&optional (extension-factor *extension-factor*))
  "adds an object reference, INDEX-NUM to a BMI, a bit map index, by KEY
returns the KEY if successful"
  (with-slots (key-hash size)
      bmi
    (if (> index-num size)
	(n-extend-bit-map-index (* extension-factor size) bmi))
    (let ((bit-map (gethash key key-hash)))
      (unless bit-map
	(setf bit-map (make-array (bitmap-index-size bmi) :element-type 'bit :adjustable t)))
      (setf (bit bit-map index-num) 1)
      (setf (gethash key (bitmap-index-key-hash bmi))
	    bit-map))))

(defgeneric remove-from-index (index-num key bmi)
  (:documentation "Remove an object reference, INDEX-NUM from a BMI, a bit map index, by KEY returns the KEY if successful"))

(defmethod remove-from-index (index-num key (bmi bitmap-index))
  "Remove an object reference, INDEX-NUM from a BMI, a bit map index, by KEY returns the KEY if successful"
  (let ((bit-map (gethash key (bitmap-index-key-hash bmi))))
    (setf (bit bit-map index-num) 0)
    (setf (gethash key (bitmap-index-key-hash bmi))
	  bit-map))
  t)

(defgeneric get-bitmap (bmi key &optional default-bit-vector)
  (:documentation "Returns the bit-map from a bit map index, BMI, by KEY, returns
DEFAULT-BIT-VECTOR or NIL if not found"))

(defmethod get-bitmap ((bmi bitmap-index) key &optional (default-bit-vector nil))
  "Returns the bit-map from a bit map index, BMI, by KEY, returns
DEFAULT-BIT-VECTOR or NIL if not found"
  (gethash key (bitmap-index-key-hash bmi)
	   default-bit-vector))


(defgeneric get-bitmap-indexes (bmi key)
  (:documentation "Returns the indexes of the KEY in the bit-map-index, BMI"))

(defmethod get-bitmap-indexes ((bmi bitmap-index) key)
  (let* ((bitmap (get-bitmap bmi key nil)))
    (if bitmap
	(mapcan #'identity
		(loop for index from 0 upto (- (length bitmap) 1)
		   collecting
		     (if (= 1 (bit bitmap index))
			 (list index)))))))

