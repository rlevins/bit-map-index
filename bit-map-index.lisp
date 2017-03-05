;;;; bit-map-index.lisp

(in-package #:bit-map-index)

;;; "bit-map-index" goes here. Hacks and glory await!

(defstruct (bitmap-index (:constructor mk-bitmap-index ))
  "A BIT Map Index.  Assumes that the position in the bitmap of a 1 is
used as a reference into an array, id or other way to look up an object.
See example.lisp for a run-through of how to use "
  key-hash
  size)

(defun make-bitmap-index (size &optional (test #'equal))
  "Makes a BIT-MAP-INDEX, TEST tests for equality in the KEY for the BIT-MAP-INDEX"
  (mk-bitmap-index :key-hash (make-hash-table :test test #+sbcl #+sbcl :synchronized t)
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

(defun add-to-index (index-num key bmi)
  "adds an object reference, INDEX-NUM to a BMI, a bit map index, by KEY returns the KEY if successful"
  
  (let ((bit-map (gethash key (bitmap-index-key-hash bmi))))
    (unless bit-map
      (setf bit-map (make-array (bitmap-index-size bmi) :element-type 'bit :adjustable t)))
    (setf (bit bit-map index-num) 1)
    (setf (gethash key (bitmap-index-key-hash bmi))
	  bit-map)))

(defun remove-from-index (index-num key bmi)
  "Remove an object reference, INDEX-NUM from a BMI, a bit map index, by KEY returns the KEY if successful"
  (let ((bit-map (gethash key (bitmap-index-key-hash bmi))))
    (setf (bit bit-map index-num) 0)
    (setf (gethash key (bitmap-index-key-hash bmi))
	  bit-map)))

(defun get-bitmap (bmi key default-bit-vector)
  "Returns the bit-map from a bit map index, BMI, by KEY, returns
DEFAULT-BIT-VECTOR if not found"
  (copy-seq (gethash key (bitmap-index-key-hash bmi)
	    default-bit-vector)))

(defun get-bitmap-indexes (bmi key)
  (let* ((default-bv (make-array (bmi:bitmap-index-size bmi)
				:element-type 'bit :adjustable t))
	(bitmap (get-bitmap bmi key default-bv))
	(answer nil))
    (dotimes (i (length bitmap))
      (if (= 1 (bit bitmap i))
	  (push i answer)))
    (reverse answer)))



;; (bit-and
;;  (bit-io (get-bit-map bmi-1 key-val1)))



#|
bit-and bit-array1 bit-array2 &optional result-bit-array 
bit-ior bit-array1 bit-array2 &optional result-bit-array 
bit-xor bit-array1 bit-array2 &optional result-bit-array 
bit-eqv bit-array1 bit-array2 &optional result-bit-array 
bit-nand bit-array1 bit-array2 &optional result-bit-array 
bit-nor bit-array1 bit-array2 &optional result-bit-array 
bit-andc1 bit-array1 bit-array2 &optional result-bit-array 
bit-andc2 bit-array1 bit-array2 &optional result-bit-array 
bit-orc1 bit-array1 bit-array2 &optional result-bit-array 
bit-orc2 bit-array1  bit-array2 &optional result-bit-array

:and
:ior :xor
:eqv
:nand :nor
:andc1 :andc2
:orc1  :orc2

(defun query-bit-map (query)
  "Queries a bit-map index for binary matches
  (:and
     (bmi-1 (:ior (is key-a key-val1)
              (is key-a key-val2))
    "
  (declare (ignore query)))


'(:and
       (with-bmi (bmi-1)
	 (:or key-val1
	      key-val2))
       (with-bmi (bmi-2)
	 (:not key-val3)))

|#
