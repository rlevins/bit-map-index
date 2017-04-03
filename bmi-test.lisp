(defpackage #:bit-map-index/bmi-test
  ;;(:nicknames :bmi-test)
  (:use #:cl #:fiveam #:bit-map-index)
  (:export  #:run-my-tests))

(in-package :bit-map-index/bmi-test)

(defun run-my-tests ()
  (fiveam:run-all-tests ))


(def-suite :bit-map-index-tests)


(in-suite :bit-map-index-tests)

(defparameter *db* (make-array 10 
			       :initial-contents
			       '(("InGen" "closed" "normal" "Bob Tomato")
				 ("Initech" "open" "normal" "Larry Cucumber")
				 ("Charles Dickens, Inc." "closed" "high"
				  "Dogbert")
				 ("Wally World" "closed" "normal" "Jack Torrance")
				 ("Multi-National United" "closed" "normal"
				  "Agent Smith")
				 ("BiffCo" "closed" NIL "Loki")
				 ("RDA Corporation" "closed" "low" "Thor")
				 ("Initech" "closed" "high" "John Doe")
				 ("Weyland-Yutani Corporation" "closed" "high"
				  "Jane Doe")
				 ("Weyland-Yutani Corporation" "open" "normal"
				  "Bob Tomato")) 
			       :fill-pointer t))
(defparameter *bmi-status* nil)

(test api-make-bitmap-index
  (is (equal (type-of  (make-bitmap-index 100 #'eql))
	     (class-name
	      (find-class 'bit-map-index::bitmap-index))))
  (is (equal (type-of  (make-bitmap-index 100 #'eql :not-compressed))
	     (class-name
	      (find-class 'bit-map-index::bitmap-index))))
  (setf *bmi-status* (make-bitmap-index 10 #'equal)))


(test (api-add-to-index :depends-on api-make-bitmap-index)
    ;;;  Adds the values from the DB into the bitmap-index
  (dotimes (i (fill-pointer *db*))
    (add-to-index i (nth 1 (aref *db* i)) *bmi-status*)))

(test (api-get-bitmap :depends-on api-add-to-index)
  (is (equal #*1011111110
	     (get-bitmap *bmi-status* "closed"
			 (make-array (bmi:bitmap-index-size *bmi-status*)
				     :element-type 'bit :adjustable t))))
  (is (equal #*0000000000
	     (get-bitmap *bmi-status* "key-wont-be-found"
			 (make-array (bmi:bitmap-index-size *bmi-status*)
				     :element-type 'bit :adjustable t)))))

(test (api-get-bitmap-indexes :depends-on api-add-to-index)
  (is (equal  '(0 2 3 4 5 6 7 8)
	      (get-bitmap-indexes *bmi-status* "closed"))))

(test (api-remove-from-indexes :depends-on api-get-bitmap-indexes)
  (is (equal t
	     (remove-from-index 8 "closed" *bmi-status*)))
  (is (equal  '(0 2 3 4 5 6 7)
	      (get-bitmap-indexes *bmi-status* "closed"))))

(test (api-bitmap-index-size :depends-on api-make-bitmap-index)
  (is (= 10 (bitmap-index-size *bmi-status*))))

(test (api-bitmap-index-key-hash :depends-on api-bitmap-index-size)
  (is (equal  (class-name (find-class 'hash-table))
	     (type-of (bitmap-index-key-hash *bmi-status*)))))


(test (api-n-extend-bitmap-index :depends-on api-make-bitmap-index)
  (is (= 10 (bitmap-index-size *bmi-status*)))
  (bmi::n-extend-bit-map-index 20 *bmi-status*)
  (is (= 20 (bitmap-index-size *bmi-status*)))
  (loop for bitmap being the hash-values of (bitmap-index-key-hash *bmi-status*)
     do
       (is (= 20 (array-total-size bitmap)))))

(test (keys-present-test :depends-on api-make-bitmap-index)
  (is (equal '("open" "closed")
	     (loop for key being the hash-keys of (bitmap-index-key-hash *bmi-status*)
		collecting key))))

