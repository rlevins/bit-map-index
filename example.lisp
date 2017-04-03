;;;; example.lisp
;;;; Full working example for bit-map-index library


(defpackage :bmi-example
  (:use :cl :bit-map-index))

(in-package :bmi-example)
;; #<Package "BMI-TEST">


;; An Array of objects/rows, e.g. support tickets
;; 'account', 'status', 'prirority' 'assignee'

;; Make a data set
(defparameter *db* (make-array 10 
		       :initial-contents
		       '(("InGen" "closed" "normal" "Bob Tomato")
			 ("Initech" "open" "normal" "Larry Cucumber")
			 ("Charles Dickens, Inc." "closed" "high" "Dogbert")
			 ("Wally World" "closed" "normal" "Jack Torrance")
			 ("Multi-National United" "closed" "normal" "Agent Smith")
			 ("BiffCo" "closed" NIL "Loki")
			 ("RDA Corporation" "closed" "low" "Thor")
			 ("Initech" "closed" "high" "John Doe")
			 ("Weyland-Yutani Corporation" "closed" "high" "Jane Doe")
			 ("Weyland-Yutani Corporation" "open" "normal" "Bob Tomato")) 
		       :fill-pointer t))


;;  #(("InGen" "closed" "normal" "Bob Tomato") ("Initech" "open" "normal" "Larry Cucumber") ("Charles Dickens, Inc." "closed" "high" "Dogbert") ("Wally World" "closed" "normal" "Jack Torrance") ("Multi-National United" "closed" "normal" "Agent Smith") ("BiffCo" "closed" NIL "Loki") ("RDA Corporation" "closed" "low" "Thor") ("Initech" "closed" "high" "John Doe") ("Weyland-Yutani Corporation" "closed" "high" "Jane Doe") ("Weyland-Yutani Corporation" "open" "normal" "Bob Tomato"))


;; make a bit-map-indexes for two columns
(defparameter *bmi-status* (make-bitmap-index 10 #'equal))

;;#S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQL size 0/60 #x30203DF6FDAD> :SIZE 10)

(defparameter *bmi-priority* (make-bitmap-index 10 #'equal))

;;#S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQL size 0/60 #x30203E13F4FD> :SIZE 10)


;; Creating a BIT-MAP-INDEX for the above:
(dotimes (i (fill-pointer *db*))
       (add-to-index i (nth 1 (aref *db* i)) *bmi-status*))
;; NIL

(dotimes (i (fill-pointer *db*))
       (add-to-index i (nth 2 (aref *db* i)) *bmi-priority*))
;; NIL

*bmi-status*
;; #S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQUAL size 2/60 #x30203DF95D5D> :SIZE 10)

(bitmap-index-key-hash *bmi-status*)
;; slime inspect the hash-table

;; #<HASH-TABLE #x30203DF95D5D>
;; --------------------
;; Count: 2
;; Size: 60
;; Test: EQUAL
;; Rehash size: 1.5
;; Rehash threshold: 0.85
;; [clear hashtable]
;; Contents: 
;; "closed" = #*1011111110 [remove entry]
;; "open" = #*0100000001 [remove entry]

;; Underlying UVECTOR


*bmi-priority*
;; #S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQUAL size 4/60 #x30203DF9574D> :SIZE 10)

(bitmap-index-key-hash *bmi-priority*)
;; #<HASH-TABLE #x30203DF9574D>
;; --------------------
;; Count: 4
;; Size: 60
;; Test: EQUAL
;; Rehash size: 1.5
;; Rehash threshold: 0.85
;; [clear hashtable]
;; Contents: 
;; NIL        = #*0000010000 [remove entry]
;; "high"   = #*0010000110 [remove entry]
;; "low"    = #*0000001000 [remove entry]
;; "normal" = #*1101100001 [remove entry]

;; Underlying UVECTOR


;;; Retrieve a bitmap for a key ... note the default-bv gets
;;; returned if the key is not found.  It should be a bitvector
;;; of all 1s or all 0s as appropriate
(get-bitmap *bmi-status* "closed" (make-array (bmi:bitmap-index-size *bmi-status*)
						    :element-type 'bit :adjustable t))
;;  #*1011111110

(get-bitmap *bmi-status* "key-wont-be-found" (make-array (bmi:bitmap-index-size *bmi-status*)
							 :element-type 'bit :adjustable t))
;; #*0000000000



;; in our example, we can map through the bit-map and lookup the 'answer'
;; based on the position of the 1s


(let ((answer nil)
      (bit-vector (get-bitmap *bmi-status*
			      "closed"
			      (make-array (bmi:bitmap-index-size *bmi-status*)
					  :element-type 'bit :adjustable t))))
    (dotimes (i (fill-pointer *db*))
      (if (= 1 (bit bit-vector i))
	  (push (aref *db* i) answer)))
    (reverse answer))

;; (("InGen" "closed" "normal" "Bob Tomato")
;;  ("Charles Dickens, Inc." "closed" "high" "Dogbert")
;;  ("Wally World" "closed" "normal" "Jack Torrance")
;;  ("Multi-National United" "closed" "normal" "Agent Smith")
;;  ("BiffCo" "closed" NIL "Loki")
;;  ("RDA Corporation" "closed" "low" "Thor")
;;  ("Initech" "closed" "high" "John Doe")
;;  ("Weyland-Yutani Corporation" "closed" "high" "Jane Doe"))




;; Alternatively, you can retrieve the indexes
(get-bitmap-indexes *bmi-status* "closed")
;; (0 2 3 4 5 6 7 8)


(loop for index in (get-bitmap-indexes *bmi-status* "closed")
       collecting (aref *db* index))
;; (("InGen" "closed" "normal" "Bob Tomato")
;;  ("Charles Dickens, Inc." "closed" "high" "Dogbert")
;;  ("Wally World" "closed" "normal" "Jack Torrance")
;;  ("Multi-National United" "closed" "normal" "Agent Smith")
;;  ("BiffCo" "closed" NIL "Loki")
;;  ("RDA Corporation" "closed" "low" "Thor")
;;  ("Initech" "closed" "high" "John Doe")
;;  ("Weyland-Yutani Corporation" "closed" "high" "Jane Doe"))



;; Finally, because they are bit-maps, you can do complex queries
;; Such as, all items with 'status' 'closed' *and* 'priority' 'high'

(let* ((answer nil)
	    (closed-bv (get-bitmap *bmi-status* "closed"
				   (make-array (bmi:bitmap-index-size *bmi-status*)
					       :element-type 'bit :adjustable t)))
	    (high-bv (get-bitmap *bmi-priority* "high"
				 (make-array (bmi:bitmap-index-size *bmi-priority*)
					     :element-type 'bit :adjustable t) ))
	    (combined-bv (bit-and closed-bv high-bv)))
       (dotimes (i (fill-pointer *db*))
	 (if (= 1 (bit combined-bv i))
	     (push (aref *db* i) answer)))
       (reverse answer))
;;  (("Charles Dickens, Inc." "closed" "high" "Dogbert")
;;   ("Initech" "closed" "high" "John Doe")
;;   ("Weyland-Yutani Corporation" "closed" "high" "Jane Doe"))

