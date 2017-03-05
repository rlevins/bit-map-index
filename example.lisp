(defpackage :bmi-test
  (:use :cl :bit-map-index))

(in-package :bmi-test)
;; #<Package "BMI-TEST">


;; An Array of objects/rows, e.g. support tickets
;; 'account', 'status', 'prirority' 'assignee'

;; Make a data set
(setf *db* (make-array 10 
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
(setf *bmi-status* (make-bitmap-index 10 #'equal))

;;#S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQL size 0/60 #x30203DF6FDAD> :SIZE 10)

(setf *bmi-priority* (make-bitmap-index 10 #'equal))

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


(loop for index in (get-bitmap-indexes *bmi* :first )
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



;;;  run length huffman encodings


(setf *db* (make-array 19 :initial-contents (list 
		      :male :female :female :female
		      :male :male :male :female
		      :female :male :male :male
		      :female :female :female :male
		      :female :female :female) :fill-pointer t))


(setf *clients* (make-bitmap-index 19 #'eql))




(defun rlh (bitmap)
  (let ((counter 0)
	(encoded nil))
    (map nil
	 #'(lambda (bit)
	     (case bit
	       (0 (incf counter))
	       (1 (setf encoded
			(push counter encoded))
		  (setf counter 0))))
	 bitmap)
    (nreverse
     (if (> counter 0)
	 (push counter encoded)
	 encoded))))

(loop for bitmap being the hash-values of (bitmap-index-key-hash *clients*)
	     collecting (rlh bitmap))

(defun freqs (rlh-bitmaps)
  (let ((freqs nil))
    (macrolet ((count-place (count)
		 `(assoc ,count freqs :test #'=)))
     (map nil #'(lambda (rlh-bitmap)
		  (map nil #'(lambda (count)
			       (if (count-place count) 
				   (incf (cdr (count-place count)))
				   (push (cons count 1) freqs)))
		       rlh-bitmap))
	  rlh-bitmaps))
    (sort freqs #'> :key #'cdr )))

(freqs (loop for bitmap being the hash-values of (bitmap-index-key-hash *clients*)
		    collecting (rlh bitmap)))
;Compiler warnings :
;   In an anonymous lambda form: Undeclared free variable *CLIENTS*
;;  ((0 . 12) (3 . 5) (1 . 2) (2 . 1))



(defstruct tree-node 
	   lh-node
	   rh-node)

(defun huffman-tree (freqs)
  (let ((rev-freqs (reverse freqs)))
    (do ((rh-node (car rev-freqs))
	 (rev-freqs (cdr (cdr rev-freqs))
		    (cdr rev-freqs))
	 (lh-node (car rev-freqs)
		  (car rev-freqs))
	 (done-p nil))
	(done-p rh-node)
      (format t "rev-freqs:~a~%lh-node: ~a ~%rh-node: ~a~%"
	      rev-freqs lh-node rh-node)
      (setf rh-node (make-tree-node :rh-node  rh-node
				    :lh-node  lh-node))
      (setf done-p (null rev-freqs)))))

(huffman-tree
 (freqs
  (loop for bitmap being the hash-values of (bitmap-index-key-hash *clients*)
     collecting (rlh bitmap))))

;Compiler warnings :
;   In an anonymous lambda form: Undeclared free variable *CLIENTS*
;; rev-freqs:((3 . 5) (0 . 12))
;; lh-node: (2 . 1) 
;; rh-node: (2 . 1)
;; rev-freqs:((0 . 12))
;; lh-node: (3 . 5) 
;; rh-node: #S(TREE-NODE :LH-NODE (2 . 1) :RH-NODE (2 . 1))
;; rev-freqs:NIL
;; lh-node: (0 . 12) 
;; rh-node: #S(TREE-NODE :LH-NODE (3 . 5) :RH-NODE #S(TREE-NODE :LH-NODE (2 . 1) :RH-NODE (2 . 1)))


;; #S(TREE-NODE :LH-NODE (0 . 12) :RH-NODE #S(TREE-NODE :LH-NODE (3 . 5) :RH-NODE #S(TREE-NODE :LH-NODE (2 . 1) :RH-NODE (2 . 1))))



(defun huffman-encodings (huffman-tree)
  ;;  ()
  ;;  Need to calculate huffman-encodings from a tree
  ;; (let ((visited nil)
  ;; 	(path nil))
  ;;   (do ((rh-node (tree-node-rh-node huffman-tree)
  ;; 		  (tree-node-rh-node huffman-tree)))
  ;; 	((tree-node-p rh-node) rh-node)
  ;;     (setf path (push 1 path))))
  )


