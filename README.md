# bit-map-index

A common lisp library that provides a bit-mapped index. See the file `example.lisp` for a working example of usage. 

```lisp
;; Do this to setup to follow the examples
(defpackage :bmi-test
  (:use :cl :bit-map-index))

(in-package :bmi-test)
;; #<Package "BMI-TEST">


;; An Array of objects/rows, e.g. support tickets
;;       'account', 'status', 'prirority' 'assignee'
;; i.e. ("InGen"    "closed   "normal"    "Bob Tomato")

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




```



## Quick usage notes

*  Not in quicklisp yet, so for now, clone the repo to your local lisp projects directory, then use `(ql:quickload :bit-map-index)` to load the project.
*  Assumes that for a given field in a table or array of objects, the values of the field will be used as `KEY`s to a bitmap for that value of the KEY, and the bitmap stores an integer reference e.g. table-row number or object id.
* Given a key, index number and a bit-map-index, `add-to-index` stores the index-number, and an object-id or array-index will be stored by the bitmap as a 1 in a bitmap for the key `key`
* Stores individual bit-maps for a given `KEY` as values in a hashtable with lookup by the `KEY`
* Does not check bounds, so before adding an index number, check that the index number is less than `bitmap-index-size` if it is larger, call `n-extend-bit-map-index` to increase the length of the bitmap to `new-size` 

## Structure

### bitmap-index  		 [Structure]
A BIT Map Index. Assumes that the position in the bitmap of a 1 is used as a reference into an array, id or other way to look up an object. See example.lisp for a run-through of how to use

## API

### make-bitmap-index
Makes a BIT-MAP-INDEX, TEST tests for equality in the KEY for the BIT-MAP-INDEX, valid values of TEST are the same as for hashtables


lambda list : `(SIZE &optional (test #'equal) )`

 * SIZE - Sets the initial size of the bitmap index
 * TEST - The test for equality on the `KEY` provided, must be a valid test for a hash-table

```lisp
;; (make-bitmap-index size &optional (test #'equal) )
EG:

;; make a bit-map-indexes for two columns
(setf *bmi-status* (make-bitmap-index 10 #'equal))

;;#S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQL size 0/60 #x30203DF6FDAD> :SIZE 10)

(setf *bmi-priority* (make-bitmap-index 10 #'equal))

;;#S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQL size 0/60 #x30203E13F4FD> :SIZE 10)


```


### add-to-index
adds an object reference, INDEX-NUM to a BMI, a bit map index, by KEY returns the KEY if successful. Will error if index is greater than bitmap-size.
See n-extend-bit-map-index and add-to-index-extend

lambda list: `(index-num key bmi &optional extension-factor)`

```lisp
;; Assuming you've been following the example
;; Creating a BIT-MAP-INDEX for the above:
(dotimes (i (fill-pointer *db*))
       (add-to-index i (nth 1 (aref *db* i)) *bmi-status*))
;; NIL

(dotimes (i (fill-pointer *db*))
       (add-to-index i (nth 2 (aref *db* i)) *bmi-priority*))
;; NIL

*bmi-status*
;; #S(BITMAP-INDEX :KEY-HASH #<HASH-TABLE :TEST EQUAL size 2/60 #x30203DF95D5D> :SIZE 10)
```

### n-extend-bit-map-index  [Generic function]
lambda list: `(new-size bitmap-index)`	 
Destructively extends the bit-map-index, BITMAP-INDEX to the new size


### \*extension-factor\*  		 [Variable]
Default factor to exend bitmap-index by, i.e. 10x current size


### add-to-index-extend  	index-num key bmi &optional extension-factor	 [Generic function]
adds an object reference, INDEX-NUM to a BMI, a bit map index, by KEY returns the KEY if successful. Automatically resizes index by a multiple of EXTENSION-FACTOR, default factor is 10x



### remove-from-index  	   [Generic function]
Remove an object reference, INDEX-NUM from a BMI, a bit map index, by KEY returns the KEY if successful

lambda list: `(index-num key bmi)`

### get-bitmap  		 [Generic function]
Returns the bit-map from a bit map index, BMI, by KEY, returns DEFAULT-BIT-VECTOR or NIL if not found

lambda list: `(bmi key &optional default-bit-vector)`

### get-bitmap-indexes  	[Generic function]
Returns the indexes of the KEY in the bit-map-index, BMI

lambda list: `(bmi key)`


### bitmap-index-size  	[Function]
Returns and (with setf) changes the size of the specified bitmap-index

lambda list: `(bitmap-index)


### bitmap-index-key-hash  	[Function]
Returns and (with setf) changes the key-hash of the specified bitmap-index

lambda list: `(index )`


## Authors

  * Rathan Levins [ https://github.com/rlevins ]

```
;; Copyright (c) Rathan Levins
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
