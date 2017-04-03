;;;; bit-map-index.asd

#-asdf3.1 (error "my-lib requires ASDF 3.1")


(asdf:defsystem "bit-map-index"
  :description
  "A primitive BIT Map Index.  Assumes that the position in the bitmap of a 1 is used as a reference into an array, id or other way to look up an object."
  :author "Rathan Levins <rlandbl@gmail.com>"
  :class :package-inferred-system
  :license "MIT License"
  :serial t
  :components ((:file "bit-map-index"))
  :depends-on (#:log4cl)
  :in-order-to ((test-op (test-op "bit-map-index/bmi-test"))))

(asdf:defsystem "bit-map-index/bmi-test"
  :depends-on (#:bit-map-index #:fiveam)
  :components ((:file "bmi-test"))
  :perform (asdf:test-op (o s)
		    (uiop:symbol-call "BIT-MAP-INDEX/BMI-TEST"
				      '#:run-my-tests)))
