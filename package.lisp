(defpackage #:sc-extensions
  (:use #:cl #:sc)
  (:export #:define-code
	   
	   #:tr
	   #:cnt
	   #:gate-lane
	   #:div
	   #:beat
	   #:wait-when
	   #:tempo

	   #:pattern
	   #:on-trigger
	   #:pattern-at
	   #:with-grid-poly
	   #:with-grid
	   
	   #:poly
	   #:rand-tr
	   #:r+
	   #:g+
	   #:t-line.ar
	   #:t-line.kr
	   #:perc.kr
	   #:del.kr
	   #:asr.kr
	   #:sel-pos.kr
	   #:proxy-handle))

(defpackage #:pc
  (:use #:cl)
  (:export #:quantize
	   #:relative
	   #:make-chord
	   #:degree
	   #:quantize-list
	   #:transpose
	   #:scale
	   #:chord-to-scale
	   #:chord
	   #:diatonic))
