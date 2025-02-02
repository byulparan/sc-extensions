(defpackage #:sc-extensions
  (:use #:cl #:sc)
  (:export #:define-code
	   
	   #:tr
	   #:cnt
	   #:gate-lane
	   #:div
	   #:beat
	   #:wait-when
	   #:metro
	   #:tempo
	   #:pattern
	   #:on-trigger
	   #:pattern-at
	   #:with-grid-poly
	   #:with-grid
	   #:poly
	   #:rand-tr
	   #:safe-tr
	   #:r+
	   #:g+
	   #:sel-pos.kr
	   #:s+
	   #:t+
	   #:bpm
	   #:pp
	   #:rrand
	   #:sinr
	   #:cosr
	   #:once
	   #:nth-beat
	   #:beat-count
	   #:latch
	   #:coin
	   #:schedule
	   #:with-lambda
	   #:*scale*
	   
	   #:t-line.ar
	   #:t-line.kr
	   #:t-gate.kr
	   #:perc.kr
	   #:del.kr
	   #:asr.kr
	   #:env.kr
	   #:coin.kr
	   #:proxy-handle))

(defpackage #:pc
  (:use #:cl)
  (:export #:pc-random
	   #:quantize
	   #:relative
	   #:make-chord
	   #:degree
	   #:quantize-list
	   #:q-list
	   #:transpose
	   #:scale
	   #:chord-to-scale
	   #:chord
	   #:diatonic))
