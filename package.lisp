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
	   #:next-dur
	   #:rrand
	   #:sinr
	   #:cosr
	   #:once
	   #:nth-beat
	   #:do-dolist
	   #:schedule
	   
	   #:t-line.ar
	   #:t-line.kr
	   #:t-gate.kr
	   #:perc.kr
	   #:del.kr
	   #:asr.kr
	   #:env.kr
	   #:proxy-handle))

(defpackage #:pc
  (:use #:cl)
  (:export #:pc-random
	   #:quantize
	   #:relative
	   #:make-chord
	   #:degree
	   #:quantize-list
	   #:transpose
	   #:scale
	   #:chord-to-scale
	   #:chord
	   #:diatonic))
