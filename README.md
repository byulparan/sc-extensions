# sc-extensions

A <a href="https://github.com/byulparan/cl-collider">cl-collider</a> additional library collection.   
this include server-side-sequencing tool / alias ugens 
and it implements `pc` library from Andrew Sorensen's [Extempore](http://digego.github.io/extempore/index.html)  

## Usage:

```cl
;;; load on cl-collider and sc-extensions
(ql:quickload '(:sc :sc-extensions))

(in-package :sc-user)
(use-package :sc-extensions)
(named-readtables:in-readtable :sc)

;;; bootup server
(setf *s* (make-external-server "Lisp-collider" :port 57880))
(server-boot *s*)


;;; set bpm and run `tempo` synth
(tempo 60.0)
(server-query-all-nodes) ;; You can look nodes state on scsynth server

;; run synth
(proxy :foo
  (pan2.ar (sin-osc.ar 440 0 (env-gen.kr (perc .0 .4 .2) :gate (tr 4)))))  ;; trigger on every 1/16 beat


;;; change note and beat
(proxy :foo
  (pan2.ar (sin-osc.ar (midicps (t-choose.kr (tr 4) [60 67 71 72])) 0
                       (env-gen.kr (perc .0 .4 .2) :gate (tr 8)))))


;;; chord progression every 8 beat 
(proxy :foo
  (pan2.ar (mix (sin-osc.ar (midicps (sel-pos.kr 1 8 [(pc:make-chord 40 80 4 (pc:diatonic 0 :^ :i7))
						                              (pc:make-chord 40 80 4 (pc:diatonic 0 :^ :vi7))]))
			    0 (env-gen.kr (perc .0 .4 .2) :gate (tr 8))))))

;;; modified to gate
(proxy :foo
  (let* ((gate (g+ 4 8 [0 3 6])))
    (pan2.ar (mix (sin-osc.ar (midicps (sel-pos.kr 1 8 [(pc:make-chord 40 80 4 (pc:diatonic 0 :^ :i7))
						                            	(pc:make-chord 40 80 4 (pc:diatonic 0 :^ :vi7))]))
			      0 (env-gen.kr (perc .0 .4 .2) :gate gate))))))

;;; change tempo
(tempo 72.0)

;;; change tempo with lag time
(tempo 88.0 :lag 8.0)

;;; grid base sequencing
(proxy :foo
  (with-grid (4 8)
      ((gt [0 3 6])
       (note [60 67 71])
       (decay [.4 1.0 .2]))
    (pan2.ar (sin-osc.ar (midicps note) 0
			             (env-gen.kr (perc .0 decay .2) :gate gt)))))

;;; polyphonic grid
(proxy :foo
  (with-grid-poly (4 8)
      ((gt [0 3 6])
       (note [60 67 71]))
    (pan2.ar (mix (sin-osc.ar (midicps note) 0
			                  (env-gen.kr (perc .0 2.0 .2) :gate gt))))))

```
