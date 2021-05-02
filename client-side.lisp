(in-package :sc-extensions)
(named-readtables:in-readtable :sc)


(defvar *bpm-functions* nil)

(defun bpm (&optional bpm &key (relaunch nil) (lag 0) (pre-tick 0))
  (if (not bpm) (clock-bpm)
    (progn
      (proxy-handle :tempo-changed
	  (let* ((tempo (in.kr (- (sc::server-options-num-control-bus (server-options *s*)) 2))))
	    (line.kr 0 0 (+ lag 1) :act :free)
	    [(impulse.kr 30) tempo])
	(lambda (tempo)
	  (clock-bpm (* 1.0d0 (/ 60 tempo))))
	:pos :after
	:to :metro)
      (metro bpm :relaunch relaunch :lag lag :pre-tick pre-tick)
      (dolist (f *bpm-functions*)
	(funcall f bpm :relaunch relaunch :lag lag)))))

(defmacro pp (&body body)
  (let* ((sym-beat (alexandria:symbolicate "BEAT"))
	 (sym-dur (alexandria:symbolicate "DUR"))
	 (has-beat-offset (evenp (length body)))
	 (params (if has-beat-offset (cddr body) (cdr body)))
	 (has-dur-p (getf params :dur)))
    (when has-dur-p (setf params (alexandria:remove-from-plist params :dur)))
    (if has-beat-offset 
	`(let ((,sym-beat (+ ,sym-beat ,(car body))))
	   (at-synth ,sym-beat ,(second body) ,@params
		     :dur (clock-dur ,(if has-dur-p has-dur-p sym-dur))))
      `(at-synth ,sym-beat ,(car body) ,@params :dur (clock-dur ,(if has-dur-p has-dur-p sym-dur))))))

(defmacro next-dur (beat new-value)
  (let ((sym-beat (alexandria:symbolicate "BEAT"))
	(sym-dur (alexandria:symbolicate "DUR")))
    `(if (zerop (mod (+ ,sym-beat ,sym-dur) ,beat)) ,new-value ,sym-dur)))

(defun rrand (n &optional p)
  (cond (p (let* ((min (min n p))
		  (max (max n p)))
	     (+ min (random (- max (- min (if (every #'integerp (list n p)) 1 0)))))))
	((numberp n) (random n))
	((listp n) (alexandria:random-elt n))))

(defmacro sinr (offset gain ratio)
  `(+ ,offset (* ,gain (sin (* pi ,(alexandria:symbolicate "BEAT") ,ratio)))))

(defmacro cosr (offset gain ratio)
  `(+ ,offset (* ,gain (cos (* pi ,(alexandria:symbolicate "BEAT") ,ratio)))))

(defmacro once (form)
  (let* ((result (eval form)))
    `(quote ,result)))


(defmacro do-dolist (binding &body body)
  (let* ((i (alexandria:symbolicate "I")))
    `(loop for ,(caar binding) in ,@(cdar binding)
	   for ,i from 0
	   ,@(loop for (name form special) in (cdr binding)
		   for namewrap = (intern (concatenate 'string (string-upcase name) "BIND"))
		   append `(,(if special 'for 'with) ,namewrap = ,form
			    for ,name = (if (listp ,namewrap) (nth (mod ,i (length ,namewrap)) ,namewrap)
					  ,namewrap)))
	   do (progn ,@body))))

