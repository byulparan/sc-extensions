(in-package :sc-extensions)
(named-readtables:in-readtable :sc)


(defvar *bpm-functions* nil)

(defun bpm (&optional bpm &key (relaunch nil) (lag 0))
  (if (not bpm) (clock-bpm)
    (progn
      (proxy-handle :tempo-changed
	  (let* ((tempo (in.kr (- (sc::server-options-num-control-bus (server-options *s*)) 2))))
	    (line.kr 0 0 (+ lag 1) :act :free)
	    [(impulse.kr 30) tempo])
	(lambda (tempo)
	  (clock-bpm (* 60 (reciprocal tempo)))))
      (metro bpm :relaunch relaunch :lag lag)
      (dolist (f *bpm-functions*)
	(funcall f bpm :relaunch relaunch :lag lag)))))

(defmacro pp (arg &body body)
  (let* ((sym-beat (alexandria:symbolicate "BEAT"))
	 (sym-dur (alexandria:symbolicate "DUR")))
    (alexandria:with-gensyms (name)
      `(let* ((,name ,arg))
	 (if (numberp ,name) (let ((,sym-beat (+ ,sym-beat ,name)))
			       (at-beat ,sym-beat
				 (let* ((,name ,(car body)))
				   (apply (if (keywordp ,name) #'ctrl #'synth)
					  ,name
					  (list
					   ,@(progn (alexandria:if-let ((dur (getf (cdr body) :dur)))
						      (append
						       (alexandria:remove-from-plist (cdr body) :dur)
						       (list :dur `(clock-dur ,dur)))
						      (append (cdr body) (list :dur `(clock-dur ,sym-dur))))))))))
	   (at-beat ,sym-beat
	     (apply (if (keywordp ,name) #'ctrl #'synth) ,name
		      (list
		       ,@(progn (alexandria:if-let ((dur (getf body :dur)))
				 (append (alexandria:remove-from-plist body :dur)
					 (list :dur `(clock-dur ,dur)))
				 (append body (list :dur `(clock-dur ,sym-dur)))))))))))))

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
  `(+ ,offset (* ,gain (sin (* 2 pi ,(alexandria:symbolicate "BEAT") ,ratio)))))

(defmacro cosr (offset gain ratio)
  `(+ ,offset (* ,gain (cos (* 2 pi ,(alexandria:symbolicate "BEAT") ,ratio)))))


