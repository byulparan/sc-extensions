(in-package :sc-extensions)
(named-readtables:in-readtable :sc)

(defun bpm (bpm &key (relaunch nil) (lag 0))
  (tempo bpm :relaunch relaunch :lag lag)
  (clock-bpm bpm))

(defmacro pp (arg &body body)
  (let* ((sym-beat (alexandria:symbolicate "BEAT"))
	 (sym-dur (alexandria:symbolicate "DUR")))
    (alexandria:with-gensyms (name)
      `(let* ((,name ,arg))
	 (if (numberp ,name) (let ((,sym-beat (+ ,sym-beat ,name)))
			       (at-beat ,sym-beat ,@(progn (alexandria:if-let ((dur (getf (cdr body) :dur)))
							     (cons (car body)
								   (append
								    (alexandria:remove-from-plist (cdr body) :dur)
								    (list :dur `(clock-tm ,dur))))
							     (append body (list :dur `(clock-tm ,sym-dur)))))))
	   (at-beat ,sym-beat ,name  ,@(progn (alexandria:if-let ((dur (getf body :dur)))
						(append (alexandria:remove-from-plist body :dur)
							(list :dur `(clock-tm ,dur)))
						(append body (list :dur `(clock-tm ,sym-dur)))))))))))

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


