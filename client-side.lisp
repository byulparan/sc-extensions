(in-package :sc-extensions)
(named-readtables:in-readtable :sc)

(defun bpm (bpm &key (relaunch nil) (lag 0))
  (tempo bpm :relaunch relaunch :lag lag)
  (clock-bpm bpm))

(defmacro pp (arg &body body)
  (let* ((sym-beat (alexandria:symbolicate "BEAT"))
	 (sym-dur (alexandria:symbolicate "DUR"))
	 (no-del-p (or (keywordp arg) (and (consp arg) (eql (car arg) 'quote))))
	 (param (if no-del-p body (cdr body))))
    (alexandria:if-let ((dur (getf param :dur)))
      (setf (getf param :dur) `(clock-tm ,dur))
      (setf body (append body (list :dur `(clock-tm ,sym-dur)))))
    (if no-del-p `(at-beat ,sym-beat ,arg ,@body)
      `(let ((,sym-beat (+ ,sym-beat ,arg)))
	 (at-beat ,sym-beat ,@body)))))

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


