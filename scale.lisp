(in-package #:sc-extensions)
(named-readtables:in-readtable :sc)

(defvar *scale-table* (make-hash-table))
(defvar *chord-table* (make-hash-table))

(macrolet ((set-table (mode &body key-values)
	     `(setf ,@(loop for (key value) on key-values by #'cddr
			    append (list `(gethash ,key ,(if (eql mode :scale) '*scale-table*
							     '*chord-table*)) value)))))
  (set-table :scale :chromatic '(0 1 2 3 4 5 6 7 8 9 10 11)
		    :ionian '(0 2 4 5 7 9 11)
		    :dorian '(0 2 3 5 7 9 10)
		    :phrygian '(0 1 3 5 7 8 10)
		    :lydian '(0 2 4 6 7 9 11)
		    :mixolydian '(0 2 4 5 7 9 10)
		    :aeolian '(0 2 3 5 7 8 10)
		    :locrian '(0 1 3 5 6 8 10)
		    :melodic-minor '(0 2 3 5 7 9 11)
		    :harmonic-minor '(0 2 3 5 7 8 11)
		    :pentatonic '(0 2 4 7 9)
		    :whole '(0 2 4 6 8 10))
  (set-table :chord :maj '(0 4 7)
		    :min '(0 3 7)
		    :maj7 '(0 4 7 11)
		    :7 '(0 4 7 10)
		    :min7 '(0 3 7 10)))



(defun get-key-number (key)
  (let* ((scale '((#\C . 60) (#\D . 62) (#\E . 64) (#\F . 65) (#\G . 67) (#\A . 69) (#\B . 71)))
	 (key-str (string-upcase key)))
    (let* ((base-code (cdr (assoc (elt key-str 0) scale :test #'char=)))
	   (octave (* 12 (- (parse-integer (string (alexandria:last-elt key-str))) 4))))
      (+ (+ base-code octave) (if (= 3 (length key-str)) (let* ((half (elt key-str 1)))
							   (ecase half
							     (#\S 1) (#\F -1)))
				0)))))

(defun scale (base scale &optional num &key octave)
  (let* ((note (get-key-number base))
	 (offset (gethash scale *scale-table*))
	 (offset-len (length offset)))
    (assert offset nil "not find ~a in scale" scale)
    (unless num (setf num offset-len
		      octave nil))
    (when octave (setf num (* num offset-len)))
    (loop for i from 0 below num
	  collect (+ note
		     (* 12 (floor (/ i offset-len)))
		     (nth (mod i offset-len) offset)))))

(defun chord (base chord)
  (let* ((note (get-key-number base))
	 (offset (gethash chord *chord-table*)))
    (assert offset nil "not find ~a in chord" chord)
    (mapcar #'(lambda (n) (+ note n)) offset)))

(defun inversion (chord n)
  (assert (or (numberp n) (typep n 'sc::ugen)) nil)
  (if (numberp n) (loop for c in chord
			for i from 0
			collect (if (<  i n) (+ c 12) c))
    (sc::+~ chord (loop for i from 0 below (length chord)
			collect (sc::*~ 12 (sc::<~ i n))))))

(defun pq (note scale)
  (let ((octave (+ 12 (car scale))))
    (when (> octave (last-elt scale))
      (setf scale (append scale (list octave)))))
  (let* ((rescale-value (* 12 (ceiling (/ (- note (last-elt scale)) 12.0))))
	 (rescale-note (- note rescale-value)))
    (+ rescale-value
       (loop for n in (reverse scale)
	     when (<= n rescale-note)
	       return n))))
