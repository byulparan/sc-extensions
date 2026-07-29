(in-package :sc-extensions)

(defmacro define-code (name args &body body)
  (sc::convert-code
   `(defun ,name ,args
      ,@body)))


(defun load-serum-wavetables (path) 
  (let* ((tab (buffer-read path))
	 (data (buffer-to-array tab :channels 0))
	 (size 2048))
    (with-async nil
      (let* ((result
	      (loop for i below (/ (length data) size)
		    for start = (* i size)
		    for wavetable = (as-wavetable (subseq data start (+ start size)))
		    collect (uiop:with-temporary-file (:stream stream :pathname pathname :element-type '(unsigned-byte 8) :keep t)
			      (sc::write-mono-fl32-wav stream (floor (sc::sample-rate *s*)) wavetable)
			      (close stream)
			      (let* ((buffer (sc:buffer-read-no-update pathname :complete-handler (lambda (b)
												    (declare (ignore b))
												    (delete-file pathname)))))
				(setf (slot-value buffer 'sc::chanls) 1
				      (slot-value buffer 'sc::frames) (length wavetable))
				buffer)))))
	(assert (=
		 (- (bufnum (alexandria:lastcar result))
		    (bufnum (nth 0 result)))
		 (1- (length result))))
	(prog1 result
	  (buffer-free tab))))))


