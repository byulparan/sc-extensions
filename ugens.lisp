(in-package #:sc-extensions)
(named-readtables:in-readtable :sc)

(define-code poly (n gate)
  (* gate (== 0 (mod (+ (loop for i from 0 below n collect i)
			(pulse-count.kr gate)) n))))

(define-code rand-tr (div odds)
  (let* ((trg (tr div)))
    (* trg (> odds (t-rand.kr .0 1.0 trg)))))

(define-code r+ (sig min max)
  (range sig min max))

(define-code g+ (div len contents)
  (gate-lane div len contents))

(define-code t-line.ar (start end dur gate)
  (env-gen.ar (env [0 start end] [0 dur]) :gate gate))

(define-code t-line.kr (start end dur gate)
  (env-gen.kr (env [0 start end] [0 dur]) :gate gate))

(define-code perc.kr (gate &optional (attack 0.01) (release 1.0) (level 1.0) (curve -4.0))
  (env-gen.kr (perc attack release level curve) :gate gate))

(define-code del.kr (in del)
  (delay-l.kr in 2.0 del))

(define-code asr.kr (attk rel level gate)
  (* level (env-gen.kr (env [0 1 0] [attk rel]) :gate gate)))


(defun sel-pos.kr (div len contents)
  (sc:select.kr (sc::mod~ (sc::floor~ (sc::/~ (cnt div) len)) (length contents)) contents))


(defmacro proxy-handle (key &optional action handle (to 1))
  (let* ((name (format nil "~a-HANDLE" (string-upcase key)))
	 (cmd (format nil "/~a" name))
	 (key (intern name :keyword)))
    (if action `(let ((func ,handle))
		  (add-reply-responder
		   ,cmd
		   (lambda (&rest args)
		     (flet ((float-to-int (number)
			      (multiple-value-bind (v fract)
				  (floor number)
				(if (zerop fract) v (+ v fract)))))
		       (apply func (mapcar #'float-to-int (cddr args))))))
		  (proxy ,key
		    (destructuring-bind (trig &rest values)
			,action
		      (send-reply.kr trig ,cmd values))
		    :to ,to
		    :fade .0))
      `(proxy ,key))))


