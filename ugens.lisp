(in-package #:sc-extensions)
(named-readtables:in-readtable :sc)

(define-code poly (n gate)
  (* gate (== 0 (mod (+ (loop for i from 0 below n collect i)
			(pulse-count.kr gate)) n))))

(define-code rand-tr (tr odds)
  (* tr (> odds (t-rand.kr .0 .9999 tr))))

(define-code safe-tr (tr)
  (let* ((trg tr)
	 (sel (gate.kr 1 trg)))
    (select.kr sel [1 trg])))

(define-code r+ (sig min max)
  (range sig min max))

(define-code g+ (div len contents)
  (gate-lane div len contents))

(define-code sel-pos.kr (div contents)
  (select.kr (mod (floor (cnt div)) (length contents)) contents))

(define-code s+ (div contents)
  (select.kr (mod (floor (cnt div)) (length contents)) contents))

(define-code t+ (tr contents)
  (t-choose.kr tr contents))

(define-code t-line.ar (start end dur gate)
  (env-gen.ar (env [0 start end] [0 dur]) :gate gate))

(define-code t-line.kr (start end dur gate)
  (env-gen.kr (env [0 start end] [0 dur]) :gate gate))

(define-code perc.kr (gate &optional (attack 0.01) (release 1.0) (level 1.0) (curve -4.0))
  (env-gen.kr (perc attack release level curve) :gate gate))

(define-code del.kr (in del)
  (delay-l.kr in 2.0 del))

(define-code asr.kr (attk level rel gate &key (act :no-action))
  (env-gen.kr (asr attk level rel) :gate gate :act act))

(define-code env.kr (attk dur rel level trig &key (act :no-action))
  (env-gen.kr (env [0 level level 0] (* dur [attk (- 1.0 (+ attk rel)) rel])) :gate (t-line.kr -1 1 .001 trig) :act act))

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
		    (let ((result ,action))
		      (destructuring-bind (trig &rest values)
			result
		      (send-reply.kr trig ,cmd values)))
		    :to ,to
		    :fade .0))
      `(proxy ,key))))


