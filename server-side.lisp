(in-package :sc-extensions)
(named-readtables:in-readtable :sc)
;;; ==================================================================================================================
;;;
(defvar *max-beat* 96)
(defvar *tr-function* nil)
(defvar *cnt-function* nil)
(defvar *gate-lane-function* nil)

(define-code metro-tr (div)
  (sc::*~ (== 0 (sc::floor~ (sc::mod~ (in.kr (1- (sc::server-options-num-control-bus (server-options *s*))))
					 (sc::/~ *max-beat* div))))
	      (in.kr (- (sc::server-options-num-control-bus (server-options *s*)) 3))))

(defun tr (div)
  (if *tr-function* (funcall *tr-function* div)
    (metro-tr div)))

(define-code metro-cnt (div)
  (sc::floor~ (sc::/~ (in.kr (1- (sc::server-options-num-control-bus (server-options *s*))))
			  (sc::/~ *max-beat* div))))

(defun cnt (div)
  (if *cnt-function* (funcall *cnt-function* div)
    (metro-cnt div)))

(define-code metro-gate-lane (div n seqs)
  (mix (sc::*~ (tr div) (sc:dup (lambda (s) (== s (sc::mod~ (cnt div) n))) seqs))))

(defun gate-lane (div n seqs)
  (if *gate-lane-function* (funcall *gate-lane-function* div n seqs)
    (metro-gate-lane div n seqs)))

(define-code div (n)
  (/ (in.kr (- (sc::server-options-num-control-bus (server-options *s*)) 2)) n))

(define-code beat (n)
  (* (in.kr (- (sc::server-options-num-control-bus (server-options *s*)) 2)) n))

(define-code wait-when (div)
  (let ((synth-node (gethash (intern (string-upcase sc::*temp-synth-name*) :keyword) (sc::node-proxy-table *s*))))
    (cond ((and synth-node (sc::is-playing-p synth-node)) 1)
	  (t (gate.kr 1 (tr div))))))

(defun metro (bpm &key (relaunch nil) (lag 0) (pre-tick 0))
  (let* ((is-playing-p (sc:is-playing-p :metro)))
    (if (and is-playing-p (not relaunch)) (ctrl :metro :bpm bpm :lag lag)
      (progn
	(when is-playing-p
	  (free :metro)
	  (sync *s*))
	(proxy :metro
	  (with-controls ((bpm bpm) (lag 0.0) (reset 0 :tr))
	    (let* ((bpm (var-lag.kr bpm lag))
		   (tick (impulse.kr (* (/ bpm 60.0) *max-beat*)))
		   (count (- (pulse-count.kr tick reset) 1 pre-tick)))
	      (out.kr (- (sc::server-options-num-control-bus (server-options *s*)) 3) tick)
	      (out.kr (- (sc::server-options-num-control-bus (server-options *s*)) 2) (/ 60.0 bpm))
	      (out.kr (- (sc::server-options-num-control-bus (server-options *s*)) 1) count)))
	  :pos :head
	  :to 0)))))

;;; ==================================================================================================================

(defun pattern (buffer list &rest lists)
  (let ((all-data (append list (apply #'append lists))))
    (assert (>= (frames buffer) (length all-data)))
    (buffer-setn buffer
			(loop for i from 0 below (frames buffer)
			      collect (nth (mod i (length all-data)) all-data)))))

(defmacro on-trigger ((cnt div &optional (rem 0)) &body body)
  (let ((cmd (format nil "/a~a" (get-internal-real-time))))
    `(progn
       (add-reply-responder ,cmd
			    (lambda (a b c)
			      (declare (ignore a b c))
			      (unwind-protect (progn ,@body)
				(remove-reply-responder ,cmd))))
       (play
	(let ((et (* (tr ,cnt) (== ,rem (mod (cnt ,cnt) ,div)))))
	  (send-reply.kr et ,cmd 0)
	  (free-self.kr et)
	  0.0)
	:to *counter-group*
	:pos :tail))))

(defmacro pattern-at (buffer (cnt div &optional (rem 0)) list &rest lists)
  `(let ((all-data (append ,list (apply #'append ,(cons 'list lists)))))
     (assert (>= (frames ,buffer) (length all-data)))
     (let ((value (loop for i from 0 below (frames ,buffer)
			collect (nth (mod i (length all-data)) all-data))))
       (play
	(let ((et (* (tr ,cnt) (== (mod (cnt ,cnt) ,div) ,rem))))
	  (demand.kr et 0 (d-bufwr (d-seq value) ,buffer `,(loop for i from 0 below (length value) collect i)))
	  (free-self.kr et)
	  0.0)
	:to *counter-group* :pos :tail))))


(defmacro with-grid-poly ((div len &optional self) grid &body body)
  (setf grid (mapcar #'(lambda (g)
			 (destructuring-bind (name var)
			     g
			   (unless (eql (car var) 'list)
			     (setf var (cons 'list var)))
			   (list name var)))
		     grid))
  (let ((names (mapcar #'(lambda (n) (intern (format nil "~aS" (car n)))) (cdr grid)))
	(div-var (if (consp div) (car div)))
	(len-var (if (consp len) (car len)))
	(div (if (consp div) (second div) div))
	(len (if (consp len) (second len) len)))
    (alexandria:with-unique-names (length gates gate-num)
      `(mix (let* (,@(if div-var `((,div-var ,div)))
		   ,@(if len-var `((,len-var ,len)))
		   (,gates (list ,@(mapcar (lambda (g) `(gate-lane ,div ,len [,g]))
					   (cdr (second (car grid)))))))
	      ,@(if (or div-var len-var) `((declare (ignorable ,@(remove nil (list div-var len-var))))))
	      (macrolet ((with-sel (lst &body body)
			   `(if (find magic-number ,lst) (progn ,@body) 0.0)))
		(loop for ,(caar grid) in ,gates
		      for magic-number in ,(second (car grid))
		      ,@(if self `(for ,gate-num in ,(second (car grid))))
		      ,@(loop for d in (cdr grid)
			      nconc
			      `(for ,(car d) in
				    (list ,@(loop 
					      for i from 0 below (length (cdr (second (car grid))))
					      for v = (nth (mod i (length (cdr (second d)))) (cdr (second d)))
					      collect `(let ((,(caar grid) (nth ,i ,gates)))
							 (declare (ignorable ,(caar grid)))
							 ,(if (and (listp v) (not (symbolp (car v))))
							      (cons 'list v)
							    v))))))
		      collect (mix (let ,(mapcar (lambda (n v) (list n `(alexandria:ensure-list ,(car v))))
					  names (cdr grid))
				     magic-number
				     (let ((,length (apply #'max (mapcar #'length (list
										   (list ,(caar grid))
										   ,@names)))))
				       (loop for i from 0 below ,length
					     ,@(loop for d in (mapcar #'car (cdr grid))
						     for name in names
						     nconc `(for ,d = (nth (mod i (length ,name)) ,name)))
					     collect (progn
						       ,@(mapcar #'car (cdr grid))
						       (let ,(if self `((,self (sc::== (sc::mod~ (cnt ,div) ,len)
										       ,gate-num)))
							       ())
							 ,(if self `(declare (ignorable ,self)))
							 ,@body)))))))))))))

(defmacro with-grid ((div n &key (mode :cycle)) grid &body body)
  (let* ((gate (car (car grid)))
	 (indx (gensym)))
    `(destructuring-bind (,gate ,indx)
	 (mix
	  (loop for g in ,(second (car grid))
		for i from 0
		collect (let ((gt (== (sc::mod~ (cnt ,div) ,n ) g)))
			  [(sc::*~ (tr ,div) gt) (sc::*~ gt i)])))
       (declare (ignorable ,gate ,indx))
       (let* ,(loop for x in (cdr grid)
		    collect (let* ((sel `(select.kr ,(if (eql mode :cycle) `(sc::mod~ (latch.kr ,indx ,gate)
										      ,(1- (length (second x))))
						       `(latch.kr ,indx ,gate))
						    ,(second x))))
			      `(,(car x) (progn
					   (send-reply.kr (sc::>~ ,gate 1)
							  (format nil "/~s with-grid with duplicate number. it will be wrong works"
								  sc::*temp-synth-name*))
					   ,(if (third x)
						`(select.kr (gate.kr 1 ,gate) [,(third x) ,sel])
					      `,sel)))))
	 (declare (ignorable ,@(mapcar #'first (cdr grid))))
	 ,@body))))

