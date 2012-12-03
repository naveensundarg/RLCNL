;;;;;; NLG CORE REPRESENTATIONS;;;;;
(defun atomic-functor? (form) (symbolp form))
(defun functor-name (form) (if (atomic-functor? form) form (first form)))
(defun functor-arguments (form) (rest form))
(defun parse (form) form)

(defparameter *nlg-mappings* (make-hash-table))
 
(defmacro define-compositional-nlg (functor  args &body specs)
  `(setf (gethash ',functor  *nlg-mappings*)
	 (lambda ,args (concatenate 'string ,@specs " "))))

(defun nlg (form)
  (string-upcase 
   (apply (gethash (functor-name form)  *nlg-mappings*)
	  (if (not (atomic-functor? form)) 
	      (mapcar #'nlg (functor-arguments form))
	      nil))))

;;;;; NLG DEFINITIONS FOR A TOY LOGIC;;
(define-compositional-nlg jack () "jack")
(define-compositional-nlg mary () "mary")
(define-compositional-nlg loves (x y) x " loves " y)
(nlg (parse '(loves jack mary)))

;;;;; NLG DEFINITIONS FOR DCEC ;;;;;;;
(define-compositional-nlg action (agent actiontype)
  "Action of type " actiontype "performed by agent " agent)

(define-compositional-nlg initially (fluent)
  "Fluent " fluent " holds initially")

(define-compositional-nlg holds (fluent moment)
  "Fluent " fluent " holds at the moment " moment)

(define-compositional-nlg happens (event moment)
  "Event " event " happens at the moment " moment)

(define-compositional-nlg clipped (moment-1 fluent moment-2)
  "Fluent " fluent "is clipped between moment " moment-1 " and moment " moment-2)

(define-compositional-nlg initiates (moment-1 fluent moment-2)
  "Fluent " fluent "is initiated between moment " moment-1 " and moment " moment-2)

(define-compositional-nlg terminates (moment-1 fluent moment-2)
  "Fluent " fluent "is terminates between moment " moment-1 " and moment " moment-2)

(define-compositional-nlg prior (moment-1 moment-2)
  "Moment " moment-1 " happens before moment " moment-2)

(define-compositional-nlg perceives (a m P)
  "Agent " a " perceives that " P " at moment " m)

(define-compositional-nlg knows (a m P)
  "Agent " a " perceives that " P " at moment " m)

(define-compositional-nlg believes (a m P)
  "Agent " a " perceives that " P " at moment " m)

;;;;; CONSTANTS ;;;;;
(define-compositional-nlg jack () "jack")
(define-compositional-nlg t1 () "t1")


(nlg (parse '(perceives jack t1 (loves jack mary))))