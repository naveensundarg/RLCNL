;;;;;; NLG CORE REPRESENTATIONS;;;;;
(defun atomic-functor? (form) (symbolp form))
(defun functor-name (form) (if (atomic-functor? form) form (first form)))
(defun functor-arguments (form) (rest form))
;; for now just a list representation. will modify to use clos and integrate with cec dpl.
(defun parse (form) form)

(defparameter *nlg-mappings* (make-hash-table))
 
(defmacro define-compositional-nlg (functor  args &body specs)
  `(setf (gethash ',functor  *nlg-mappings*)
	 (lambda ,args (concatenate 'string ,@specs ))))

(defmacro define-self-named-constants (&rest constants)
  `(progn
     ,@(mapcar 
	(lambda (constant) 
	  `(define-compositional-nlg  ,constant () ,(symbol-name constant)))
	constants)))

(defparameter *post-processing-pipline*
  ())

(defun post-process (sentence)
  (reduce (lambda (current func) (funcall func current)) 
	  *post-processing-pipline*
	  :initial-value sentence))

(defun nlg (form)
  (post-process 
   (apply (gethash (functor-name form)  *nlg-mappings*)
	  (if (not (atomic-functor? form)) 
	      (mapcar #'nlg (functor-arguments form))
	      nil))))

(defmacro time-index (m)
 `(cond 
    ((equalp ,m "now") " now")
    ((equalp ,m "anytime") " at any time")
    ((equalp ,m "alltime") " all the time")
    (t  (concatenate' string " at  moment " ,m ))))

;;;;; NLG DEFINITIONS FOR A TOY LOGIC;;
(define-compositional-nlg jack () "jack")
(define-compositional-nlg mary () "mary")
(define-compositional-nlg loves (x y) x " loves " y)
(nlg (parse '(loves jack mary)))

;;;;; NLG DEFINITIONS FOR LOGICAL CONNECTIVES ;;;;
(define-compositional-nlg and (x y) x ", and " y)
(define-compositional-nlg or (x y) x ", or " y)
(define-compositional-nlg implies (x y) "if " x ", then " y)
(define-compositional-nlg not (x) "it is not the case that " x)

;;;;; NLG DEFINITIONS FOR DCEC ;;;;;;;
(define-compositional-nlg action (agent actiontype)
  actiontype " by " agent)

(define-compositional-nlg initially (fluent)
  "Fluent " fluent " holds initially")

(define-compositional-nlg holds (fluent moment)
  "the fluent, '" fluent "', holds " (time-index moment))

(define-compositional-nlg happens (event moment)
  event " happens" (time-index moment))

(define-compositional-nlg clipped (moment-1 fluent moment-2)
  "Fluent " fluent "is clipped between moment " moment-1 " and moment " moment-2)

(define-compositional-nlg initiates (moment-1 fluent moment-2)
  "Fluent " fluent "is initiated between moment " moment-1 " and moment " moment-2)

(define-compositional-nlg terminates (moment-1 fluent moment-2)
  "Fluent " fluent "is terminated between moment " moment-1 " and moment " moment-2)

(define-compositional-nlg prior (moment-1 moment-2)
  "Moment " moment-1 " happens before moment " moment-2)

(define-compositional-nlg perceives (a m P)
   a  (time-index m) " perceives that " P )

(define-compositional-nlg knows (a m P)
  a (time-index m) " knows that " P)

(define-compositional-nlg believes (a m P)
  a (time-index m) " believes that " P)

(define-compositional-nlg ought (a m P Q)
  "it is obligatory" (time-index m) " under situation " P ", " a " should see to it that " Q)

(define-compositional-nlg  * (a) a " itself")
 
;;; NON LOGICAL FUNCTORS                                                                                                                                                         
(define-compositional-nlg carrying (a b)
  a " is carrying " b)
;;;;; CONSTANTS ;;;;;

(define-self-named-constants 
    t1 t2 t3 t4 now anytime alltime
    cogito jack mary marie)

(define-compositional-nlg main () "main")
(define-compositional-nlg mission (m)  m " mission being carried out")

(define-compositional-nlg ugv () "the ugv")
(define-compositional-nlg commander () "the commander")
(define-compositional-nlg soldier () "the soldier")
(define-compositional-nlg firefight () "a firefight")
(define-compositional-nlg radio-silence () "maintaing silence")
(define-compositional-nlg main-mission () "the main mission being carried out")
(define-compositional-nlg t1 () "t1")
(nlg (parse '(perceives jack t1 (loves jack mary))))
(nlg (parse '(knows ugv now (holds (carrying ugv soldier) now))))
(nlg (parse '(believes ugv now (believes commander t1 (not (perceives ugv anytime (happens firefight anytime)))))))

(nlg (parse '(knows ugv now (ought ugv anytime (mission main) (happens (action (* ugv) radio-silence) alltime)))))