RLCNL
=====

RAIR Lab's Logically Controlled Natural Language

====
NLG Example

(define-compositional-nlg jack () "jack")
(define-compositional-nlg mary () "mary")
(define-compositional-nlg loves (x y) x " loves " y)
(nlg (parse '(loves jack mary)))
