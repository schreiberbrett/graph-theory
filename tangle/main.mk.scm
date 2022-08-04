(load "~/miniKanren/mk.scm")

(define riffleo (lambda (a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o))))))


(define proof-stepo (lambda (step h-l h-r cbs h-out)
    (conde
        ((== step '()) (== h-l '()) (== h-r '()) (== cbs '()) (== h-out '()))
        ((fresh (car-h-l cdr-h-l car-h-r cdr-h-r car-h-out cdr-h-out car-step cdr-step car-cbs cdr-cbs)
            (== step `(,car-step . ,cdr-step))
            (== h-l `(,car-h-l . ,cdr-h-l))
            (== h-r `(,car-h-r . ,cdr-h-r))
            (== cbs `(,car-cbs . ,cdr-cbs))
            (== h-out `(,car-h-out . ,cdr-h-out))
            (conde
                ((== car-h-l 0) (== car-h-r 0) (== car-h-out 0) (== car-cbs 'o) (== car-step 'o))
                ((== car-h-l 1) (== car-h-r 0) (== car-h-out 0) (== car-cbs 'l) (== car-step 'r1))
                ((== car-h-l 1) (== car-h-r 0) (== car-h-out 1) (== car-cbs 'o) (== car-step 'b1))
                ((== car-h-l 0) (== car-h-r 1) (== car-h-out 0) (== car-cbs 'r) (== car-step 'r2))
                ((== car-h-l 0) (== car-h-r 1) (== car-h-out 1) (== car-cbs 'o) (== car-step 'b2))
                ((== car-h-l 1) (== car-h-r 1) (== car-h-out 1) (== car-cbs 'o) (== car-step 'b)))
            (proof-stepo cdr-step cdr-h-l cdr-h-r cdr-cbs cdr-h-out))))))
(define proveso (lambda (h cbs steps)
    (conde
        ((== steps '()) (any-less-than-3o h))
        ((fresh (step rest-steps h1 h2 h-l h-r h-out h-rest c rest-cbs new-cbs)
            (== steps `(,step . ,rest-steps))
            ;; Pick two hyperedges
            (riffleo `(,h1 ,h2) h-rest h)
            
            ;; Order matters since one will be considered "left" and the other "right"
            (riffleo `(,h-l) `(,h-r) `(,h1 ,h2))
            
            ;; Pick one cbs
            (riffleo `(,c) rest-cbs cbs)

            ;; These hyperedges must be connected
            (proof-stepo step h-l h-r c h-out)
            
            (filter-cbso c rest-cbs new-cbs)
            
            ;; Recur on the rest of the steps with the newly-made h-out
            (proveso `(,h-out . ,h-rest) new-cbs rest-steps))))))

(define any-less-than-3o (lambda (h)
    (fresh (car-h cdr-h)
        (== h `(,car-h . ,cdr-h))
        (conde
            ((less-than-3o car-h))
            ((any-less-than-3o cdr-h))))))

(define less-than-3o (lambda (hyperedge)
    (fresh (n)
        (conde
            ((== n 'z))
            ((== n `(s . z)))
            ((== n `(s s . z))))
            
        (sizeo hyperedge n))))
     
(define sizeo (lambda (hyperedge size)
    (fresh (first rest size-rec)
        (conde
            ((== hyperedge '()) (== size 'z))
            ((== hyperedge `(,first . ,rest))
                (conde
                    ((== first 0) (== size size-rec))
                    ((== first 1) (== size `(s . ,size-rec))))
                (sizeo rest size-rec))))))

(define ex-a '(l o r o l))
(define ex-l '(
    (l o o r o)
    (r o l o o)
    (o l o r o)
    (o r r l o)))

(define filter-cbso (lambda (x l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l should-discard o-rest)
            (== l `(,car-l . ,cdr-l))
            (conde
                ((== should-discard #f) (== o `(,car-l . ,o-rest)))
                ((== should-discard #t) (== o o-rest)))
            (overlapso x car-l should-discard)
            (filter-cbso x cdr-l o-rest))))))

(define overlapso (lambda (x y result)
    (conde
        ((== x '()) (== y '()) (== result #f))
        ((fresh (car-x cdr-x car-y cdr-y)
            (== x `(,car-x . ,cdr-x))
            (== y `(,car-y . ,cdr-y))
            (conde
                (
                    (== result #t)
                    (conde
                        ((== car-x 'l) (== car-y 'l))
                        ((== car-x 'r) (== car-y 'l))
                        ((== car-x 'l) (== car-y 'r))
                        ((== car-x 'r) (== car-y 'r))))
                (
                    (conde
                        ((== car-x 'o))
                        ((== car-y 'o)))
                        
                    (overlapso cdr-x cdr-y result))))))))
(define sample-hypergraph '(
    (1 1 1 0 0 0 0 0 0)
    (0 0 0 1 1 1 0 0 0)
    (0 0 0 0 0 0 1 1 1)))
    
(define sample-graph '(
    (0 0 0 1 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 1 0 0)
    (1 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 1 0 0)
    (0 1 1 0 1 1 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)))
(define smaller-sample-hypergraph '(
    (1 1 1 0)
    (0 1 1 1)))

(define smaller-sample-graph '(
    (0 0 0 1)
    (0 0 0 0)
    (0 0 0 0)
    (1 0 0 0)))
(define abc-example-hypergraph '(
    (1 0 1 1 0 0 0 0 0)
    (0 1 0 0 1 1 0 0 0)
    (0 0 0 0 0 0 1 1 1)))

(define abc-example-cbh '(
    (l r o o o o o o o)
    (o o o o o l r o o)
    (o o o o l o r o o)
    (o o o o l l r o o)
    (o o o l o o r o o)
    (o o o l o l r o o)
    ;; (o o o l l o r o o) ;; This edge seems to add 8s to the search
    (o o l l l l r o o)))
(define k4-example-hypergraph '(
    (1 1 1 0)
    (0 1 1 1)))
    
(define k4-example-cbh '(
    (l o o r)))
(define example-cbs
    '(l l l l l l l l l l l l l l o o o))

(define example-filter '(
 (o o o o o o o o o o o o r o o o l)
 (o o o o o o o o o o o l o o o o l)
 (o o o o o o o o o o o r o o o o l)
 (o o o o o o o o o o l o o o o o l)
 (o o o o o o o o o o r o o o o o l)
 (o o o o o o o o o l o o o o o o l)
 (o o o o o o o o o r o o o o o o l)
 (o o o o o o o o l o o o o o o o l)
 (o o o o o o o o r o o o o o o o l)
 (o o o o o o o l o o o o o o o o l)
 (o o o o o o o r o o o o o o o o l)
 (o o o o o o l o o o o o o o o o l)
 (o o o o o o r o o o o o o o o o l)
 (o o o o o l o o o o o o o o o o l)
 (o o o o o r o o o o o o o o o o l)
 (o o o o l o o o o o o o o o o o l)
 (o o o o r o o o o o o o o o o o l)
 (o o o l o o o o o o o o o o o o l)
 (o o o r o o o o o o o o o o o o l)
 (o o l o o o o o o o o o o o o o l)
 (o o r o o o o o o o o o o o o o l)
 (o l o o o o o o o o o o o o o o l)
 (o r o o o o o o o o o o o o o o l)
 (l o o o o o o o o o o o o o o o l)
 (r o o o o o o o o o o o o o o o l)
 (o o o o o o o o o o o o o o r o r)
 (o o o o o o o o o o o o o o l l o)
 (o o o o o o o o o o o o o l o o r)
 (o o o o o o o o o o o o o o r l o)
 (o o o o o o o o o o o o o r o o r)
 (o o o o o o o o o o o o l o o o r)
 (o o o o o o o o o o o o r o o o r)
 (o o o o o o o o o o o l o o o o r)
 (o o o o o o o o o o o r o o o o r)
 (o o o o o o o o o o l o o o o o r)
 (o o o o o o o o o o r o o o o o r)
 (o o o o o o o o o l o o o o o o r)
 (o o o o o o o o o r o o o o o o r)
 (o o o o o o o o l o o o o o o o r)
 (o o o o o o o o r o o o o o o o r)
 (o o o o o o o l o o o o o o o o r)
 (o o o o o o o r o o o o o o o o r)
 (o o o o o o l o o o o o o o o o r)
 (o o o o o o r o o o o o o o o o r)
 (o o o o o l o o o o o o o o o o r)
 (o o o o o r o o o o o o o o o o r)
 (o o o o l o o o o o o o o o o o r)
 (o o o o r o o o o o o o o o o o r)
 (o o o l o o o o o o o o o o o o r)
 (o o o r o o o o o o o o o o o o r)
 (o o l o o o o o o o o o o o o o r)
 (o o r o o o o o o o o o o o o o r)
 (o l o o o o o o o o o o o o o o r)
 (o r o o o o o o o o o o o o o o r)
 (l o o o o o o o o o o o o o o o r)))

    
(define is-cbs (lambda (l)
    (conde
        ((== l '()))
        ((fresh (car-l cdr-l)
            (== l `(,car-l . ,cdr-l))
            (conde
                ((== car-l 'o))
                ((== car-l 'l))
                ((== car-l 'r)))
            (is-cbs cdr-l))))))
(define same-lengtho (lambda (a b)
    (conde
        ((== a '()) (== b '()))
        ((fresh (x y z w)
            (== a `(,x . ,y))
            (== b `(,z . ,w))
            (same-lengtho y w))))))

