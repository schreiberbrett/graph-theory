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

(define roleo (lambda (role el er biclique eo)
    (fresh (x)
        (== x `(,role ,el ,er ,biclique ,eo))
        (conde
            ((== x '(a 1 0 l 0)))
            ((== x '(b 0 1 r 0)))
            ((== x '(u 1 0 o 1)))
            ((== x '(v 0 1 o 1)))
            ((== x '(w 1 1 o 1)))
            ((== x '(o 0 0 o 0)))))))

(define all-roleso (lambda (a b c d e)
    (conde
        ((== `(,a ,b ,c ,d ,e) '(() () () () ())))
        ((fresh (car-a cdr-a car-b cdr-b car-c cdr-c car-d cdr-d car-e cdr-e)
            (== `(,a ,b ,c ,d ,e) `((,car-a . ,cdr-a) (,car-b . ,cdr-b) (,car-c . ,cdr-c) (,car-d . ,cdr-d) (,car-e . ,cdr-e)))
            (roleo car-a car-b car-c car-d car-e)
            (all-roleso cdr-a cdr-b cdr-c cdr-d cdr-e))))))
(define proveso (lambda (hypergraph bicliques steps)
    (conde
        ((== steps '()) (any-less-than-3o hypergraph))
        ((fresh (step rest-steps e1 e2 el er eo rest-hyperedges biclique rest-bicliques filtered-bicliques)
            (== steps `(,step . ,rest-steps))
            ;; Pick two hyperedges
            (riffleo `(,e1 ,e2) rest-hyperedges hypergraph)
            
            ;; Order matters since one will be considered "left" and the other "right"
            (riffleo `(,el) `(,er) `(,e1 ,e2))
            
            ;; Pick one cbs
            (riffleo `(,biclique) rest-bicliques bicliques)

            ;; These hyperedges must be connected
            (all-roleso step el er biclique eo)
            
            (filter-bicliqueso biclique rest-bicliques filtered-bicliques)
            
            ;; Recur on the rest of the steps with the newly-made h-out
            (proveso `(,eo . ,rest-hyperedges) filtered-bicliques rest-steps))))))

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

(define filter-bicliqueso (lambda (x l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l o-rest)
            (== l `(,car-l . ,cdr-l))
            (conde
                ((== o `(,car-l . ,o-rest))
                    (all-disjointo x car-l))
                ((== o o-rest)
                    (any-overlapo x car-l)))
            (filter-bicliqueso x cdr-l o-rest))))))

(define overlapo (lambda (u v)
    (fresh (x)
        (== x `(,u ,v))
        (conde
            ((== x '(l l)))
            ((== x '(l r)))
            ((== x '(r l)))
            ((== x '(r r)))))))

(define any-overlapo (lambda (x y)
    (fresh (car-x cdr-x car-y cdr-y)
        (== x `(,car-x . ,cdr-x))
        (== y `(,car-y . ,cdr-y))
        (conde
            ((overlapo car-x car-y))
            ((any-overlapo cdr-x cdr-y))))))

(define disjoint2o (lambda (u v)
    (conde
        ((== u 'o))
        ((== v 'o)))))

(define disjointo (lambda (u v)
    (fresh (x)
        (== x `(,u ,v))
        (conde
            ((== x '(l o)))
            ((== x '(r o)))
            ((== x '(o o)))
            ((== x '(o r)))
            ((== x '(o l)))))))
        
(define all-disjointo (lambda (x y)
    (conde
        ((== x '()) (== y '()))
        ((fresh (car-x cdr-x car-y cdr-y)
            (== x `(,car-x . ,cdr-x))
            (== y `(,car-y . ,cdr-y))
            (disjointo car-x car-y)
            (all-disjointo cdr-x cdr-y))))))
(define abc-hypergraph '(
    (1 0 1 1 0 0 0 0 0)
    (0 1 0 0 1 1 0 0 0)
    (0 0 0 0 0 0 1 1 1)))

(define abc-bicliques '(
    (l r o o o o o o o)
    (o o o o o l r o o)
    (o o o o l o r o o)
    (o o o o l l r o o)
    (o o o l o o r o o)
    (o o o l o l r o o)
    (o o o l l o r o o)
    (o o o l l l r o o)
    (o o l o o o r o o)
    (o o l o o l r o o)
    (o o l o l o r o o)
    (o o l o l l r o o)
    (o o l l o o r o o)
    (o o l l o l r o o)
    (o o l l l o r o o)
    (o o l l l l r o o)))
(define k4-example-hypergraph '(
    (1 1 1 0)
    (0 1 1 1)))
    
(define k4-example-cbh '(
    (l o o r)))
(define same-lengtho (lambda (a b)
    (conde
        ((== a '()) (== b '()))
        ((fresh (x y z w)
            (== a `(,x . ,y))
            (== b `(,z . ,w))
            (same-lengtho y w))))))

