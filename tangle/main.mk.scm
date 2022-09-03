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
        ((fresh (step rest-steps e1 e2 el er eo rest-hyperedges biclique rest-bicliques filtered-bicliques _a _b)
            (== steps `(,step . ,rest-steps))
            ;; Pick two hyperedges
            (riffleo `(,e1 ,e2) rest-hyperedges hypergraph)
            
            ;; Order matters since one will be considered "left" and the other "right"
            (riffleo `(,el) `(,er) `(,e1 ,e2))
            
            ;; Pick one biclique
            (riffleo `(,biclique) rest-bicliques bicliques)

			;; The proof step must have an l and r (sometimes useful)
			;; (riffleo '(l) _a step)
			;; (riffleo '(r) _b step)

            ;; These hyperedges must be connected
            (all-roleso step el er biclique eo)
            
            (filter-bicliqueso biclique rest-bicliques filtered-bicliques)
            
            ;; Recur on the rest of the steps with the new hypergraph
            (proveso `(,eo . ,rest-hyperedges) filtered-bicliques rest-steps))))))

(define less-than-3o (lambda (hyperedge)
    (fresh (n)
        (conde
            ((== n '(z)))
            ((== n '(s z)))
            ((== n '(s s z))))
            
        (sizeo hyperedge n))))

(define any-less-than-3o (lambda (h)
    (fresh (car-h cdr-h)
        (== h `(,car-h . ,cdr-h))
        (conde
            ((less-than-3o car-h))
            ((any-less-than-3o cdr-h))))))

(define sizeo (lambda (hyperedge size)
    (conde
        ((== hyperedge '()) (== size '(z)))
        ((fresh (first rest size-rec)
            (== hyperedge `(,first . ,rest))
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


(define all-has-botho (lambda (proof-string biclique-edges)
    (conde
        ((== biclique-edges '()))
        ((fresh (car-l cdr-l)
            (== biclique-edges `(,car-l . ,cdr-l))
            (has-botho proof-string car-l)
            (all-has-botho proof-string cdr-l))))))
            
(define has-botho (lambda (proof-string edge)
    (fresh (x) ;; Needed for some reason to complete the conjunction
        (has-lo proof-string edge)
        (has-ro proof-string edge))))
                
(define has-lo (lambda (proof-string edge)
    (fresh (car-proof-string cdr-proof-string car-edge cdr-edge)
        (== proof-string `(,car-proof-string . ,cdr-proof-string))
        (== edge `(,car-edge . ,cdr-edge))
        (conde
            ((== car-proof-string 'l) (== car-edge 1))
            ((has-lo cdr-proof-string cdr-edge))))))

(define has-ro (lambda (proof-string edge)
    (fresh (car-proof-string cdr-proof-string car-edge cdr-edge)
        (== proof-string `(,car-proof-string . ,cdr-proof-string))
        (== edge `(,car-edge . ,cdr-edge))
        (conde
            ((== car-proof-string 'r) (== car-edge 1))
            ((has-ro cdr-proof-string cdr-edge))))))


(define all-pairso (lambda (l out)
    (conde
        ((fresh (a b)
            (== l `(,a ,b)) (== out `((,a ,b)))))
            
        ((fresh (a b c)
            (== l `(,a ,b ,c))
            (== out `((,a ,b) (,a ,c) (,b ,c)))))
        
        ((fresh (a b c cdr-l out-rec my-pairs)
            (== l `(,a ,b ,c . ,cdr-l))
            (prefix-allo a `(,b ,c . ,cdr-l) my-pairs)
            (appendo my-pairs out-rec out)
            (all-pairso `(,b ,c . ,cdr-l) out-rec))))))

(define graph-has-bicliqueso (lambda (proof-string graph)
    (fresh (pairs filtered-pairs edges rest-graph)
        (all-pairs-indexedo proof-string pairs)
        (filter-is-lr-pairo pairs filtered-pairs)
        (map-needed-edgeo filtered-pairs edges)
        (riffleo edges rest-graph graph))))
            
(define filter-is-lr-pairo (lambda (l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l cdr-o)
            (== l `(,car-l . ,cdr-l))
            
            (conde
                ((== o `(,car-l . ,cdr-o))
                    (is-lr-pairo car-l))
                ((== o cdr-o)
                    (not-is-lr-pairo car-l)))
            
            (filter-is-lr-pairo cdr-l cdr-o))))))

(define is-lr-pairo (lambda (indexed-pair)
    (fresh (l1 x l2 y l3 z)
        (== indexed-pair `(,l1 ,x ,l2 ,y ,l3))
        (== z `(,x ,y))
        (conde
            ((== z '(l r)))
            ((== z '(r l)))))))
            
(define not-is-lr-pairo (lambda (indexed-pair)
    (fresh (l1 x l2 y l3 z)
        (== indexed-pair `(,l1 ,x ,l2 ,y ,l3))
        (== z `(,x ,y))
        (conde
            ((== x '(l l)))
            ((== x '(o l)))
            ((== z '(l o)))
            ((== z '(o o)))
            ((== z '(r o)))
            ((== z '(o r)))
            ((== z '(r r)))))))

(define map-needed-edgeo (lambda (l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l car-o cdr-o)
            (== l `(,car-l . ,cdr-l))
            (== o `(,car-o . ,cdr-o))
            (needed-edgeo car-l car-o)
            (map-needed-edgeo cdr-l cdr-o))))))


(define needed-edgeo (lambda (indexed-pair edge)
    (fresh (l1 x l2 y l3 l1-zeroes l2-zeroes l3-zeroes o1o o1o1o)
        (== indexed-pair `(,l1 ,x ,l2 ,y ,l3))
        (== o1o1o edge)
        (map-zeroo l1 l1-zeroes)
        (map-zeroo l2 l2-zeroes)
        (map-zeroo l3 l3-zeroes)
        (appendo l1-zeroes `(1 . ,l2-zeroes) o1o)
        (appendo o1o `(1 . ,l3-zeroes) o1o1o))))
        
(define map-zeroo (lambda (l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l car-o cdr-o)
            (== l `(,car-l . ,cdr-l))
            (== o `(,car-o . ,cdr-o))
            (zeroo car-o)
            (map-zeroo cdr-l cdr-o))))))

(define zeroo (lambda (x)
    (== x 0)))
            
(define prefixo (lambda (x l o)
    (== o `(,x ,l))))

(define prefix-allo (lambda (x l o)
    (conde
        ((==  l '()) (== o '()))
        ((fresh (car-l cdr-l car-o cdr-o)
            (== l `(,car-l . ,cdr-l))
            (== o `(,car-o . ,cdr-o))
            (prefixo x car-l car-o)
            (prefix-allo x cdr-l cdr-o))))))

(define appendo (lambda (l r o)
    (conde
        ((== l '()) (== r o))
        ((fresh (car-l cdr-l cdr-o)
            (== l `(,car-l . ,cdr-l))
            (== o `(,car-l . ,cdr-o))
            (appendo cdr-l r cdr-o))))))
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
    
    
(define failure-hypergraph '(
	(1 1 1 0 0 0 0 0 0 0 0)
	(0 0 0 1 1 1 0 0 0 0 0)
	(0 0 0 0 0 0 1 1 1 1 1)))
	
(define failure-bicliques '(
	(o o l r o o o o o o o)
	(o o o o o l r o o o o)))
(define is-zipper-listo (lambda (zipper-list list)
    (fresh (l x r)
        (== zipper-list `(,l ,x ,r))
        (appendo l `(,x . ,r) list))))
;; i.e, (windowo '(a b c) '((() a (b c)) ((a) b (c)) ((a b) c ())))
(define windowo (lambda (l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l car-o cdr-o cdr-l-zipper-lists)
            (== l `(,car-l . ,cdr-l))
            (== o `(,car-o . ,cdr-o))
            (== car-o `(() ,car-l ,cdr-l))
            (map-shoveo car-l cdr-l-zipper-lists cdr-o)
            (windowo cdr-l cdr-l-zipper-lists))))))

(define shoveo (lambda (a l o)
    (fresh (left x right)
        (== l `(,left ,x ,right))
        (== o `((,a . ,left) ,x ,right)))))
(define window2o (lambda (l o)
    (fresh (zipper-lists)
        (windowo l zipper-lists)
        (flatmap-window2-helpero zipper-lists o))))

(define window2-helpero (lambda (zipper-list indexed-pairs)
    (fresh (l x r triples)
        (== zipper-list `(,l ,x ,r))
        (windowo r triples)
        (map-make-indexed-pairo l x triples indexed-pairs))))

(define make-indexed-pairo (lambda (l x triple o)
    (fresh (m y r)
        (== triple `(,m ,y ,r))
        (== o `(,l ,x ,m ,y ,r)))))
(define flatmap-window2-helpero (lambda (l o)
    (conde
        ((== l '()) (== o '()))
        
        ((fresh (car-l cdr-l o-left o-right)
            (== l `(,car-l . ,cdr-l))
            (window2-helpero car-l o-left)
            (appendo o-left o-right o)
            (flatmap-window2-helpero cdr-l o-right))))))

(define map-shoveo (lambda (a l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l car-o cdr-o)
            (== l `(,car-l . ,cdr-l))
            (== o `(,car-o . ,cdr-o))
            (shoveo a car-l car-o)
            (map-shoveo a cdr-l cdr-o))))))

(define map-make-indexed-pairo (lambda (l x triples o)
    (conde
        ((== triples '()) (== o '()))
        ((fresh (car-triples cdr-triples car-o cdr-o)
            (== triples `(,car-triples . ,cdr-triples))
            (== o `(,car-o . ,cdr-o))
            (make-indexed-pairo l x car-triples car-o)
            (map-make-indexed-pairo l x cdr-triples cdr-o))))))
(define prime-factorso (lambda (x primes)
    (fresh (y)
        (== y `(,x ,primes))
        (conde
            ((== y '(1 ())))
            ((== y '(2 (2))))
            ((== y '(3 (3))))
            ((== y '(4 (2 2))))
            ((== y '(5 (5))))
            ((== y '(6 (2 3))))
            ((== y '(7 (7))))
            ((== y '(8 (2 2 2))))
            ((== y '(9 (3 3))))))))
(define flatmap-prime-factorso (lambda (l o)
    (conde
        ((== l '()) (== o '()))
        ((fresh (car-l cdr-l o-left o-right)
            (== l `(,car-l . ,cdr-l))
            (prime-factorso car-l o-left)
            (appendo o-left o-right o)
            (flatmap-prime-factorso cdr-l o-right))))))
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

