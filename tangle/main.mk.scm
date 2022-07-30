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

(define produceso (lambda (moves g h)
    (fresh
        (cbs h1 h2 h3)

        (conde (
            (== moves (quote ()))
            (any-less-than-3o h)

        ) (
            (fresh (rest-moves cbs h1 h2 h3)
                (== moves `((,cbs ,h1 ,h2) . ,rest-moves))
                (riffleo `(,h1 ,h2) rest-h h)
                (waro g h1 h2 cbs h3)
                (produceso rest-moves g `(,h3 . ,rest-h)))

        ))
    )
))

;; (define waro (lambda (g h1 h2 h3)
;; ))


(define proof-stepo (lambda (step h1 h2 h3)
    (conde
        ((== step '()) (== h1 '()) (== h2 '()) (== h3 '()))
        ((fresh (car-h1 cdr-h1 car-h2 cdr-h2 car-h3 cdr-h3 car-step cdr-step)
            (== step `(,car-step . ,cdr-step))
            (== h1 `(,car-h1 . ,cdr-h1))
            (== h2 `(,car-h2 . ,cdr-h2))
            (== h3 `(,car-h3 . ,cdr-h3))
            (conde
                ((== car-h1 0) (== car-h2 0) (== car-h3 0) (== car-step 'o))
                ((== car-h1 1) (== car-h2 0) (== car-h3 0) (== car-step 'r1))
                ((== car-h1 1) (== car-h2 0) (== car-h3 1) (== car-step 'b1))
                ((== car-h1 0) (== car-h2 1) (== car-h3 0) (== car-step 'r2))
                ((== car-h1 0) (== car-h2 1) (== car-h3 1) (== car-step 'b2))
                ((== car-h1 1) (== car-h2 1) (== car-h3 1) (== car-step 'b)))
            (proof-stepo cdr-step cdr-h1 cdr-h2 cdr-h3))))))
(define nonempty-proof-stepo (lambda (step h1 h2 h3)
    (fresh (rest)
        (proof-stepo step h1 h2 h3)
        (riffleo '(r1 r2) rest step))))
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


