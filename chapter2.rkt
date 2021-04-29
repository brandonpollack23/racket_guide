#lang racket

(define (mymap fun ls)
  (reverse (myfoldl (λ (a result)
                      (cons (fun a) result))
                    '()
                    ls)))

(define (myfoldl fun initial ls)
  (define (myfoldl/helper fun ls result)
    (if (empty? ls)
        result 
        (let* ([new-result (fun (first ls) result)]
               [rst (rest ls)])
          (myfoldl/helper fun rst new-result))))

  (myfoldl/helper fun ls initial))

(define (my-length ls)
  (myfoldl (λ (_ acc) (+ acc 1)) 0 ls))

(define (remove-dups l)
  (cond
    [(empty? l) empty]
    [(empty? (rest l)) l]
    [else
     (let ([curr (first l)]
           [next (first (rest l))])
       (if (equal? curr next)
           (remove-dups (rest l))
           (cons curr (remove-dups (rest l)))))]))

(define (myflatten ls)
  (cond
    [(empty? ls) empty]
    [else 
     (let ([hd (car ls)]
           [rst (myflatten (cdr ls))])
       (if (list? hd)
           (append (myflatten hd) (myflatten rst))
           (cons hd (myflatten rst))))]))
