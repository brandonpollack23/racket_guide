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

;; 2.3.4
