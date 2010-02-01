(define (fact x) (if (= x 1) 1 (* x (fact (- x 1)))))

; program testowy

(define res (fact (string->number (car args))))

(letrec ((even? (lambda (n) (if (= 0 n) #t (odd? (- n 1)))))
         (odd? (lambda (n) (if (= 0 n) #f (even? (- n 1)))))) (even? res))   

(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
       (sum res (+ sum (car x))))
      ((null? x) sum))) 
