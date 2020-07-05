#lang racket

;;
;; Exercise 2.53
;;
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(list 'a 'b 'c)                              ;; => '(a b c)
(list (list 'george))                        ;; => '((george))
(cdr '((x1 x2) (y1 y2)))                     ;; => '((y1 y2))
(cadr '((x1 x2) (y1 y2)))                    ;; => '(y1 y2)
(pair? (car '(a short list)))                ;; => #f
(memq 'red '((red shoes) (blue socks))       ;; => #f
(memq 'red '(red shoes blue socks))          ;; => '(red shoes blue socks)

;;
;; Exercise 2.54
;;
(define (equal? lst-a lst-b)
  (if (or (null? lst-a) (null? lst-b))
      (and (null? lst-a) (null? lst-b))
      (let ([ca (car lst-a)]
            [cb (car lst-b)])
        (cond [(and (null? ca) (null? cb)) #t]
              [(and (symbol? ca) (symbol? cb))
               (and (eq? ca cb) (equal? (cdr lst-a) (cdr lst-b)))]
              [(and (pair? ca) (pair? cb))
               (and (equal? ca cb) (equal? (cdr lst-a) (cdr lst-b)))]
              [else #f]))))

(equal? '() '())
(equal? '(a) '(blue))
(equal? '(a) '(a))
(equal? '((a b) (a c)) '((a b) (a c)))

;;
;; Exercise 2.55
;;
(car ''abracadadabra)
;; Evalutes to the symbol 'quote', because 'x is shorthand for (quote x)
;; So the above is the same as:
(car '(quote abracadabra))
;; Which can be further exponded to
(car (quote (quote abracadabra)))
;; The first call to quote quotes its argument "(quote abracadabra)"
;; resulting in a list containing the symbols 'quote and 'abracadabra
;;
;; The car of this list is the symbol 'quote, which is returned

;;
;; Exercise 2.66
;;
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-exponentiation b e)
  (cond [(=number? e 1) b]
        [(=number? e 0) 1]
        [(=number? b 1) 1]
        [(=number? b 0) 0]
        [else (list '** b e)]))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp)))]
        [(exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1)))]
        [else (error "unknown expression
                      type: DERIV" exp)]))

;;
;; Exercise 2.57
;;
(define (make-sum a1 . a2)
  (define (assemble number-part symbol-part)
    (cond
      ;; The sum is a pure number, return it
      [(null? symbol-part)
       number-part]
      ;; Single symbol and number is zero, return the symbol
      [(and (= 0 number-part) (null? (cdr symbol-part)))
       (car symbol-part)]
      ;; The number is zero, so just return the sum of the symbols
      [(= 0 number-part) (cons '+ symbol-part)]
      ;; General case
      [else
       (cons '+ (cons number-part symbol-part))]))
  (define (aggregate numberaccum symbolaccum lefttoprocess)
    (cond
      ;; Nothing left to process so assemble the result
      [(null? lefttoprocess)
       (assemble numberaccum symbolaccum)]
      ;; Process a number
      [(number? (car lefttoprocess))
       (aggregate (+ (car lefttoprocess) numberaccum) symbolaccum (cdr lefttoprocess))]
      ;; Process a symbol
      [else (aggregate numberaccum (cons (car lefttoprocess) symbolaccum) (cdr lefttoprocess))]))
  (aggregate 0 '() (cons a1 a2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))
(define (make-product a1 . a2)
  (define (assemble number-part symbol-part)
    (cond
      ;; The sum is a pure number, return it
      [(null? symbol-part)
       number-part]
      ;; Single symbol and number is zero, return the symbol
      [(and (= 1 number-part) (null? (cdr symbol-part)))
       (car symbol-part)]
      ;; The number is zero, so just return the sum of the symbols
      [(= 1 number-part) (cons '* symbol-part)]
      ;; General case
      [else
       (cons '* (cons number-part symbol-part))]))
  (define (aggregate numberaccum symbolaccum lefttoprocess)
    (cond
      ;; Short circuit to 0 if numberaccum is zero
      [(=number? numberaccum 0) 0]
      ;; Nothing left to process so assemble the result
      [(null? lefttoprocess)
       (assemble numberaccum symbolaccum)]
      ;; Process a number
      [(number? (car lefttoprocess))
       (aggregate (* (car lefttoprocess) numberaccum) symbolaccum (cdr lefttoprocess))]
      ;; Process a symbol
      [else (aggregate numberaccum (cons (car lefttoprocess) symbolaccum) (cdr lefttoprocess))]))
  (aggregate 1 '() (cons a1 a2)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier s) (cadr s))
(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))

;;
;; Exercise 2.58
;;

;;
;; Part 1
;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (make-exponentiation b e)
  (cond [(=number? e 1) b]
        [(=number? e 0) 1]
        [(=number? b 1) 1]
        [(=number? b 0) 0]
        [else (list b '** e)]))
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base x) (car x))
(define (exponent x) (caddr x))

;;
;; Part 2
;;
(deriv '(x + 3 * (x + y + 2)) 'x)
