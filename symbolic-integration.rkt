#lang racket

;; Operators: 'sum 'pow 'mult

(define (is-sum? expr)
  (let ([op (first expr)]
        [subexps (rest expr)])
    (and (equal? op 'sum) (andmap is-expr? subexps))))

(define  (print-sum expr)
  (string-append " (+" (apply string-append (map print-expr (rest expr))) ") "))

(define (integrate-sum expr)
  (append '(sum) (apply list (map integrate-expr (rest expr)))))

(define (is-mult? expr)
  (if (or (not (equal? (length expr) 3)) (not (equal? (first expr) 'mult)))
      #f
      (let ([a (second expr)]
            [b (third expr)])
        (or (and (number? a) (is-expr? b)) (and (number? b) (is-expr? a))))))

(define  (print-mult expr)
  (let ([a (second expr)]
        [b (third expr)])
    (if (and (number? a) (is-expr? b))
        (string-append " (" (number->string a) " * " (print-expr b) ") ")
        (string-append " (" (print-expr a) " * " (number->string b) ") "))))

(define (integrate-mult expr)
  (let ([a (second expr)]
        [b (third expr)])
    (if (and (number? a) (is-expr? b))
        (list 'mult a (integrate-expr b))
        (list 'mult b (integrate-expr a)))))

(define (is-pow? expr)
  (if (or (not (equal? (length expr) 3)) (not (equal? (first expr) 'pow)))
      #f
      (let ([a (second expr)]
            [b (third expr)])
        (and (number? b) (equal? a 'x)))))

(define  (print-pow expr)
  (string-append " (" (symbol->string (second expr)) " ^ " (number->string (third expr)) ") "))

(define (integrate-pow expr)
  (let ([sym (second expr)]
        [idx (third expr)])
    (list 'mult (/ 1 (add1 idx)) (list 'pow sym (add1 idx))))) 

(define OPERATORS (list 'sum 'mult 'pow))
(define CHECK-FUNCTIONS (list is-sum? is-mult? is-pow?))
(define PRINT-FUNCTIONS (list print-sum print-mult print-pow))
(define INTEGRATE-FUNCTIONS (list integrate-sum integrate-mult integrate-pow))
(define OP-FUNC-PAIRS (for/list ([op OPERATORS]
                                 [fun CHECK-FUNCTIONS])
                        (cons op fun)))
(define OP-PRINT-PAIRS (for/list ([op OPERATORS]
                                 [fun PRINT-FUNCTIONS])
                        (cons op fun)))
(define OP-INTEGRATE-PAIRS (for/list ([op OPERATORS]
                                 [fun INTEGRATE-FUNCTIONS])
                        (cons op fun)))

(define (is-expr? expr)
  (cond [(or (number? expr) (equal? expr 'x)) #t]
        [(<= (length expr) 1) #f]
        [else (let ([op (first expr)])
                (and (member op OPERATORS) ((cdr (first (filter (lambda (p) (equal? (car p) op)) OP-FUNC-PAIRS))) expr)))]))

(define (print-expr expr)
  (cond [(number? expr) (string-append (number->string expr) " ")]
        [(equal? expr 'x) (string-append (symbol->string expr) " ")]
        [else (let ([op (first expr)])
                ((cdr (first (filter (lambda (p) (equal? (car p) op)) OP-PRINT-PAIRS))) expr))]))

(define (integrate-expr expr)
  (cond [(number? expr) (list 'mult expr 'x)]
        [(equal? expr 'x) (list 'pow 'x 2)]
        [else (let ([op (first expr)])
                ((cdr (first (filter (lambda (p) (equal? (car p) op)) OP-INTEGRATE-PAIRS))) expr))]))


(define exp1 (list 'sum (list 'mult 2 (list 'pow 'x 3)) (list 'pow 'x 5) 'x 4)) 
(define exp2 '(sum '(mult 2 '(pow x 3)) '(pow x 5) x 4))
(is-expr? exp1)
(is-expr? exp2)
(print-expr exp1)
(define exp1-int (integrate-expr exp1))
(print-expr exp1-int)