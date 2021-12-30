#lang racket
(provide (all-defined-out))

(define (append lst input_list)(
cond [(null? (cdr lst)) (cons (car lst) (cons input_list null))]
     [#t (cons [car lst] [append (cdr lst) input_list] )]
                            ))


(define (is_in string lst)(
cond [(null? lst) #f] [(equal? string (car (car lst))) #t] [#t (is_in string (cdr lst))]
                           ))


(define (E2_processor lst E2)(

cond [(null? E2) lst] [#t (E2_processor (append lst (car E2)) (cdr E2)) ]

                              ))

(define (E1_processor lst E1 E2)(
cond [(null? E1) lst] [(is_in (car (car E1)) E2) (E1_processor lst (cdr E1) E2)] [#t (E1_processor (append lst (car E1)) (cdr E1) E2) ]

                              ))


(define (merge_envs E1 E2) (
E1_processor (E2_processor (cons (car E2) null ) (cdr E2)  ) E1 E2

                            ))