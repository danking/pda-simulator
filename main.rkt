#lang typed/racket

(require rackdan/maybe
         "data.rkt"
         "matchers.rkt"
         "runner.rkt"
         "rule-helpers.rkt"
         )

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack and Input Types

(struct: nt ([name : Symbol] [context : Symbol]) #:transparent)
(define-type StackElement (U nt Symbol))
(define-type Stack [Listof StackElement])
(define-type InputElement Symbol)
(define-type Input [Listof InputElement])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input and Stack Modifiers

(: stack-peeker : (Stack -> (Maybe StackElement)))
(define stack-peeker maybe-first)
(: input-peeker : (Input -> (Maybe InputElement)))
(define input-peeker maybe-first)
(: eos? : Input -> Boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Parse Definition

(define (eos? γ) (and (not (empty? γ)) (eq? '$ (first γ))))
(: final? : ((PDAInstance StackElement InputElement Stack Input) -> Boolean))
(define (final? pdai) (eos? (pda-instance-input pdai)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S -> a A b | b A a
;; A -> c S | ε
;; [Grune & Jacobs, 248-251]
(: st1 : (State StackElement InputElement Stack Input))
(define st1 (state "1" "" 0 #f #f))

(: st1-transition-table : (TT StackElement InputElement Stack Input))
(define st1-transition-table
  (let ((nt=> (curry nt=> st1))
        (t=> (curry t=> st1)))
    (rules StackElement InputElement Stack Input
           ;; Non-Terminals
           ((nt 'S '$) 'a   (nt=> (list 'a (nt 'A 'b) 'b)))
           ;;
           ((nt 'S 'a) 'a   (nt=> (list 'a (nt 'A 'b) 'b)))
           ;;
           ((nt 'S 'b) 'a   (nt=> (list 'a (nt 'A 'b) 'b)))
           ;;
           ((nt 'A 'a) 'a   (nt=> (list )))
           ;;
           ;;
           ((nt 'S '$) 'b   (nt=> (list 'b (nt 'A 'a) 'a)))
           ;;
           ((nt 'S 'a) 'b   (nt=> (list 'b (nt 'A 'a) 'a)))
           ;;
           ((nt 'S 'b) 'b   (nt=> (list 'b (nt 'A 'a) 'a)))
           ;;
           ((nt 'A 'b) 'b   (nt=> (list )))
           ;;
           ;;
           ((nt 'A 'a) 'c   (nt=> (list 'c (nt 'S 'a))))
           ;;
           ((nt 'A 'b) 'c   (nt=> (list 'c (nt 'S 'b))))
           ;; Terminals
           ('a         'a   (t=> (list )))
           ('b         'b   (t=> (list )))
           ('c         'c   (t=> (list )))
           )))

;; tie the knot
(set-state-transition-table! st1 st1-transition-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; must appear after the definition of st1

(: parse : (Input -> (PDAInstance StackElement InputElement Stack Input)))
(define (parse input)
  (define pdai (pda-instance st1 (list (nt 'S '$) '$) input))

  (runPDA pdai stack-peeker input-peeker final?))
