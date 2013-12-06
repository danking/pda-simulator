#lang typed/racket

(require rackdan/maybe
         "data.rkt"
         )

(provide rules
         =>
         nt=>
         t=>
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Defining Helper Crap

(define-syntax-rule (rules StackPeek InputPeek Stack Input (a b result) ...)
  ((inst make-hash
         [List [Maybe StackPeek] [Maybe InputPeek]]
         [TransitionProcedure StackPeek InputPeek Stack Input])
   (list ((inst cons [List StackPeek InputPeek] [TransitionProcedure StackPeek InputPeek Stack Input])
          (list a b)
          result)
         ...)))

(: => : ((State StackElement InputElement Stack Input) (Input -> Input) [Listof StackElement]
         ->
         [TransitionProcedure StackElement InputElement Stack Input]))
(define (=> st modify-stack σ′-head)
  (lambda: ([σ : Stack] [γ : Input])
    (pda-instance
     st
     (append σ′-head (rest σ))
     (modify-stack γ))))

(: nt=> : ((State StackElement InputElement Stack Input) [Listof StackElement]
           ->
           [TransitionProcedure StackElement InputElement Stack Input]))
(define (nt=> st σ′-head)
  (=> st (lambda (x) x) σ′-head))

(: t=> : ((State StackElement InputElement Stack Input) [Listof StackElement]
          ->
          [TransitionProcedure StackElement InputElement Stack Input]))
(define (t=> st σ′-head)
  (=> st rest σ′-head))

