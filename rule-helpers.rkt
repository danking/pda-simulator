#lang typed/racket

(require rackdan/maybe
         "data.rkt"
         )

(provide rules
         =>
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

(: => : (All (StackElement InputElement Stack Input)
             ((State StackElement InputElement Stack Input)
              (Input -> Input)
              (Stack -> Stack)
              ->
              [TransitionProcedure StackElement InputElement Stack Input])))
(define (=> st modify-input modify-stack)
  (lambda: ([σ : Stack] [γ : Input])
    (pda-instance
     st
     (modify-stack σ)
     (modify-input γ))))


