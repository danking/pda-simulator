#lang typed/racket

(require rackdan/maybe
         "data.rkt"
         "matchers.rkt"
         )

(provide runPDA
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sa string-append)

(: runPDA : (All (StackPeek InputPeek Stack Input)
                 ([PDAInstance StackPeek InputPeek Stack Input]
                  (Stack -> [Maybe StackPeek])
                  (Input -> [Maybe InputPeek])
                  ([PDAInstance StackPeek InputPeek Stack Input] -> Boolean)
                  ->
                  [PDAInstance StackPeek InputPeek Stack Input])))
(define/match (runPDA pdai stack-peeker input-peeker final?)
  [((pda-instance$ ς σ γ) _ _ _)
   (if (final? (pda-instance ς σ γ))
       (pda-instance ς σ γ)
       (match ((get-state-transition ς (stack-peeker σ) (input-peeker γ))
               σ γ)
         [#f (error 'runPDA
                    (sa "No transition from state ~a with stack ~a and input ~a, "
                        "with a stack peek of ~a and an input peek of ~a")
                    ς σ γ (stack-peeker σ) (input-peeker γ))]
         [(pda-instance ς′ σ′ γ′)
          (runPDA (pda-instance ς′ σ′ γ′) stack-peeker input-peeker final?)]))])
