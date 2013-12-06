#lang racket

(require rackdan/maybe
         "data.rkt"
         )

(provide state$
         pda-instance$
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match-expander state$
  (lambda (stx)
    (syntax-case stx ()
      [(_ name description id transition-table)
       #'(state name description id transition-table)])))

(define-match-expander pda-instance$
  (lambda (stx)
    (syntax-case stx ()
      [(_ ς σ γ)
       #'(pda-instance ς σ γ)])))
