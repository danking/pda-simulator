#lang typed/racket

(require rackdan/maybe
         )

(provide (struct-out state)
         (struct-out pda-instance)
         State
         PDAInstance
         TT
         TransitionProcedure
         get-state-transition
         state-writer
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct: (StackPeek InputPeek Stack Input)
         state
         ([name : String]
          [description : String]
          [id : Natural]
          [transition-table : (U False [TT StackPeek InputPeek Stack Input])]
          [final? : Boolean])
         #:transparent
         #:mutable
         ;; #:methods gen:custom-write
         ;; [(define write-proc state-writer)]
         )

(: state-writer : (All (StackPeek InputPeek Stack Input)
                       ((state StackPeek InputPeek Stack Input)
                        Output-Port
                        (U Zero One)
                        ->
                        Void)))
(define (state-writer s port mode)
  ;; wtf type system, port somehow gets the type `Any` unless I force it through
  ;; this indirection
  (: port2 : Output-Port)
  (define port2 port)
  (let ([name (state-name s)]
        [description (state-description s)]
        [id (state-id s)]
        [transitions (let ((t (state-transition-table s)))
                       (if t (hash-keys t) '()))]
        [final? (state-final? s)]

        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port2 mode))])])
    (let ([recur (lambda (x) (recur x port))])
     (write-string "(state ")
     (recur name)
     (recur description)
     (recur id)
     (recur transitions)
     (recur final?)
     (write-string ")")
     (void))))

(define-type [State StackPeek InputPeek Stack Input]
  (state StackPeek InputPeek Stack Input))

(struct: (StackPeek InputPeek Stack Input)
         pda-instance
         ([state : (State StackPeek InputPeek Stack Input)]
          [stack : Stack]
          [input : Input])
         #:transparent)

(define-type (PDAInstance StackPeek InputPeek Stack Input)
  (pda-instance StackPeek InputPeek Stack Input))

;; Here the false case for either the stack peek or the input peek indicates
;; that we've reached the end of the stack or the end of the input. This is
;; valid in either case
(define-type [TT StackPeek InputPeek Stack Input]
  [HashTable [List [Maybe StackPeek] [Maybe InputPeek]]
             [TransitionProcedure StackPeek InputPeek Stack Input]])

;; If this function returns #f then there was no valid transition, given the
;; stack and input, from the state associated with this rule
(define-type [TransitionProcedure StackPeek InputPeek Stack Input]
  [Stack Input -> [Maybe (PDAInstance StackPeek InputPeek Stack Input)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: get-state-transition : (All (StackPeek InputPeek Stack Input)
                               ([State StackPeek InputPeek Stack Input]
                                StackPeek
                                InputPeek
                                ->
                                [TransitionProcedure StackPeek InputPeek Stack Input])))
;; abstract over the hash-ref
(define (get-state-transition ς α β)
  (: failure-transition : [TransitionProcedure StackPeek InputPeek Stack Input])
  (define (failure-transition σ γ) #f)

  (let ((maybe-table (state-transition-table ς)))
    (if maybe-table
        ((inst hash-ref
               (List [Maybe StackPeek] [Maybe InputPeek])
               [TransitionProcedure StackPeek InputPeek Stack Input]
               [TransitionProcedure StackPeek InputPeek Stack Input])
         maybe-table
         (list α β)
         (lambda () failure-transition))
        (error 'get-state-transition "Uninitialized state: ~a" ς))))

