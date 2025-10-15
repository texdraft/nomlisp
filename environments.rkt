#lang racket

(provide (all-defined-out))

(struct Module
  (name
   body ; unexpanded syntax object
   highest-phase ; integer
   expanded ; AST or #f
   elaborated)) ; AST (Record) or #f

;; Expansion

;; represents “syntactic namespace”
(struct Synspace
  (name))

(define-syntax define-synspace
  (syntax-rules ()
    [(_ name1 name2)
     (define name1 (Synspace name2))]))

(define-synspace term-synspace "term")
(define-synspace type-synspace "type")
(define-synspace pattern-synspace "pattern")
(define-synspace module-synspace "module")
(define-synspace declaration-synspace "declaration")
(define-synspace synspace-synspace "synspace")

;; a frame maps names to binding sets, where
;;  a binding set maps synspaces to phase vector, where
;;    a phase vector holds a meaning for each phase
;; there is a frame for each level of scope
(struct Syntactic-Environment
  (frames)) ; list of map of string → (map of Synspace → vector of meanings)

;; make new empty frame
(define (new-frame)
  (make-hash))

;; make new empty map of synspaces to phase vectors
(define (new-synspace-map)
  (make-hasheq))

;; make new empty phase vector, up to given phase
(define (new-phase-vector [phases 1])
  (make-vector phases #f))

;; most recent frame of syntactic environment
(define (most-recent-frame se)
  (car (Syntactic-Environment-frames se)))

;; new syntactic environment with additional empty frame
(define (add-frame se)
  (Syntactic-Environment (cons (new-frame) (Syntactic-Environment-frames se))))

;; get synspace map for a name in a frame or add and return an empty one
(define (frame-get-synspace-map! frame name)
  (hash-ref! frame
             name
             (new-synspace-map)))

;; get phase vector for a synspace of a name in a frame or add and return
;; a new one, initialized to have maximum-phase slots
(define (frame-get-phases! frame name synspace [maximum-phase 0])
  (define desired-length (+ maximum-phase 1))
  (define synspace-map (frame-get-synspace-map! frame name))
  (define result (hash-ref synspace-map synspace))
  (cond [(not result)
         (hash-set! synspace-map synspace (new-phase-vector maximum-phase))]
        [(< (vector-length result) desired-length)
         (hash-set! synspace-map synspace (vector-extend result desired-length))]
        [else result]))

;; add binding to syntactic environment, given name and synspace and phase
(define (add-syntactic-binding! name phase synspace meaning se)
  (let ([frame (most-recent-frame se)])
    (if (lookup-in-frame name phase synspace frame)
        (error "Redefinition prohibited in the same scope")
        (let ([phases (frame-get-phases! frame name synspace phase)])
          (vector-set! phases phase meaning)))))

;; look for binding in one frame, return #f if not found
(define (lookup-in-frame name phase synspace frame)
  (vector-ref (frame-get-phases! frame name synspace phase)
              phase))

;; look for binding in all frames, call continuation if not found
(define (lookup-syntactic name phase synspace se not-found)
  (let/ec found
    (for ([frame (in-frames se)])
      (define result (lookup-in-frame name phase synspace frame))
      (if result
          (found result)
          (void)))
    (not-found name phase synspace)))

;; change most recent meaning given name and synspace and phase,
;; or add new entry
(define (update-meaning! name phase synspace meaning se)
  (if (lookup-syntactic name phase synspace se (const #f))
      (let/ec found
        (for ([frame (in-frames se)])
          (vector-set! (frame-get-phases! frame name synspace phase)
                       phase
                       meaning)))
      (add-syntactic-binding! name phase synspace meaning se))
  meaning)

;; iteration abstractions
(define (in-frames se)
  (Syntactic-Environment-frames se))
(define (in-frame frame)
  frame)

;; apply p to all bindings in se, collect results into list
(define (map-bindings p se)
  (for*/list ([frame (in-frames se)]
              [(name binding-set) frame]
              [(synspace phases) binding-set]
              [(phase meaning) (in-indexed phases)])
    (p meaning name synspace phase)))

;; gather bindings for which p is true,
;; calling p on (meaning, name, synspace, phase)
(define (filter-bindings p se)
  (filter (λ (data)
            (apply p data))
          (map-bindings list se)))

;; Elaboration

(struct Elaboration-Environment
  (terms ; variable → type
   types ; type → meaning
   patterns ; constructor → arity and type
   modules))

;; Evaluation

(struct Runtime-Environment
  (values))