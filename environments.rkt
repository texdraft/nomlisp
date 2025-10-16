#lang racket

(provide (all-defined-out))

(struct Module
  (name
   body ; unexpanded syntax object
   highest-phase ; integer
   expanded ; AST or #f
   elaborated)) ; AST (Record) or #f

;; Expansion

;; represents a “syntactic namespace”
(struct Synspace
  (name))

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

(define (empty-syntactic)
  (Syntactic-Environment (list (new-frame))))

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

;; add binding to syntactic environment, given name and synspaces and phase
(define (add-syntactic-binding! name phase synspaces meaning se)
  (let ([frame (most-recent-frame se)])
    (if (lookup-in-frame name phase synspaces frame)
        (error "Redefinition prohibited in the same scope")
        (for ([synspace synspaces])
          (let ([phases (frame-get-phases! frame name synspace phase)])
            (vector-set! phases phase meaning)))))
  meaning)

;; look for binding in one frame, return #f if not found
(define (lookup-in-frame name phase synspaces frame)
  (let/ec found
    (for ([synspace synspaces])
      (define result (vector-ref (frame-get-phases! frame name synspace phase)
                                 phase))
      (found result))
    #f))

;; look for binding in all frames, call continuation if not found
(define (lookup-syntactic name phase synspaces se not-found)
  (let/ec found
    (for ([frame (in-frames se)])
      (define result (lookup-in-frame name phase synspaces frame))
      (if result
          (found result)
          (void)))
    (not-found name phase synspaces)))

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

;; convert a list of (meaning, name, synspace, phase) into a
;; new syntactic environment
(define (reconstruct-syntactic bindings)
  (define se (empty-syntactic))
  (map (match-λ [(list meaning name synspace phase)
                 (add-syntactic-binding! name phase (list synspace) meaning se)])
       bindings)
  se)

;; create new environment where all bindings are taken from use-environment
;; except for those mentioned in free-names, which are taken from
;; captured-environment
(define (merge-free-names use-environment captured-environment free-names)
  (reconstruct-syntactic (map-bindings (λ (x) x)
                                       use-environment)))

;; Elaboration

(struct Elaboration-Environment
  (terms ; variable → type
   types ; type → meaning
   patterns ; constructor → arity and type
   modules))

;; Evaluation

(struct Runtime-Environment
  (values))