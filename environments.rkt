#lang racket

(provide (all-defined-out))

;; Expansion

;; represents a “syntactic namespace”
(struct Synspace
  (name)
  #:transparent)

;; a frame maps names to binding sets, where
;;  a binding set maps synspaces to phase vector, where
;;    a phase vector holds a meaning for each phase
;; there is a frame for each level of scope
(struct Syntactic-Environment
  (frames) ; list of map of string → (map of Synspace → vector of meanings)
  #:transparent)

;; make new empty frame
(define (new-frame)
  (make-hash))

;; make new empty map of synspaces to phase vectors
(define (new-synspace-map)
  (make-hasheq))

;; make new empty phase vector, up to given phase
(define (new-phase-vector [maximum-phase 0])
  (make-vector (+ maximum-phase 1) #f))

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
  (define result (hash-ref synspace-map synspace #f))
  (cond [(not result)
         (define v (new-phase-vector maximum-phase))
         (hash-set! synspace-map synspace v)
         (printf "frame-get-phases: synspace map afterwards: ~A" synspace-map)
         v]
        [(< (vector-length result) desired-length)
         (define v (vector-extend result desired-length))
         (hash-set! synspace-map synspace v)
         v]
        [else result]))

;; add binding to syntactic environment, given name and synspaces and phase
(define (add-syntactic-binding! name phase synspaces meaning se)
  (printf "I'm adding ~A~%" name)
  (let ([frame (most-recent-frame se)])
    (if (lookup-in-frame name phase synspaces frame)
        (error "Redefinition prohibited in the same scope")
        (for ([synspace synspaces])
          (let ([phases (frame-get-phases! frame name synspace phase)])
            (printf "  synspace map after2: ~A~%" (frame-get-synspace-map! frame name))
            (printf "  in ~A~%" phases)
            (vector-set! phases phase meaning)
            (printf "  phase vector after: ~A~%" phases)))))
  meaning)

;; look for binding in one frame, return #f if not found
(define (lookup-in-frame name phase synspaces frame)
  (let/ec found
    (for ([synspace synspaces])
      (printf "  in frame, meanings: ~A~%" (frame-get-phases! frame name synspace phase))
      (define result (vector-ref (frame-get-phases! frame name synspace phase)
                                 phase))
      (found result))
    #f))

;; look for binding in all frames, call continuation if not found
(define (lookup-syntactic name phase synspaces se [not-found unbound-error])
  (printf "Looking for ~A in ~A at ~A…~%" name synspaces phase)
  (let/ec found
    (for ([frame (in-frames se)])
      (define result (lookup-in-frame name phase synspaces frame))
      (if result
          (found result)
          (void)))
    (not-found name phase synspaces)))

(define (unbound-error name phase synspaces)
  (error (format "~A is unbound" name)))

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

;; Elaboration

(struct Elaboration-Environment
  (terms ; variable → type
   types ; type → meaning
   patterns ; constructor → arity and type
   modules))

;; Evaluation

(struct Runtime-Environment
  (values))