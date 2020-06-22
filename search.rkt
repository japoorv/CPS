#lang racket
(require data/queue)
;; Code is heavily inspired from the code listing (search.lisp) of the Building Problem Solver by Forbus & Kleer (http://www.qrg.northwestern.edu/BPS/readme.html)
#|
The below code basically provides an interface for making CPS (Problem Solver)
which searches the whole problem space (from initial state) using some operators.
The interface is minimalistic in the sense that very less assumptions have been made in order to generalize it.
The idea and terms used are all from "Building Problem Solver" book. 
|#
(provide (struct-out problem))
(struct problem ( ;; struct for holding problem
                 name ;; name of the problem 
                 goal-recognizer ;; procedure for recognizing whether a state is the goal state or not
                 state-identical? ;; state1==state2 ? 
                 print-state ;; print state in appropriate form 
                 path-filter ;; Optional, select only paths that comply with path-filter ie path-filter(path) == #t
                 operators ;; List of all operators (along with their different operations as pair) that can be applied on any state, Hence eg (op1 . +) , (op1 . -) hence op1 can be applied in two ways thus operator . procedure
                 operator-applier ;; utility for applying particular operator to a state. Args : operator,state
                 ))
(provide (struct-out path))
(struct path ( ;; struct for holding path from initial state to some current state
             current-state ;; current state 
             so-far ;; alternating state, operation in the form of list (in stack form aka earlier states deeper in the list)
             pr ;; problem whose path has been made pointed to the appropriate problem struct 
             ) #:mutable #:transparent)
(provide bsolve)
(define (bsolve pr initial_state);; Solver function explores the problem space for possible paths to a goal 
  (let ([Qu (make-queue)]) ;; Qu is the queue for bfs
    (enqueue! Qu (path initial_state '() pr))
    (define (bfs)
      (when (non-empty-queue? Qu)
        (let ([Qfront (dequeue! Qu)])
          (cond
            [((problem-goal-recognizer pr) (path-current-state Qfront)) Qfront]
            [else (enqueue_list Qu (extend-path Qfront)) (bfs)]
            )
          )
        )
      )
    (bfs)
    )
  )
(define (extend-path path1)
  (let  ([return_lst  (make-queue)])
    (for/list ([op (problem-operators (path-pr path1))])
      (let ([new_state ((problem-operator-applier (path-pr path1)) op (path-current-state path1))])
        (cond
          [(loop? path1 new_state) (void)]
          [else (enqueue! return_lst (path new_state (cons (cons op (path-current-state path1)) (path-so-far path1)) (path-pr path1)))]
          )
        )
      )
    (queue->list return_lst)))

(define (loop? path_ state_)
  (ormap (lambda (x) ((problem-state-identical? (path-pr path_)) x state_)) (cons (path-current-state path_) (path-so-far path_)))
  )
(define (enqueue_list Qu lst)
  (map (lambda (x) (enqueue! Qu x)) lst))

