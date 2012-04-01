;; -*- mode: scheme48; scheme48-package: graph  -*-

(define-structure graph (export make-graph
				 add-edge!
				 add-directed-edge!
				 vertices-connected?
				 edge-weight-ref
				 edge-weight-set!
				 edge-ref
				 edge-set!
				 *vertices*
				 *internal-vertex-map*
				 *edge-weights*
				 vertex->int)
  (open scheme srfi-1 srfi-25)
  (begin
    ;; Constructors
    ;; Selectors
    ;; Recognizers
    ;; Mutators

    (define *internal-vertex-map* '())
    (define *vertices* '())
    (define *edge-weights* '())

    (define (make-internal-vertex-map vertices)
      (let ((i 0))
	(map (lambda (v)
	       (begin (set! i (+ 1 i))
		      (list v i)))
	     vertices)))

    (define (vertex->int v)
      (second (assoc v *internal-vertex-map*)))

    ;; (define (add-edge! v1 v2 G)
    ;;   (let* ((row (vertex->int v1))
    ;; 	     (col (vertex->int v2)))
    ;; 	(array-set! G row col 1)))

    (define (edge-ref v1 v2 G)
      (let* ((row (vertex->int v1))
	     (col (vertex->int v2))
	     (edge-count (array-ref G row col)))
	edge-count))

    (define (edge-set! n v1 v2 G)
      (let ((row (vertex->int v1))
	    (col (vertex->int v2)))
	(array-set! G row col n)))

    (define (add-directed-edge! v1 v2 G)
      (let ((curval (edge-ref v1 v2 G)))
	(edge-set! (+ 1 curval) v1 v2 G)))

    (define (add-edge! v1 v2 G)
      (begin (add-directed-edge! v1 v2 G)
	     (add-directed-edge! v2 v1 G)))

    (define (vertices-connected? v1 v2 G)
      (let ((row (vertex->int v1))
	    (col (vertex->int v2)))
	(if (array-ref G row col)
	    #t
	    #f)))

    (define (make-graph vertices)
      (let ((v (+ 1 (length vertices))))
	(begin
	  (set! *vertices* vertices)
	  (set! *internal-vertex-map*
	    (make-internal-vertex-map vertices))
	  (set! *edge-weights* (make-array (shape 0 v 0 v) 0))
	  (make-array (shape 0 v 0 v) 0))))

    (define (edge-weight-ref v1 v2 G)
      (let ((row (vertex->int v1))
	    (col (vertex->int v2)))
	(if (vertices-connected? v1 v2 G)
	    (array-ref *edge-weights* row col)
	    #f)))

    (define (edge-weight-set! n v1 v2 G)
      (let ((row (vertex->int v1))
	    (col (vertex->int v2)))
	(if (vertices-connected? v1 v2 G)
	    (array-set! *edge-weights* row col n)
	    #f)))

)) ;; Leave these here.
