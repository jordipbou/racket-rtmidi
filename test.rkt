#lang racket

(let loop ()
  (log-error "o")
  (if (= 5 (random 10))
	(exit)
	(loop)))
