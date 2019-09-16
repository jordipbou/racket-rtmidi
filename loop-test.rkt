#lang racket

(require "rtmidi.rkt")

; Open inputs
(define midiin0 (rtmidi-in-create-default))
(rtmidi-open-port midiin0 0 "")
(rtmidi-in-set-callback midiin0
						(lambda (ts msg ud)
						  (display msg))
						#f)

(define midiin1 (rtmidi-in-create-default))
(rtmidi-open-port midiin1 1 "")
(rtmidi-in-set-callback midiin1
						(lambda (ts msg ud)
						  (display msg)
						  (display ud))
						'port-1)

; Very basic loop
;(let loop ()
;  (sleep 0.1)
;  (let-values
;	(((msg0 ts) (rtmidi-in-get-message midiin0)))
;	(let-values 
;	  (((msg1 ts) (rtmidi-in-get-message midiin1)))
;	  (begin
;	    (unless (equal? msg0 '()) (log-error (format "0: ~a" msg0)))
;	    (unless (equal? msg1 '()) (log-error (format "1: ~a" msg1)))
;	    (loop)))))

(thread-wait
  (thread 
	(lambda ()
	  (let loop ()
		(let ((callback (rtmidi-in-dequeue)))
		  (callback)
		  (loop))))))

; TODO: Esto molaría más:
; Pero habría que hacer que rtmidi-in-queue fuera del tipo evt
; (rtmidi-in-set-callback midiin) ;; Activar callback para midiin
; (let loop ()
;   (let ((msg (sync rtmidi-in-queue)))
;     (;Do something with msg;)))
; TODO: Permitiendo:
;   * Sincronizar sobre más de 1 entrada midi
;   * Sincronizar sobre otros eventos (timer, por ejemplo)

; Lo guay sería utilizar pattern matching, en plan Erlang para el loop.
