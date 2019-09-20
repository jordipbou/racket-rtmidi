#lang racket

(require ffi/unsafe
		 ffi/unsafe/atomic
		 ffi/unsafe/define
		 ffi/unsafe/define/conventions)

(provide RtMidiApi
         RtMidiErrorType
		 rtmidi-list-compiled-apis
		 rtmidi-get-compiled-api
		 rtmidi-api-name
		 rtmidi-api-display-name
		 rtmidi-compiled-api-by-name
     rtmidi-list-port-names
     rtmidi-list-all-port-names
		 rtmidi-open-port
		 rtmidi-open-virtual-port
		 rtmidi-close-port
		 rtmidi-get-port-count
		 rtmidi-get-port-name
		 rtmidi-in-create-default
		 rtmidi-in-create
		 rtmidi-in-free
		 rtmidi-in-get-current-api
		 rtmidi-in-set-callback
		 rtmidi-in-cancel-callback
		 rtmidi-in-ignore-types
		 rtmidi-in-get-message
		 rtmidi-out-create-default
		 rtmidi-out-create
		 rtmidi-out-free
		 rtmidi-out-get-current-api
		 rtmidi-out-send-message)

; TODO: Adapt this to load both on Linux, OSX and Windows
(define rtmidi-lib (ffi-lib "librtmidi" '("5" #f)))
(define-ffi-definer define-rtmidi 
					rtmidi-lib
					#:make-c-id convention:hyphen->underscore)

(define-cstruct _RtMidiWrapper ([ptr _pointer]
                                [data _pointer]
                                [ok _bool]
                                [msg _string]))

(define _RtMidiPtr _RtMidiWrapper-pointer)
(define _RtMidiInPtr _RtMidiWrapper-pointer)
(define _RtMidiOutPtr _RtMidiWrapper-pointer)

(define RtMidiApiList '(RTMIDI_API_UNSPECIFIED
						 RTMIDI_API_MACOSX_CORE
						 RTMIDI_API_LINUX_ALSA
						 RTMIDI_API_UNIX_JACK
						 RTMIDI_API_WINDOWS_MM
						 RTMIDI_API_RTMIDI_DUMMY
						 RTMIDI_API_NUM))
(define RtMidiApi (_enum RtMidiApiList))

(define RtMidiErrorType
  (_enum '(RTMIDI_ERROR_WARNING
           RTMIDI_ERROR_DEBUG_WARNING
           RTMIDI_ERROR_UNSPECIFIED
           RTMIDI_ERROR_NO_DEVICES_FOUND
           RTMIDI_ERROR_INVALID_DEVICE
           RTMIDI_ERROR_MEMORY_ERROR
           RTMIDI_ERROR_INVALID_PARAMETER
           RTMIDI_ERROR_INVALID_USE
           RTMIDI_ERROR_DRIVER_ERROR
           RTMIDI_ERROR_SYSTEM_ERROR
           RTMIDI_ERROR_THREAD_ERROR)))

; Helper function to help retrieve compiled apis
(define (rtmidi-list-compiled-apis)
  (let ((apis-bytes (make-bytes 6 0)))
	(rtmidi-get-compiled-api apis-bytes 6)
	(let ((apis-list 
			(map (lambda (e) 
				   (list-ref RtMidiApiList e)) 
				 (filter positive? (bytes->list apis-bytes)))))
	  apis-list)))

; rtmidi_c exported functions related to compiled APIs
(define-rtmidi rtmidi-get-compiled-api (_fun _bytes _int -> _int))
(define-rtmidi rtmidi-api-name (_fun RtMidiApi -> _string))
(define-rtmidi rtmidi-api-display-name (_fun RtMidiApi -> _string))
(define-rtmidi rtmidi-compiled-api-by-name (_fun _string -> RtMidiApi))

; Helper functions to list midi ports
(define (rtmidi-list-port-names api)
  (let ([total-ports (rtmidi-get-port-count api)])
    (let append-port ([l '()] [n (- total-ports 1)])
      (if (natural? n) 
        (append-port 
          (append (list (list n (rtmidi-get-port-name api n))) l) 
          (- n 1))
        l)))) 

(define (rtmidi-list-all-port-names)
  (let ([midi-in (rtmidi-in-create-default)]
        [midi-out (rtmidi-out-create-default)])
    (let ([insert-inout 
           (lambda (dir) 
             (lambda (port) 
               (append 
                 (list dir (car port)) 
                 (cdr port))))])
      (let ([ports-list
        (append 
          (map (insert-inout 'in) 
               (rtmidi-list-port-names midi-in))
          (map (insert-inout 'out) 
               (rtmidi-list-port-names midi-out)))])
        (rtmidi-in-free midi-in)
        (rtmidi-out-free midi-out)
        ports-list))))

; rtmidi_c exported functions related to generic port operations
(define-rtmidi rtmidi-open-port (_fun _RtMidiPtr _int _string -> _void))
(define-rtmidi rtmidi-open-virtual-port (_fun _RtMidiPtr _string -> _void))
(define-rtmidi rtmidi-close-port (_fun _RtMidiPtr -> _void))
(define-rtmidi rtmidi-get-port-count (_fun _RtMidiPtr -> _int))
(define-rtmidi rtmidi-get-port-name (_fun _pointer _int -> _string))

; rtmidi_c exported functions related to midi input specifically
(define-rtmidi rtmidi-in-create-default (_fun -> _RtMidiInPtr))
(define-rtmidi rtmidi-in-create (_fun RtMidiApi _string _int -> _RtMidiInPtr))
(define-rtmidi rtmidi-in-free (_fun _RtMidiInPtr -> _void))
(define-rtmidi rtmidi-in-get-current-api (_fun _RtMidiPtr -> RtMidiApi))

; MIDI In callback multi thread wrapper (not exported)
; This is needed because RtMidi creates a thread for each Midi Api
; instantiated and callback functions need to run inside those
; threads and so data received has to be sent to Racket main
; scheduler thread.

; Taken from https://gist.github.com/dchest/718858
; This part is the most complex of the ffi definition and requires
; an explanation:
; RtMidi starts a new thread for each midi api instantiated, and
; callback function sent from Racket's side will be called from that 
; thread. That generates an error, as Racket expects that function
; to be called only from its own thread.
; What it's done here is that Racket allows us to set a proxy
; callback function with #:async-apply enqueue in _RtMidiCCallback
; definition.
(define sema (make-semaphore))
(define queue null)
(define (enqueue thunk)
  (set! queue (append queue (list thunk)))
  (semaphore-post sema))
(define (dequeue)
  (semaphore-wait sema)
  (start-atomic)
  (let ([v (car queue)])
	(set! queue (cdr queue))
	(end-atomic)
	v))

; rtmidi_c callback function definition modified to use a proxy
; function, enqueue, that is called with racket's side defined
; callback. Enqueue pushes that callback function to a queue
; that will be called when 
(define _RtMidiCCallback 
  (_fun	#:async-apply enqueue 
		_double _bytes _size _racket -> _void))

; These two functions are the real rtmidi_c functions but are
; used internally, not exported.
(define rtmidi_in_set_callback
  (get-ffi-obj 
	"rtmidi_in_set_callback" rtmidi-lib
	(_fun _RtMidiInPtr _RtMidiCCallback _racket -> _void)))
(define rtmidi_in_cancel_callback
  (get-ffi-obj
	"rtmidi_in_cancel_callback" rtmidi-lib
	(_fun _RtMidiInPtr -> _void)))

; Callbacks are registered when calling rtmidi-in-set-callback
; to allow creating a background thread when a callback is 
; inserted, and killing that thread when no more callbacks are
; registered.
(define receiver-thread null)
(define setted-callbacks null)
(define (rtmidi-in-set-callback midi-ptr callback user-data)
  (when (member midi-ptr setted-callbacks)
    (rtmidi-in-cancel-callback midi-ptr))
  (rtmidi_in_set_callback midi-ptr callback user-data)
  (set! setted-callbacks (append setted-callbacks (list midi-ptr)))
  ; If this is the first callback set, start the receiver thread
  (when (= (length setted-callbacks) 1)
	(begin
      (set! receiver-thread
  	    (thread 
  	      (lambda ()
  	  	  (let loop ()
              (let ((thunk (dequeue)))
  	  		    (thunk)
  	  		    (loop)))))))))
(define (rtmidi-in-cancel-callback midi-ptr)
  (rtmidi_in_cancel_callback midi-ptr)
  (set! setted-callbacks (remove midi-ptr setted-callbacks))
  ; If this was the last callback, stop the receiver thread
  (when (= (length setted-callbacks) 0)
	(kill-thread receiver-thread)))

; exported rtmidi_c function to ignore certain message types
; in registered callback for that midi api pointer
(define-rtmidi rtmidi-in-ignore-types (_fun _RtMidiInPtr _bool _bool _bool -> _void))

; for getting a midi message, a 1024 bytes buffer is created and
; only the filled part (indicated by len) is returned 
(define-rtmidi rtmidi-in-get-message
  (_fun (in-ptr) ::
        (in-ptr : _RtMidiInPtr)
        (msg : (_bytes o 1024))
        (len : (_ptr o _size))
        -> (res : _double)
        -> (list (subbytes msg 0 len) res)))

; rtmidi_c exported functions related to midi out api
(define-rtmidi rtmidi-out-create-default (_fun -> _RtMidiOutPtr))
(define-rtmidi rtmidi-out-create (_fun RtMidiApi _string -> _RtMidiOutPtr))
(define-rtmidi rtmidi-out-free (_fun _RtMidiOutPtr -> _void))
(define-rtmidi rtmidi-out-get-current-api (_fun _RtMidiPtr -> RtMidiApi))
(define-rtmidi rtmidi-out-send-message (_fun _RtMidiOutPtr _bytes _int -> _int))
