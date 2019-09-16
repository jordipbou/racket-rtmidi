#lang racket

(require ffi/unsafe
		 ffi/unsafe/atomic
		 ffi/unsafe/define
		 ffi/unsafe/define/conventions)

(provide RtMidiErrorType
		 rtmidi-get-compiled-api
		 rtmidi-api-name
		 rtmidi-api-display-name
		 rtmidi-compiled-api-by-name
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
		 rtmidi-in-dequeue
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
					;(ffi-lib "librtmidi" '("5" #f))
					rtmidi-lib
					#:make-c-id convention:hyphen->underscore)

(define-cstruct _RtMidiWrapper ([ptr _pointer]
                                [data _pointer]
                                [ok _bool]
                                [msg _string]))

(define _RtMidiPtr _RtMidiWrapper-pointer)
(define _RtMidiInPtr _RtMidiWrapper-pointer)
(define _RtMidiOutPtr _RtMidiWrapper-pointer)

(define RtMidiApi
  (_enum '(RTMIDI_API_UNSPECIFIED
           RTMIDI_API_MACOSX_CORE
           RTMIDI_API_LINUX_ALSA
           RTMIDI_API_UNIX_JACK
           RTMIDI_API_WINDOWS_MM
           RTMIDI_API_RTMIDI_DUMMY
           RTMIDI_API_NUM)))

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

(define-rtmidi rtmidi-get-compiled-api (_fun _pointer _int -> _int))
(define-rtmidi rtmidi-api-name (_fun RtMidiApi -> _string))
(define-rtmidi rtmidi-api-display-name (_fun RtMidiApi -> _string))
(define-rtmidi rtmidi-compiled-api-by-name (_fun _string -> RtMidiApi))

(define-rtmidi rtmidi-open-port (_fun _RtMidiPtr _int _string -> _void))
(define-rtmidi rtmidi-open-virtual-port (_fun _RtMidiPtr _string -> _void))
(define-rtmidi rtmidi-close-port (_fun _RtMidiPtr -> _void))
(define-rtmidi rtmidi-get-port-count (_fun _RtMidiPtr -> _int))
(define-rtmidi rtmidi-get-port-name (_fun _pointer _int -> _string))

(define-rtmidi rtmidi-in-create-default (_fun -> _RtMidiInPtr))
(define-rtmidi rtmidi-in-create (_fun RtMidiApi _string _int -> _RtMidiInPtr))
(define-rtmidi rtmidi-in-free (_fun _RtMidiInPtr -> _void))
(define-rtmidi rtmidi-in-get-current-api (_fun _RtMidiPtr -> RtMidiApi))

; MIDI In callback multi thread wrapper
; https://gist.github.com/dchest/718858
(define sema (make-semaphore))
(define queue null)
(define (enqueue thunk)
  (set! queue (append queue (list thunk)))
  (semaphore-post sema))
(define (rtmidi-in-dequeue)
  (semaphore-wait sema)
  (start-atomic)
  (let ([v (car queue)])
	(set! queue (cdr queue))
	(end-atomic)
	v))

(define _RtMidiCCallback 
  (_fun	#:async-apply enqueue _double _bytes _size _racket -> _void))

;(define-rtmidi 
;  rtmidi-in-set-callback 
;  (_fun _RtMidiInPtr _RtMidiCCallback _racket -> _void))
(define real-rtmidi-in-set-callback
  (get-ffi-obj "rtmidi_in_set_callback" rtmidi-lib
			   (_fun _RtMidiInPtr _RtMidiCCallback _racket -> _void)))

; Set a proxy between user callback and rtmidi to
; transform midi messages, as done on rtmidi-in-get-message
(define (rtmidi-in-set-callback midi-in-ptr callback user-data)
  (real-rtmidi-in-set-callback 
	midi-in-ptr 
	(lambda (timestamp msg len user-data)
	  (callback timestamp (bytes->list (subbytes msg 0 len)) user-data))
	user-data))

; Example of thread to run async calls in the background
;(thread-wait
;  (thread (lambda ()
;            (let loop ()
;		      (let ((thunk (dequeue)))
;			    (thunk)
;			    (loop))))))
; ----------

(define-rtmidi rtmidi-in-cancel-callback (_fun _RtMidiInPtr -> _void))
(define-rtmidi rtmidi-in-ignore-types (_fun _RtMidiInPtr _bool _bool _bool -> _void))

(define-rtmidi rtmidi-in-get-message
  (_fun (in-ptr) ::
        (in-ptr : _RtMidiInPtr)
        (msg : (_bytes o 10))
        (len : (_ptr o _size))
        -> (res : _double)
        -> (values (bytes->list (subbytes msg 0 len)) res)))

(define-rtmidi rtmidi-out-create-default (_fun -> _RtMidiOutPtr))
(define-rtmidi rtmidi-out-create (_fun RtMidiApi _string -> _RtMidiOutPtr))
(define-rtmidi rtmidi-out-free (_fun _RtMidiOutPtr -> _void))
(define-rtmidi rtmidi-out-get-current-api (_fun _RtMidiPtr -> RtMidiApi))
(define-rtmidi rtmidi-out-send-message (_fun _RtMidiOutPtr _bytes _int -> _int))
