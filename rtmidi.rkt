#lang racket
(require ffi/unsafe)

(define rtmidi-lib (ffi-lib "C:/labs/racket/rtmidi-4.0.0/cmake-build/Debug/rtmidi.dll" #f))


(define in-create-default
  (get-ffi-obj "rtmidi_in_create_default" rtmidi-lib
			   (_fun -> _pointer)))

(define get-port-name
  (get-ffi-obj "rtmidi_get_port_name" rtmidi-lib
			   (_fun _pointer _int -> _string/utf-8)))

(display (get-port-name (in-create-default) 1))
