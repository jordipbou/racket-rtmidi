#lang racket

(require ffi/unsafe
		 ffi/unsafe/define)

;(provide RtMidiErrorType
;		 rtmidi-get-compiled-api
;		 rtmidi-api-name
;		 rtmidi-api-display-name
;		 rtmidi-compiled-api-by-name
;		 rtmidi-open-port
;		 rtmidi-open-virtual-port
;		 rtmidi-close-port
;		 rtmidi-get-port-count
;		 rtmidi-get-port-name
;		 rtmidi-in-create-default
;		 rtmidi-in-create
;		 rtmidi-in-free
;		 rtmidi-in-get-current-api
;		 rtmidi-in-set-callback
;		 rtmidi-in-cancel-callback
;		 rtmidi-in-ignore-types
;		 rtmidi-in-get-message
;		 rtmidi-out-create-default
;		 rtmidi-out-create
;		 rtmidi-out-free
;		 rtmidi-out-get-current-api
;		 rtmidi-out-send-message)

;(define rtmidi-lib (ffi-lib "binaries/x64/rtmidi.dll" #f))
(define-ffi-definer define-rtmidi (ffi-lib "rtmidi/cmake-build/librtmidi.so"))

(define _RtMidiPtr (_cpointer 'RtMidiWrapper))
(define _RtMidiInPtr (_cpointer 'RtMidiWrapper))
(define _RtMidiOutPtr (_cpointer 'RtMidiWrapper))

(define _RtMidiCCallback 
  (_cpointer 
	(_fun _double _pointer _size _pointer -> _pointer)))

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

(define-rtmidi rtmidi_get_compiled_api (_fun _pointer _int -> _int))
(define-rtmidi rtmidi_api_name (_fun RtMidiApi -> _string)))

;(define rtmidi-api-display-name
;  (get-ffi-obj "rtmidi_api_display_name" rtmidi-lib
;			   (_fun RtMidiApi -> _string)))
;               
;(define rtmidi-compiled-api-by-name
;  (get-ffi-obj "rtmidi_compiled_api_by_name" rtmidi-lib
;			   (_fun _string -> RtMidiApi)))
;
;
;(define rtmidi-open-port
;  (get-ffi-obj "rtmidi_open_port" rtmidi-lib
;			   (_fun _RtMidiPtr _int _string -> _void)))
;
;(define rtmidi-open-virtual-port
;  (get-ffi-obj "rtmidi_open_virtual_port" rtmidi-lib
;			   (_fun _RtMidiPtr _string -> _void)))
;
;(define rtmidi-close-port
;  (get-ffi-obj "rtmidi_close_port" rtmidi-lib
;			   (_fun _RtMidiPtr -> _void)))
;
;(define rtmidi-get-port-count
;  (get-ffi-obj "rtmidi_get_port_count" rtmidi-lib
;			   (_fun _RtMidiPtr -> _int)))
;
;(define rtmidi-get-port-name
;  (get-ffi-obj "rtmidi_get_port_name" rtmidi-lib
;			   (_fun _pointer _int -> _string)))
;
;
;(define rtmidi-in-create-default
;  (get-ffi-obj "rtmidi_in_create_default" rtmidi-lib
;			   (_fun -> _pointer)))
;
;(define rtmidi-in-create
;  (get-ffi-obj "rtmidi_in_create" rtmidi-lib
;			   (_fun RtMidiApi _string _int -> _RtMidiInPtr)))
;
;(define rtmidi-in-free
;  (get-ffi-obj "rtmidi_in_free" rtmidi-lib
;			   (_fun _RtMidiInPtr -> _void)))
;
;(define rtmidi-in-get-current-api
;  (get-ffi-obj "rtmidi_in_get_current_api" rtmidi-lib
;			   (_fun _RtMidiPtr -> RtMidiApi)))
;
;(define rtmidi-in-set-callback
;  (get-ffi-obj "rtmidi_in_set_callback" rtmidi-lib
;			   (_fun _RtMidiInPtr _RtMidiCCallback _pointer -> _void)))
;
;(define rtmidi-in-cancel-callback
;  (get-ffi-obj "rtmidi_in_cancel_callback" rtmidi-lib
;			   (_fun _RtMidiInPtr -> _void)))
;
;(define rtmidi-in-ignore-types
;  (get-ffi-obj "rtmidi_in_ignore_types" rtmidi-lib
;			   (_fun _RtMidiInPtr _bool _bool _bool -> _void)))
;
;(define rtmidi-in-get-message
;  (get-ffi-obj "rtmidi_in_get_message" rtmidi-lib
;			   (_fun _RtMidiInPtr _pointer _size -> _double)))
;
;
;(define rtmidi-out-create-default
;  (get-ffi-obj "rtmidi_out_create_default" rtmidi-lib
;			   (_fun -> _RtMidiOutPtr)))
;
;(define rtmidi-out-create
;  (get-ffi-obj "rtmidi_out_create" rtmidi-lib
;			   (_fun RtMidiApi _string -> _RtMidiOutPtr)))
;
;(define rtmidi-out-free
;  (get-ffi-obj "rtmidi_out_free" rtmidi-lib
;			   (_fun _RtMidiOutPtr -> _void)))
;
;(define rtmidi-out-get-current-api
;  (get-ffi-obj "rtmidi_out_get_current_api" rtmidi-lib
;			   (_fun _RtMidiPtr -> RtMidiApi)))
;
;(define rtmidi-out-send-message
;  (get-ffi-obj "rtmidi_out_send_message" rtmidi-lib
;			   (_fun _RtMidiOutPtr _pointer _int -> _int)))
