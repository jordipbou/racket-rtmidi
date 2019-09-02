#lang racket
(require ffi/unsafe)

(define rtmidi-lib (ffi-lib "binaries/x64/rtmidi.dll" #f))

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
           RTMIDI_ERROR_THREAD_ERROR))
};

(define rtmidi-get-compiled-api
  (get-ffi-obj "rtmidi_get_compiled_api" rtmidi-lib
               (_fun _pointer _int -> _int)))
               
(define in-create-default
  (get-ffi-obj "rtmidi_in_create_default" rtmidi-lib
			   (_fun -> _pointer)))

(define get-port-name
  (get-ffi-obj "rtmidi_get_port_name" rtmidi-lib
			   (_fun _pointer _int -> _string/utf-8)))

(display (get-port-name (in-create-default) 1))
