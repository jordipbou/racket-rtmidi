#lang info

(define collection "rtmidi")
(define version "1.0")

(define deps '("base"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rtmidi.scrbl" ())))
(define pkg-desc "FFI bindings for the C Api of the C++ RtMidi library (https://www.music.mcgill.ca/~gary/rtmidi/)")
(define pkg-authors '(jperez))
