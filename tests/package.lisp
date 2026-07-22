(defpackage #:cl-colorist/tests
  (:use #:cl)
  (:import-from #:cl-colorist
                #:*color-level*
                #:ansi-control-end
                #:basic-color
                #:basic-color-names
                #:color-fallback
                #:color-kind
                #:color-value
                #:colorize
                #:effective-color-level
                #:indexed-color
                #:make-style
                #:paint
                #:reset-sequence
                #:sgr-sequence
                #:strip-ansi
                #:terminal-color-level
                #:visible-length)
  (:export #:run-tests))

(in-package #:cl-colorist/tests)
