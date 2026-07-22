(defpackage #:cl-colorist
  (:nicknames #:colorist)
  (:use #:cl)
  (:export
   ;; Capabilities
   #:*color-level*
   #:color-level
   #:terminal-color-level
   #:effective-color-level

   ;; Colors
   #:basic-color-name
   #:basic-color-names
   #:color
   #:color-p
   #:color-kind
   #:color-value
   #:color-fallback
   #:basic-color
   #:indexed-color

   ;; Styles and rendering
   #:style
   #:style-p
   #:make-style
   #:style-foreground
   #:style-background
   #:style-bold-p
   #:style-faint-p
   #:style-italic-p
   #:style-underline-p
   #:style-reverse-p
   #:sgr-sequence
   #:reset-sequence
   #:colorize
   #:paint

   ;; ANSI-aware text
   #:ansi-control-end
   #:strip-ansi
   #:visible-length)
  (:documentation
   "Capability-aware ANSI colors, text styles and escape-free text parsing."))

(in-package #:cl-colorist)
