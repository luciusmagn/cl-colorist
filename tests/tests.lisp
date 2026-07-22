;;;; -- Tests --

(in-package #:cl-colorist/tests)

(defvar *check-count* 0
  "Number of assertions made by the current test run.")

(defparameter *compiled-style*
  #.(make-style
     :foreground (indexed-color 114 :fallback :green)
     :background (indexed-color 236 :fallback :black)
     :bold t)
  "A read-time STYLE that requires a load form when this file is compiled.")

(defun check-equal (expected actual)
  "Assert that EXPECTED and ACTUAL are EQUAL."
  (incf *check-count*)
  (unless (equal expected actual)
    (error "Expected ~S, got ~S." expected actual)))

(defun check-signals-error (function)
  "Assert that calling FUNCTION signals an ERROR."
  (incf *check-count*)
  (let ((signaled-p nil))
    (handler-case (funcall function)
      (error ()
        (setf signaled-p t)))
    (unless signaled-p
      (error "Expected an error, but the call returned."))))

(defun escape-sequence (body)
  "Return an ANSI escape sequence whose bytes after ESC are BODY."
  (format nil "~c~a" (code-char 27) body))

(defun test-basic-colors ()
  "Check all foreground and background basic ANSI colors."
  (let ((names '(:black :red :green :yellow :blue :magenta :cyan :white
                 :bright-black :bright-red :bright-green :bright-yellow
                 :bright-blue :bright-magenta :bright-cyan :bright-white)))
    (check-equal names (basic-color-names))
    (loop for name in names
          for foreground-code in '(30 31 32 33 34 35 36 37
                                   90 91 92 93 94 95 96 97)
          for background-code in '(40 41 42 43 44 45 46 47
                                   100 101 102 103 104 105 106 107)
          do (check-equal (escape-sequence (format nil "[~dm" foreground-code))
                          (sgr-sequence (make-style :foreground name)
                                        :level :basic))
             (check-equal (escape-sequence (format nil "[~dm" background-code))
                          (sgr-sequence (make-style :background name)
                                        :level :basic)))))

(defun test-indexed-colors ()
  "Check indexed colors and their basic fallbacks."
  (let ((green (indexed-color 114 :fallback :green)))
    (check-equal :indexed (color-kind green))
    (check-equal 114 (color-value green))
    (check-equal :green (color-fallback green))
    (check-equal (escape-sequence "[38;5;114m")
                 (sgr-sequence (make-style :foreground green)
                               :level :indexed))
    (check-equal (escape-sequence "[32m")
                 (sgr-sequence (make-style :foreground green)
                               :level :basic))
    (check-equal (escape-sequence "[48;5;236m")
                 (sgr-sequence
                  (make-style :background (indexed-color 236
                                                         :fallback :black))
                  :level :indexed)))
  (check-signals-error (lambda () (indexed-color -1)))
  (check-signals-error (lambda () (indexed-color 256)))
  (check-signals-error (lambda () (indexed-color 20 :fallback :orange))))

(defun test-style-rendering ()
  "Check attributes, reset behavior and one-shot rendering."
  (check-equal (escape-sequence "[1;38;5;114;48;5;236m")
               (sgr-sequence *compiled-style* :level :indexed))
  (check-equal (escape-sequence "[1;3;4;7;96;40m")
               (sgr-sequence (make-style :foreground :bright-cyan
                                         :background :black
                                         :bold t
                                         :italic t
                                         :underline t
                                         :reverse t)
                             :level :basic))
  (check-equal (escape-sequence "[2m")
               (sgr-sequence (make-style :faint t) :level :basic))
  (check-equal "" (sgr-sequence (make-style) :level :indexed))
  (check-equal "" (sgr-sequence (make-style :bold t) :level :none))
  (check-equal (escape-sequence "[0m") (reset-sequence :level :basic))
  (check-equal "" (reset-sequence :level :none))
  (check-equal (concatenate 'string
                            (escape-sequence "[1;32m")
                            "ready"
                            (escape-sequence "[0m"))
               (colorize "ready"
                         (make-style :foreground :green :bold t)
                         :level :basic))
  (check-equal (concatenate 'string (escape-sequence "[31m") "error")
               (paint "error" :foreground :red :level :basic :reset nil))
  (check-equal "plain" (paint "plain" :foreground :red :level :none))
  (check-signals-error (lambda () (make-style :bold t :faint t)))
  (check-signals-error (lambda () (make-style :foreground :orange))))

(defun test-color-levels ()
  "Check environment capability detection and dynamic overrides."
  (labels ((getter (bindings)
             (lambda (name)
               (rest (assoc name bindings :test #'string=)))))
    (check-equal :none
                 (terminal-color-level
                  :getenv (getter '(("NO_COLOR" . "")
                                    ("TERM" . "xterm-256color")))))
    (check-equal :none
                 (terminal-color-level
                  :getenv (getter '(("TERM" . "dumb")))))
    (check-equal :indexed
                 (terminal-color-level
                  :getenv (getter '(("TERM" . "xterm-256color")))))
    (check-equal :indexed
                 (terminal-color-level
                  :getenv (getter '(("TERM" . "xterm")
                                    ("COLORTERM" . "truecolor")))))
    (check-equal :basic
                 (terminal-color-level
                  :getenv (getter '(("TERM" . "xterm"))))))
  (let ((*color-level* :none))
    (check-equal :none (effective-color-level))
    (check-equal :indexed (effective-color-level :indexed)))
  (check-signals-error
   (lambda ()
     (let ((*color-level* :millions))
       (effective-color-level)))))

(defun test-ansi-parsing ()
  "Check stripping and visible counting for common ANSI forms."
  (let* ((escape (code-char 27))
         (styled (concatenate 'string
                              "a"
                              (escape-sequence "[31m")
                              "red"
                              (escape-sequence "[0m")
                              "b"))
         (osc (format nil "left~c]0;title~cright" escape (code-char 7)))
         (st-osc (format nil "left~c]8;;https://example.com~c\\link~c]8;;~c\\right"
                         escape escape escape escape))
         (dcs-with-bel
           (format nil "left~cPqhidden~cstill-hidden~c\\right"
                   escape (code-char 7) escape))
         (canceled-osc
           (format nil "left~c]title~cvisible" escape (code-char #x18)))
         (canceled-csi
           (format nil "left~c[31~cvisible" escape (code-char #x1a)))
         (charset (format nil "a~c(Bb" escape))
         (c1-csi (format nil "a~c31mb" (code-char #x9b))))
    (check-equal "aredb" (strip-ansi styled))
    (check-equal 5 (visible-length styled))
    (check-equal 6 (ansi-control-end styled 1))
    (check-equal nil (ansi-control-end styled 0))
    (check-equal "leftright" (strip-ansi osc))
    (check-equal "leftlinkright" (strip-ansi st-osc))
    (check-equal "leftright" (strip-ansi dcs-with-bel))
    (check-equal "leftvisible" (strip-ansi canceled-osc))
    (check-equal "leftvisible" (strip-ansi canceled-csi))
    (check-equal 11 (visible-length canceled-osc))
    (check-equal "ab" (strip-ansi charset))
    (check-equal "ab" (strip-ansi c1-csi))
    (check-equal "ok" (strip-ansi (format nil "ok~c[31" escape)))
    (check-equal 2
                 (visible-length
                  (paint "λx" :foreground :cyan :level :basic)))))

(defun run-tests ()
  "Run the cl-colorist test suite and return true on success."
  (setf *check-count* 0)
  (test-basic-colors)
  (test-indexed-colors)
  (test-style-rendering)
  (test-color-levels)
  (test-ansi-parsing)
  (format t "~&~:d cl-colorist checks passed.~%" *check-count*)
  t)
