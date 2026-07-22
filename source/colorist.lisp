;;;; -- Colorist --

(in-package #:cl-colorist)


;;;; -- Capabilities --

(deftype color-level ()
  "The amount of ANSI color understood by a terminal."
  '(member :none :basic :indexed))

(defparameter *color-level* nil
  "Color-level override, or NIL to inspect the process environment.")

(defun colorist--getenv (name)
  "Return environment variable NAME without requiring an environment library."
  (let* ((package (or (find-package '#:uiop/os)
                      (find-package '#:uiop)))
         (symbol  (and package (find-symbol "GETENV" package))))
    (when (and symbol (fboundp symbol))
      (funcall symbol name))))

(defun colorist--contains-p (needle haystack)
  "Return true when HAYSTACK contains NEEDLE without regard to case."
  (and haystack
       (not (null (search needle haystack :test #'char-equal)))))

(defun terminal-color-level (&key (getenv #'colorist--getenv))
  "Infer a COLOR-LEVEL by calling GETENV with environment variable names.

The presence of NO_COLOR disables color.  TERM=dumb also disables it.  A
256-color TERM or a true-color COLORTERM selects :INDEXED, because indexed
color is the richest format this library emits.  Other environments select
:BASIC.  Supplying GETENV makes capability decisions straightforward to test
or embed in an unusual environment."
  (let ((no-color (funcall getenv "NO_COLOR"))
        (term      (funcall getenv "TERM"))
        (colorterm (funcall getenv "COLORTERM")))
    (cond ((not (null no-color))
           :none)
          ((and term (string-equal term "dumb"))
           :none)
          ((or (colorist--contains-p "256color" term)
               (colorist--contains-p "truecolor" colorterm)
               (colorist--contains-p "24bit" colorterm)
               (colorist--contains-p "256" colorterm))
           :indexed)
          (t
           :basic))))

(defun effective-color-level (&optional level)
  "Return validated LEVEL, the dynamic override, or terminal detection.

NIL means that no explicit level was supplied."
  (let ((effective-level (or level *color-level* (terminal-color-level))))
    (check-type effective-level color-level)
    effective-level))


;;;; -- Colors --

(deftype basic-color-name ()
  "A named color from the basic 16-color ANSI palette."
  '(member :black
           :red
           :green
           :yellow
           :blue
           :magenta
           :cyan
           :white
           :bright-black
           :bright-red
           :bright-green
           :bright-yellow
           :bright-blue
           :bright-magenta
           :bright-cyan
           :bright-white))

(defparameter +basic-color-names+
  '(:black
    :red
    :green
    :yellow
    :blue
    :magenta
    :cyan
    :white
    :bright-black
    :bright-red
    :bright-green
    :bright-yellow
    :bright-blue
    :bright-magenta
    :bright-cyan
    :bright-white)
  "The basic ANSI colors in SGR palette order.")

(defstruct (color
            (:constructor color--create (kind value fallback))
            (:predicate color-p)
            (:copier nil))
  "A validated basic or indexed terminal color.

KIND is :BASIC or :INDEXED.  VALUE is a basic color name or an xterm palette
index.  FALLBACK is the basic color name used when an indexed color is
rendered for a basic terminal."
  (kind     :basic :type (member :basic :indexed) :read-only t)
  (value    :white :type (or basic-color-name (integer 0 255)) :read-only t)
  (fallback nil :type (or null basic-color-name) :read-only t))

(defun basic-color-names ()
  "Return a fresh list of all basic ANSI color names."
  (copy-list +basic-color-names+))

(defun basic-color (name)
  "Return a COLOR for basic ANSI color NAME."
  (check-type name basic-color-name)
  (color--create :basic name nil))

(defun colorist--basic-color-name (designator)
  "Return the basic color name represented by DESIGNATOR."
  (cond ((typep designator 'basic-color-name)
         designator)
        ((and (color-p designator)
              (eq (color-kind designator) :basic))
         (color-value designator))
        (t
         (error 'type-error
                :datum designator
                :expected-type '(or basic-color-name
                                    (satisfies colorist--basic-color-p))))))

(defun colorist--basic-color-p (object)
  "Return true when OBJECT is a basic COLOR."
  (and (color-p object)
       (eq (color-kind object) :basic)))

(defun indexed-color (index &key (fallback :white))
  "Return xterm-256 color INDEX with a basic COLOR FALLBACK.

FALLBACK may be a basic color name or the result of BASIC-COLOR."
  (check-type index (integer 0 255))
  (color--create :indexed index (colorist--basic-color-name fallback)))

(defun colorist--color (designator)
  "Coerce a basic color name or COLOR DESIGNATOR to a COLOR."
  (cond ((color-p designator)
         designator)
        ((typep designator 'basic-color-name)
         (basic-color designator))
        (t
         (error 'type-error
                :datum designator
                :expected-type '(or color basic-color-name)))))

(defun colorist--basic-color-index (name)
  "Return NAME's zero-based position in the basic ANSI palette."
  (or (position name +basic-color-names+)
      (error 'type-error :datum name :expected-type 'basic-color-name)))

(defun colorist--basic-sgr-code (name background-p)
  "Return the basic SGR code for color NAME and destination BACKGROUND-P."
  (let ((index (colorist--basic-color-index name)))
    (+ index
       (if background-p
           (if (< index 8) 40 92)
           (if (< index 8) 30 82)))))

(defun colorist--color-codes (color background-p level)
  "Return SGR parameters for COLOR at LEVEL and destination BACKGROUND-P."
  (ecase level
    (:none
     nil)
    (:basic
     (list (colorist--basic-sgr-code
            (if (eq (color-kind color) :basic)
                (color-value color)
                (color-fallback color))
            background-p)))
    (:indexed
     (if (eq (color-kind color) :indexed)
         (list (if background-p 48 38) 5 (color-value color))
         (list (colorist--basic-sgr-code (color-value color) background-p))))))


;;;; -- Styles and rendering --

(defstruct (style
            (:constructor style--create
                (foreground background bold-p faint-p italic-p underline-p
                 reverse-p))
            (:predicate style-p)
            (:copier nil))
  "A validated collection of terminal text attributes."
  (foreground nil :type (or null color) :read-only t)
  (background nil :type (or null color) :read-only t)
  (bold-p     nil :type boolean :read-only t)
  (faint-p    nil :type boolean :read-only t)
  (italic-p   nil :type boolean :read-only t)
  (underline-p nil :type boolean :read-only t)
  (reverse-p  nil :type boolean :read-only t))

(defun make-style (&key foreground background bold faint italic underline reverse)
  "Return a validated STYLE from colors and text attribute flags.

FOREGROUND and BACKGROUND accept a basic color keyword or a COLOR object.
BOLD and FAINT are mutually exclusive intensity requests."
  (when (and bold faint)
    (error "A terminal style cannot be both bold and faint."))
  (style--create (and foreground (colorist--color foreground))
                 (and background (colorist--color background))
                 (not (null bold))
                 (not (null faint))
                 (not (null italic))
                 (not (null underline))
                 (not (null reverse))))

(defun colorist--style-codes (style level)
  "Return all SGR parameter codes for STYLE rendered at LEVEL."
  (append (when (style-bold-p style)      '(1))
          (when (style-faint-p style)     '(2))
          (when (style-italic-p style)    '(3))
          (when (style-underline-p style) '(4))
          (when (style-reverse-p style)   '(7))
          (when (style-foreground style)
            (colorist--color-codes (style-foreground style) nil level))
          (when (style-background style)
            (colorist--color-codes (style-background style) t level))))

(defun sgr-sequence (style &key (level (effective-color-level)))
  "Return the ANSI SGR sequence that activates STYLE at color LEVEL.

The empty string is returned for :NONE and for a style without attributes."
  (check-type style style)
  (let* ((effective-level (effective-color-level level))
         (codes           (unless (eq effective-level :none)
                            (colorist--style-codes style effective-level))))
    (if codes
        (format nil "~c[~{~d~^;~}m" (code-char 27) codes)
        "")))

(defun reset-sequence (&key (level (effective-color-level)))
  "Return the ANSI SGR reset sequence at color LEVEL, or the empty string."
  (if (eq (effective-color-level level) :none)
      ""
      (format nil "~c[0m" (code-char 27))))

(defun colorize (text style
                 &key
                   (level (effective-color-level))
                   (reset t))
  "Wrap TEXT in STYLE's SGR sequence at LEVEL.

When RESET is true, append an SGR reset after styled text.  TEXT is returned
unchanged when styling is disabled or STYLE has no effective attributes."
  (check-type text string)
  (check-type style style)
  (let ((prefix (sgr-sequence style :level level)))
    (if (zerop (length prefix))
        text
        (concatenate 'string
                     prefix
                     text
                     (if reset (reset-sequence :level level) "")))))

(defun paint (text
              &key
                foreground
                background
                bold
                faint
                italic
                underline
                reverse
                (level (effective-color-level))
                (reset t))
  "Colorize TEXT with a one-shot STYLE described by keyword arguments."
  (colorize text
            (make-style :foreground foreground
                        :background background
                        :bold bold
                        :faint faint
                        :italic italic
                        :underline underline
                        :reverse reverse)
            :level level
            :reset reset))


;;;; -- ANSI-aware text --

(defconstant +escape-character+ (code-char 27)
  "The ASCII escape character.")

(defun colorist--skip-csi (string start)
  "Return the index after a CSI body in STRING beginning at START."
  (loop for index from start below (length string)
        for code = (char-code (char string index))
        when (<= #x40 code #x7e)
          return (1+ index)
        finally (return (length string))))

(defun colorist--skip-control-string (string start)
  "Return the index after a BEL- or ST-terminated control string."
  (loop for index from start below (length string)
        for character = (char string index)
        for code = (char-code character)
        when (= code 7)
          return (1+ index)
        when (= code #x9c)
          return (1+ index)
        when (and (char= character +escape-character+)
                  (< (1+ index) (length string))
                  (char= (char string (1+ index)) #\\))
          return (+ index 2)
        finally (return (length string))))

(defun colorist--skip-escape (string start)
  "Return the index after STRING's escape sequence beginning at START."
  (let ((next (1+ start)))
    (if (>= next (length string))
        (length string)
        (case (char string next)
          (#\[
           (colorist--skip-csi string (1+ next)))
          ((#\] #\P #\X #\^ #\_)
           (colorist--skip-control-string string (1+ next)))
          (otherwise
           (loop with index = next
                 while (and (< index (length string))
                            (<= #x20 (char-code (char string index)) #x2f))
                   do (incf index)
                 finally (return (min (1+ index) (length string)))))))))

(defun ansi-control-end (string index)
  "Return the end of a control sequence at INDEX, or NIL for visible text."
  (check-type string string)
  (check-type index (integer 0))
  (unless (< index (length string))
    (error 'type-error
           :datum index
           :expected-type `(integer 0 ,(max 0 (1- (length string))))))
  (let ((code (char-code (char string index))))
    (cond ((char= (char string index) +escape-character+)
           (colorist--skip-escape string index))
          ((= code #x9b)
           (colorist--skip-csi string (1+ index)))
          ((member code '(#x90 #x98 #x9d #x9e #x9f))
           (colorist--skip-control-string string (1+ index)))
          ((<= #x80 code #x9f)
           (1+ index))
          (t
           nil))))

(defun strip-ansi (string)
  "Remove ANSI CSI, control-string, C1 and ordinary escape sequences.

Incomplete control sequences are discarded through the end of STRING."
  (check-type string string)
  (with-output-to-string (clean)
    (loop with index = 0
          while (< index (length string))
          for escape-end = (ansi-control-end string index)
          do (if escape-end
                 (setf index escape-end)
                 (progn
                   (write-char (char string index) clean)
                   (incf index))))))

(defun visible-length (string)
  "Return STRING's character count with ANSI control sequences ignored.

This is deliberately a character count, not a Unicode terminal-cell width.
It lets a caller apply its own grapheme and cell-width policy after ANSI
presentation has been removed."
  (check-type string string)
  (loop with index = 0
        with count = 0
        while (< index (length string))
        for escape-end = (ansi-control-end string index)
        do (if escape-end
               (setf index escape-end)
               (progn
                 (incf count)
                 (incf index)))
        finally (return count)))
