;;;; test-suite/lib.scm --- generic support for testing
;;;; Copyright (C) 1999, 2000, 2001, 2004, 2006, 2007, 2009, 2010,
;;;;   2011, 2012, 2013, 2014 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.LESSER.
;;;; If not, write to the Free Software Foundation, Inc., 51 Franklin
;;;; Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (test-suite lib)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:autoload   (srfi srfi-1)  (append-map)
  #:autoload   (system base compile) (compile)
  #:export (

 ;; Exceptions which are commonly being tested for.
 exception:syntax-pattern-unmatched
 exception:bad-variable
 exception:missing-expression
 exception:out-of-range exception:unbound-var
 exception:used-before-defined
 exception:wrong-num-args exception:wrong-type-arg
 exception:numerical-overflow
 exception:struct-set!-denied
 exception:system-error
 exception:encoding-error
 exception:miscellaneous-error
 exception:string-contains-nul
 exception:read-error
 exception:null-pointer-error
 exception:vm-error

 ;; Reporting passes and failures.
 run-test
 pass-if expect-fail
 pass-if-equal
 pass-if-exception expect-fail-exception

 ;; Naming groups of tests in a regular fashion.
 with-test-prefix
 with-test-prefix*
 with-test-prefix/c&e
 current-test-prefix
 format-test-name

 ;; Using the debugging evaluator.
 with-debugging-evaluator with-debugging-evaluator*

 ;; Clearing stale references on the C stack for GC-sensitive tests.
 clear-stale-stack-references

 ;; Using a given locale
 with-locale with-locale* with-latin1-locale with-latin1-locale*

 ;; The bit bucket.
 %null-device

 ;; Reporting results in various ways.
 register-reporter unregister-reporter reporter-registered?
 make-count-reporter print-counts
 make-log-reporter
 full-reporter
 user-reporter))


;;;; If you're using Emacs's Scheme mode:
;;;;   (put 'with-test-prefix 'scheme-indent-function 1)


;;;; CORE FUNCTIONS
;;;;
;;;; The function (run-test name expected-result thunk) is the heart of the
;;;; testing environment.  The first parameter NAME is a unique name for the
;;;; test to be executed (for an explanation of this parameter see below under
;;;; TEST NAMES).  The second parameter EXPECTED-RESULT is a boolean value
;;;; that indicates whether the corresponding test is expected to pass.  If
;;;; EXPECTED-RESULT is #t the test is expected to pass, if EXPECTED-RESULT is
;;;; #f the test is expected to fail.  Finally, THUNK is the function that
;;;; actually performs the test.  For example:
;;;;
;;;;    (run-test "integer addition" #t (lambda () (= 2 (+ 1 1))))
;;;;
;;;; To report success, THUNK should either return #t or throw 'pass.  To
;;;; report failure, THUNK should either return #f or throw 'fail.  If THUNK
;;;; returns a non boolean value or throws 'unresolved, this indicates that
;;;; the test did not perform as expected.  For example the property that was
;;;; to be tested could not be tested because something else went wrong.
;;;; THUNK may also throw 'untested to indicate that the test was deliberately
;;;; not performed, for example because the test case is not complete yet.
;;;; Finally, if THUNK throws 'unsupported, this indicates that this test
;;;; requires some feature that is not available in the configured testing
;;;; environment.  All other exceptions thrown by THUNK are considered as
;;;; errors.
;;;;
;;;;
;;;; Convenience macros for tests expected to pass or fail
;;;;
;;;; * (pass-if name body) is a short form for
;;;;   (run-test name #t (lambda () body))
;;;; * (expect-fail name body) is a short form for
;;;;   (run-test name #f (lambda () body))
;;;;
;;;; For example:
;;;;
;;;;    (pass-if "integer addition" (= 2 (+ 1 1)))
;;;;
;;;;
;;;; Convenience macros to test for exceptions
;;;;
;;;; The following macros take exception parameters which are pairs
;;;; (type . message), where type is a symbol that denotes an exception type
;;;; like 'wrong-type-arg or 'out-of-range, and message is a string holding a
;;;; regular expression that describes the error message for the exception
;;;; like "Argument .* out of range".
;;;;
;;;; * (pass-if-exception name exception body) will pass if the execution of
;;;;   body causes the given exception to be thrown.  If no exception is
;;;;   thrown, the test fails.  If some other exception is thrown, it is an
;;;;   error.
;;;; * (expect-fail-exception name exception body) will pass unexpectedly if
;;;;   the execution of body causes the given exception to be thrown.  If no
;;;;   exception is thrown, the test fails expectedly.  If some other
;;;;   exception is thrown, it is an error.


;;;; TEST NAMES
;;;;
;;;; Every test in the test suite has a unique name, to help
;;;; developers find tests that are failing (or unexpectedly passing),
;;;; and to help gather statistics.
;;;;
;;;; A test name is a list of printable objects.  For example:
;;;; ("ports.scm" "file" "read and write back list of strings")
;;;; ("ports.scm" "pipe" "read")
;;;;
;;;; Test names may contain arbitrary objects, but they always have
;;;; the following properties:
;;;; - Test names can be compared with EQUAL?.
;;;; - Test names can be reliably stored and retrieved with the standard WRITE
;;;;   and READ procedures; doing so preserves their identity.
;;;;
;;;; For example:
;;;;
;;;;    (pass-if "simple addition" (= 4 (+ 2 2)))
;;;;
;;;; In that case, the test name is the list ("simple addition").
;;;;
;;;; In the case of simple tests the expression that is tested would often
;;;; suffice as a test name by itself.  Therefore, the convenience macros
;;;; pass-if and expect-fail provide a shorthand notation that allows to omit
;;;; a test name in such cases.
;;;;
;;;; * (pass-if expression) is a short form for
;;;;   (run-test 'expression #t (lambda () expression))
;;;; * (expect-fail expression) is a short form for
;;;;   (run-test 'expression #f (lambda () expression))
;;;;
;;;; For example:
;;;;
;;;;    (pass-if (= 2 (+ 1 1)))
;;;;
;;;; The WITH-TEST-PREFIX syntax and WITH-TEST-PREFIX* procedure establish
;;;; a prefix for the names of all tests whose results are reported
;;;; within their dynamic scope.  For example:
;;;;
;;;; (begin
;;;;   (with-test-prefix "basic arithmetic"
;;;;     (pass-if "addition" (= (+ 2 2) 4))
;;;;     (pass-if "subtraction" (= (- 4 2) 2)))
;;;;   (pass-if "multiplication" (= (* 2 2) 4)))
;;;;
;;;; In that example, the three test names are:
;;;;   ("basic arithmetic" "addition"),
;;;;   ("basic arithmetic" "subtraction"), and
;;;;   ("multiplication").
;;;;
;;;; WITH-TEST-PREFIX can be nested.  Each WITH-TEST-PREFIX appends
;;;; a new element to the current prefix:
;;;;
;;;; (with-test-prefix "arithmetic"
;;;;   (with-test-prefix "addition"
;;;;     (pass-if "integer" (= (+ 2 2) 4))
;;;;     (pass-if "complex" (= (+ 2+3i 4+5i) 6+8i)))
;;;;   (with-test-prefix "subtraction"
;;;;     (pass-if "integer" (= (- 2 2) 0))
;;;;     (pass-if "complex" (= (- 2+3i 1+2i) 1+1i))))
;;;;
;;;; The four test names here are:
;;;;   ("arithmetic" "addition" "integer")
;;;;   ("arithmetic" "addition" "complex")
;;;;   ("arithmetic" "subtraction" "integer")
;;;;   ("arithmetic" "subtraction" "complex")
;;;;
;;;; To print a name for a human reader, we DISPLAY its elements,
;;;; separated by ": ".  So, the last set of test names would be
;;;; reported as:
;;;;
;;;;   arithmetic: addition: integer
;;;;   arithmetic: addition: complex
;;;;   arithmetic: subtraction: integer
;;;;   arithmetic: subtraction: complex
;;;;
;;;; The Guile benchmarks use with-test-prefix to include the name of
;;;; the source file containing the test in the test name, to help
;;;; developers to find failing tests, and to provide each file with its
;;;; own namespace.


;;;; REPORTERS
;;;;
;;;; A reporter is a function which we apply to each test outcome.
;;;; Reporters can log results, print interesting results to the
;;;; standard output, collect statistics, etc.
;;;;
;;;; A reporter function takes two mandatory arguments, RESULT and TEST, and
;;;; possibly additional arguments depending on RESULT; its return value
;;;; is ignored.  RESULT has one of the following forms:
;;;;
;;;; pass         - The test named TEST passed.
;;;;                Additional arguments are ignored.
;;;; upass        - The test named TEST passed unexpectedly.
;;;;                Additional arguments are ignored.
;;;; fail         - The test named TEST failed.
;;;;                Additional arguments are ignored.
;;;; xfail        - The test named TEST failed, as expected.
;;;;                Additional arguments are ignored.
;;;; unresolved   - The test named TEST did not perform as expected, for
;;;;                example the property that was to be tested could not be
;;;;                tested because something else went wrong.
;;;;                Additional arguments are ignored.
;;;; untested     - The test named TEST was not actually performed, for
;;;;                example because the test case is not complete yet.
;;;;                Additional arguments are ignored.
;;;; unsupported  - The test named TEST requires some feature that is not
;;;;                available in the configured testing environment.
;;;;                Additional arguments are ignored.
;;;; error        - An error occurred while the test named TEST was
;;;;                performed.  Since this result means that the system caught
;;;;                an exception it could not handle, the exception arguments
;;;;                are passed as additional arguments.
;;;;
;;;; This library provides some standard reporters for logging results
;;;; to a file, reporting interesting results to the user, and
;;;; collecting totals.
;;;;
;;;; You can use the REGISTER-REPORTER function and friends to add
;;;; whatever reporting functions you like.  If you don't register any
;;;; reporters, the library uses FULL-REPORTER, which simply writes
;;;; all results to the standard output.


;;;; MISCELLANEOUS
;;;;

;;; Define some exceptions which are commonly being tested for.
(define exception:syntax-pattern-unmatched
  (cons 'syntax-error "source expression failed to match any pattern"))
(define exception:bad-variable
  (cons 'syntax-error "Bad variable"))
(define exception:missing-expression
  (cons 'misc-error "^missing or extra expression"))
(define exception:out-of-range
  (cons 'out-of-range "^.*out of range"))
(define exception:unbound-var
  (cons 'unbound-variable "^Unbound variable"))
(define exception:used-before-defined
  (cons 'unbound-variable "^Variable used before given a value"))
(define exception:wrong-num-args
  (cons 'wrong-number-of-args "^Wrong number of arguments"))
(define exception:wrong-type-arg
  (cons 'wrong-type-arg "^Wrong type"))
(define exception:numerical-overflow
  (cons 'numerical-overflow "^Numerical overflow"))
(define exception:struct-set!-denied
  (cons 'misc-error "^set! denied for field"))
(define exception:system-error
  (cons 'system-error ".*"))
(define exception:encoding-error
  (cons 'encoding-error "(cannot convert.* to output locale|input (locale conversion|decoding) error|conversion to port encoding failed)"))
(define exception:miscellaneous-error
  (cons 'misc-error "^.*"))
(define exception:read-error
  (cons 'read-error "^.*$"))
(define exception:null-pointer-error
  (cons 'null-pointer-error "^.*$"))
(define exception:vm-error
  (cons 'vm-error "^.*$"))

;; as per throw in scm_to_locale_stringn()
(define exception:string-contains-nul
  (cons 'misc-error "^string contains #\\\\nul character"))


;;; Display all parameters to the default output port, followed by a newline.
(define (display-line . objs)
  (for-each display objs)
  (newline))

;;; Display all parameters to the given output port, followed by a newline.
(define (display-line-port port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))


;;;; CORE FUNCTIONS
;;;;

;;; The central testing routine.
;;; The idea is taken from Greg, the GNUstep regression test environment.
(define run-test
  (let ((test-running #f))
    (lambda (name expect-pass thunk)
      (if test-running
          (error "Nested calls to run-test are not permitted."))
      (let ((test-name (full-name name)))
            (set! test-running #t)
            (catch #t
              (lambda ()
                (let ((result (thunk)))
                  (if (eq? result #t) (throw 'pass))
                  (if (eq? result #f) (throw 'fail))
                  (throw 'unresolved)))
              (lambda (key . args)
                (case key
                  ((pass)
                   (report (if expect-pass 'pass 'upass) test-name))
                  ((fail)
                   ;; ARGS may contain extra info about the failure,
                   ;; such as the expected and actual value.
                   (apply report (if expect-pass 'fail 'xfail)
                          test-name
                          args))
                  ((unresolved untested unsupported)
                   (report key test-name))
                  ((quit)
                   (report 'unresolved test-name)
                   (quit))
                  (else
                   (report 'error test-name (cons key args))))))
            (set! test-running #f)))))

;;; A short form for tests that are expected to pass, taken from Greg.
(define-syntax pass-if
  (syntax-rules ()
    ((_ name)
     ;; presume this is a simple test, i.e. (pass-if (even? 2))
     ;; where the body should also be the name.
     (run-test 'name #t (lambda () name)))
    ((_ name rest ...)
     (run-test name #t (lambda () rest ...)))))

(define-syntax pass-if-equal
  (syntax-rules ()
    "Succeed if and only if BODY's return value is equal? to EXPECTED."
    ((_ expected body)
     (pass-if-equal 'body expected body))
    ((_ name expected body ...)
     (run-test name #t
               (lambda ()
                 (let ((result (begin body ...)))
                   (or (equal? expected result)
                       (throw 'fail
                              'expected-value expected
                              'actual-value result))))))))

;;; A short form for tests that are expected to fail, taken from Greg.
(define-syntax expect-fail
  (syntax-rules ()
    ((_ name)
     ;; presume this is a simple test, i.e. (expect-fail (even? 2))
     ;; where the body should also be the name.
     (run-test 'name #f (lambda () name)))
    ((_ name rest ...)
     (run-test name #f (lambda () rest ...)))))

;;; A helper function to implement the macros that test for exceptions.
(define (run-test-exception name exception expect-pass thunk)
  (match exception
    ((expected-key . expected-pattern)
     (run-test
      name
      expect-pass
      (lambda ()
        (catch expected-key
          (lambda () (thunk) #f)
          (lambda (key proc message . rest)
            ;; Match the message against the expected pattern.  If that
            ;; doesn't work, in the case of `misc-error' and
            ;; `syntax-error' we treat the message as a format string,
            ;; and format it.  This is pretty terrible but it's
            ;; historical.
            (or (and (string-match expected-pattern message) #t)
                (and (memq expected-key '(misc-error syntax-error))
                     (list? rest)
                     (let ((out (apply simple-format #f message (car rest))))
                       (and (string-match expected-pattern out) #t)))
                (apply throw key proc message rest)))))))))

;;; A short form for tests that expect a certain exception to be thrown.
(define-syntax pass-if-exception
  (syntax-rules ()
    ((_ name exception body rest ...)
     (run-test-exception name exception #t (lambda () body rest ...)))))

;;; A short form for tests expected to fail to throw a certain exception.
(define-syntax expect-fail-exception
  (syntax-rules ()
    ((_ name exception body rest ...)
     (run-test-exception name exception #f (lambda () body rest ...)))))


;;;; TEST NAMES
;;;;

;;;; Turn a test name into a nice human-readable string.
(define (format-test-name name)
  (call-with-output-string
   (lambda (port)
     (let loop ((name name)
                (separator ""))
       (if (pair? name)
           (begin
             (display separator port)
             (display (car name) port)
             (loop (cdr name) ": ")))))))

;;;; For a given test-name, deliver the full name including all prefixes.
(define (full-name name)
  (append (current-test-prefix) (list name)))

;;; A fluid containing the current test prefix, as a list.
(define prefix-fluid (make-fluid '()))
(define (current-test-prefix)
  (fluid-ref prefix-fluid))

;;; Postpend PREFIX to the current name prefix while evaluting THUNK.
;;; The name prefix is only changed within the dynamic scope of the
;;; call to with-test-prefix*.  Return the value returned by THUNK.
(define (with-test-prefix* prefix thunk)
  (with-fluids ((prefix-fluid
		 (append (fluid-ref prefix-fluid) (list prefix))))
    (thunk)))

;;; (with-test-prefix PREFIX BODY ...)
;;; Postpend PREFIX to the current name prefix while evaluating BODY ...
;;; The name prefix is only changed within the dynamic scope of the
;;; with-test-prefix expression.  Return the value returned by the last
;;; BODY expression.
(define-syntax with-test-prefix
  (syntax-rules ()
    ((_ prefix body ...)
     (with-test-prefix* prefix (lambda () body ...)))))

(define-syntax c&e
  (syntax-rules (pass-if pass-if-equal pass-if-exception)
    "Run the given tests both with the evaluator and the compiler/VM."
    ((_ (pass-if exp))
     (c&e (pass-if "[unnamed test]" exp)))
    ((_ (pass-if test-name exp))
     (begin (pass-if (string-append test-name " (eval)")
                     (primitive-eval 'exp))
            (pass-if (string-append test-name " (compile)")
                     (compile 'exp #:to 'value #:env (current-module)))))
    ((_ (pass-if-equal test-name val exp))
     (begin (pass-if-equal (string-append test-name " (eval)") val
              (primitive-eval 'exp))
            (pass-if-equal (string-append test-name " (compile)") val
              (compile 'exp #:to 'value #:env (current-module)))))
    ((_ (pass-if-exception test-name exc exp))
     (begin (pass-if-exception (string-append test-name " (eval)")
                               exc (primitive-eval 'exp))
            (pass-if-exception (string-append test-name " (compile)")
                               exc (compile 'exp #:to 'value
                                            #:env (current-module)))))))

;;; (with-test-prefix/c&e PREFIX BODY ...)
;;; Same as `with-test-prefix', but the enclosed tests are run both with
;;; the compiler/VM and the evaluator.
(define-syntax with-test-prefix/c&e
  (syntax-rules ()
    ((_ section-name exp ...)
     (with-test-prefix section-name (c&e exp) ...))))

;;; Call THUNK using the debugging evaluator.
(define (with-debugging-evaluator* thunk)
  (let ((dopts #f))
    (dynamic-wind
	(lambda ()
	  (set! dopts (debug-options)))
	thunk
	(lambda ()
	  (debug-options dopts)))))

;;; Evaluate BODY... using the debugging evaluator.
(define-macro (with-debugging-evaluator . body)
  `(with-debugging-evaluator* (lambda () ,@body)))

;; Recurse through a C function that should clear any values that might
;; have spilled on the stack temporarily.  (The salient feature of
;; with-continuation-barrier is that currently it is implemented as a C
;; function that recursively calls the VM.)
;;
(define* (clear-stale-stack-references #:optional (n 10))
  (if (positive? n)
      (with-continuation-barrier
       (lambda ()
         (clear-stale-stack-references (1- n))))))

;;; Call THUNK with a given locale
(define (with-locale* nloc thunk)
  (let ((loc #f))
    (dynamic-wind
	(lambda ()
          (if (defined? 'setlocale)
              (begin
                (set! loc (false-if-exception (setlocale LC_ALL)))
                (if (or (not loc)
                        (not (false-if-exception (setlocale LC_ALL nloc))))
                    (throw 'unresolved)))
              (throw 'unresolved)))
	thunk
	(lambda ()
          (if (and (defined? 'setlocale) loc)
              (setlocale LC_ALL loc))))))

;;; Evaluate BODY... using the given locale.
(define-syntax with-locale
  (syntax-rules ()
    ((_ loc body ...)
     (with-locale* loc (lambda () body ...)))))

;;; Try out several ISO-8859-1 locales and run THUNK under the one that works
;;; (if any).
(define (with-latin1-locale* thunk)
  (define %locales
    (append-map (lambda (name)
                  (list (string-append name ".ISO-8859-1")
                        (string-append name ".iso88591")
                        (string-append name ".ISO8859-1")))
                '("ca_ES" "da_DK" "de_DE" "es_ES" "es_MX" "en_GB" "en_US"
                  "fr_FR" "pt_PT" "nl_NL" "sv_SE")))

  (let loop ((locales %locales))
    (if (null? locales)
        (throw 'unresolved)
        (catch 'unresolved
          (lambda ()
            (with-locale* (car locales) thunk))
          (lambda (key . args)
            (loop (cdr locales)))))))

;;; Evaluate BODY... using an ISO-8859-1 locale or throw `unresolved' if none
;;; was found.
(define-syntax with-latin1-locale
  (syntax-rules ()
    ((_ body ...)
     (with-latin1-locale* (lambda () body ...)))))

(define %null-device
  ;; On Windows (MinGW), /dev/null does not exist and we must instead
  ;; use NUL.  Note that file system procedures automatically translate
  ;; /dev/null, so this variable is only useful for shell snippets.

  ;; Test for Windowsness by checking whether the current directory name
  ;; starts with a drive letter.
  (if (string-match "^[a-zA-Z]:[/\\]" (getcwd))
      "NUL"
      "/dev/null"))


;;;; REPORTERS
;;;;

;;; The global list of reporters.
(define reporters '())

;;; The default reporter, to be used only if no others exist.
(define default-reporter #f)

;;; Add the procedure REPORTER to the current set of reporter functions.
;;; Signal an error if that reporter procedure object is already registered.
(define (register-reporter reporter)
  (if (memq reporter reporters)
      (error "register-reporter: reporter already registered: " reporter))
  (set! reporters (cons reporter reporters)))

;;; Remove the procedure REPORTER from the current set of reporter
;;; functions.  Signal an error if REPORTER is not currently registered.
(define (unregister-reporter reporter)
  (if (memq reporter reporters)
      (set! reporters (delq! reporter reporters))
      (error "unregister-reporter: reporter not registered: " reporter)))

;;; Return true iff REPORTER is in the current set of reporter functions.
(define (reporter-registered? reporter)
  (if (memq reporter reporters) #t #f))

;;; Send RESULT to all currently registered reporter functions.
(define (report . args)
  (if (pair? reporters)
      (for-each (lambda (reporter) (apply reporter args))
		reporters)
      (apply default-reporter args)))


;;;; Some useful standard reporters:
;;;; Count reporters count the occurrence of each test result type.
;;;; Log reporters write all test results to a given log file.
;;;; Full reporters write all test results to the standard output.
;;;; User reporters write interesting test results to the standard output.

;;; The complete list of possible test results.
(define result-tags
  '((pass        "PASS"        "passes:                 ")
    (fail        "FAIL"        "failures:               ")
    (upass       "UPASS"       "unexpected passes:      ")
    (xfail       "XFAIL"       "expected failures:      ")
    (unresolved  "UNRESOLVED"  "unresolved test cases:  ")
    (untested    "UNTESTED"    "untested test cases:    ")
    (unsupported "UNSUPPORTED" "unsupported test cases: ")
    (error       "ERROR"       "errors:                 ")))

;;; The list of important test results.
(define important-result-tags
  '(fail upass unresolved error))

;;; Display a single test result in formatted form to the given port
(define (print-result port result name . args)
  (let* ((tag (assq result result-tags))
	 (label (if tag (cadr tag) #f)))
    (if label
	(begin
	  (display label port)
	  (display ": " port)
	  (display (format-test-name name) port)
	  (if (pair? args)
	      (begin
		(display " - arguments: " port)
		(write args port)))
	  (newline port))
	(error "(test-suite lib) FULL-REPORTER: unrecognized result: "
	       result))))

;;; Return a list of the form (COUNTER RESULTS), where:
;;; - COUNTER is a reporter procedure, and
;;; - RESULTS is a procedure taking no arguments which returns the
;;;   results seen so far by COUNTER.  The return value is an alist
;;;   mapping outcome symbols (`pass', `fail', etc.) onto counts.
(define (make-count-reporter)
  (let ((counts (map (lambda (tag) (cons (car tag) 0)) result-tags)))
    (list
     (lambda (result name . args)
       (let ((pair (assq result counts)))
	 (if pair
	     (set-cdr! pair (+ 1 (cdr pair)))
	     (error "count-reporter: unexpected test result: "
		    (cons result (cons name args))))))
     (lambda ()
       (append counts '())))))

;;; Print a count reporter's results nicely.  Pass this function the value
;;; returned by a count reporter's RESULTS procedure.
(define (print-counts results . port?)
  (let ((port (if (pair? port?)
		  (car port?)
		  (current-output-port))))
    (newline port)
    (display-line-port port "Totals for this test run:")
    (for-each
     (lambda (tag)
       (let ((result (assq (car tag) results)))
	 (if result
	     (display-line-port port (caddr tag) (cdr result))
	     (display-line-port port
				"Test suite bug: "
				"no total available for `" (car tag) "'"))))
     result-tags)
    (newline port)))

;;; Return a reporter procedure which prints all results to the file
;;; FILE, in human-readable form.  FILE may be a filename, or a port.
(define (make-log-reporter file)
  (let ((port (if (output-port? file) file
		  (open-output-file file))))
    (lambda args
      (apply print-result port args)
      (force-output port))))

;;; A reporter that reports all results to the user.
(define (full-reporter . args)
  (apply print-result (current-output-port) args))

;;; A reporter procedure which shows interesting results (failures,
;;; unexpected passes etc.) to the user.
(define (user-reporter result name . args)
  (if (memq result important-result-tags)
      (apply full-reporter result name args)))

(set! default-reporter full-reporter)
