;;;  demyltify.lisp --- Milter Protocol library

;;;  Copyright (C) 2004, 2006, 2007 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: demyltify

#+cmu (ext:file-comment "$Module: demyltify.lisp, Time-stamp: <2007-07-27 18:50:30 wcp> $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

;;;  Commentary:
;;;
;;; Building blocks to make your own milter in Common Lisp.
;;;
;;; BEWARE: This work is based on an informal description of the
;;; undocumented Sendmail-milter protocol.  This code may therefore be
;;; outdated right now, as the Sendmail folks don't want you to mess
;;; with their protocol, but rather want you to use their
;;; pthread-based libmilter library in C.  Although, in practice, it's
;;; unlikely that this code will be invalidated by the next few
;;; Sendmail versions, you never know.
;;;
;;; To use this library, all you have to do is:
;;;
;;;   - write your own context class inheriting from MILTER-CONTEXT as
;;;     it's unlikely that the default class carries enough state for
;;;     your application
;;;
;;;   - specialise the HANDLE-EVENT methods on all the events you care
;;;     about (the default definition will simply let any mail get
;;;     through)
;;;
;;;   - call START-MILTER with the socket port you intend to use; this
;;;     can be a number, a name (to be found in /etc/services) or a
;;;     pathname for a Unix domain (local) socket.  You should also
;;;     pass the required events you want to be notified about and the
;;;     actions your milter intends to perform during its life.  (This
;;;     is a Sendmail's idiosyncrasy; you can't decide what to do on
;;;     the spur of the moment, you have to let Sendmail know in
;;;     advance.)
;;;
;;; The default options negotiation procedure will signal an error
;;; condition if the MTA doesn't fully support the milter
;;; prerequisites.  This is a sensible behaviour considering that, if
;;; the MTA doesn't match the performed actions and required events of
;;; the milter, there is very little the milter can do about it; it
;;; will simply not work.  In the exceptional case your milter is
;;; smart enough to be able to fall back to a different behaviour, if
;;; something is not supported by the MTA, then you should redefine
;;; the HANDLE-EVENT method specialised on EVENT-OPTIONS.
;;;
;;; This library is mostly stateless, so the programme, if it needs
;;; to, is responsible to save its state in the context object
;;; sometime in a HANDLE-EVENT method.  To do that you are supposed to
;;; write your own context class which inherits from MILTER-CONTEXT
;;; and pass it to START-MILTER.
;;;
;;; START-MILTER is a procedure that never exits under normal
;;; circumstances.  It enters a loop serving MTA connections on the
;;; specified socket.  The default behaviour is to serve a connection
;;; a time which is an acceptable behaviour on small (personal)
;;; systems.  If you are running a server, though, this might not be
;;; acceptable; you want to specialise the HANDLE-EVENT method for the
;;; :CONNECTION event and do the required forking/thread-firing.
;;;
;;; In case you don't know, to install a milter in Sendmail you have
;;; to add a line like this
;;;
;;; INPUT_MAIL_FILTER(`filter1', `S=unix:/var/run/demyltify.socket, F=T')
;;;
;;; at the end of your /etc/mail/sendmail.mc (or whatever mc file name
;;; you happen to use) and compile the corresponding cf file:
;;;
;;;   cd /etc/mail; make sendmail.cf
;;;
;;; Then make sure you use the same socket pathname in the call of
;;; START-MILTER:
;;;
;;;   (start-milter #P"/var/run/demyltify.socket")
;;;
;;; So it doesn't need to be named demyltify.socket as long as you are
;;; consistent.  It could be a port as well.  For instance, in a
;;; scenario of a dedicated milter machine (big-thought) that serves
;;; multiple sendmail clients, every client machine must have:
;;;
;;;   INPUT_MAIL_FILTER(`filter1', `S=inet:4242@big-thought, F=T')
;;;
;;; and on big-thought server:
;;;
;;;   (start-milter 4242 :split :fork)
;;;
;;; The F=T flag tells Sendmail to treat milter-related errors (ie
;;; milter not listening or crashing) as temporary and therefore
;;; retriable.  Read the Sendmail's cf/README file if you need further
;;; details.
;;;
;;; Specialisation of the HANDLE-EVENT methods must return a
;;; MILTER-ACTION object which will be sent to the MTA.  Some symbol
;;; macros have been defined to simplify the syntax of the trivial
;;; cases.  Those are: KEEP-GOING (continue with the next event), ACCEPT
;;; (accept the message), REJECT (bounce the message), DISCARD
;;; (silently ignore the message), PROGRESS (hang on, the milter is
;;; performing some lengthy computation), TEMPORARY-FAILURE (the
;;; message can't be processed by the milter because of a temporary
;;; problem).
;;;
;;; See the bottom of this file for a trivial usage example.
;;;
;;; This code has been tested on SBCL, CMUCL and CLISP.  Porting to
;;; other Lisp systems should be fairly easy.
;;;
;;; Credit should be given to Todd Vierling (tv@pobox.com tv@duh.org)
;;; for documenting the MTA/milter protocol and writing the first
;;; implementation in Perl.
;;;

#-(or sbcl cmu clisp)
(warn "This code hasn't been tested on your Lisp system.")

(in-package :cl-user)

(defpackage :demyltify
  (:nicknames :milter)
  (:use :common-lisp :net4cl)
  (:export #:start-milter
	   #:handle-event
	   #:send-action
	   #:milter-context
	   #:action
	   #:get-macro
	   #:dprint
	   ;; symbol macros for simple actions
	   #:keep-going
	   #:accept
	   #:reject
	   #:discard
	   #:progress
	   #:temporary-failure
	   #:no-action
	   ;; the MTA event classes
	   #:mta-event
	   #:event-abort
	   #:event-body
	   #:event-connect
	   #:event-define-macro
	   #:event-data
	   #:event-disconnect
	   #:event-end-of-headers
	   #:event-end-of-message
	   #:event-header
	   #:event-hello
	   #:event-mail
	   #:event-options
	   #:event-quit
	   #:event-recipient
	   #:event-unknown
	   ;; the milter actions classes
	   #:milter-action
	   #:action-accept
	   #:action-add-header
	   #:action-add-recipient
	   #:action-change-header
	   #:action-change-sender
	   #:action-continue
	   #:action-delete-recipient
	   #:action-discard
	   #:action-options
	   #:action-progress
	   #:action-quarantine
	   #:action-reject
	   #:action-replace-body
	   #:action-reply-code
	   #:action-skip
	   #:action-temporary-failure
	   ;; the event slot readers
	   #:event-body-data
	   #:event-conn-host-name
	   #:event-conn-family
	   #:event-conn-port
	   #:event-conn-address
	   #:event-macro-command
	   #:event-macro-definitions
	   #:event-hello-greeting
	   #:event-header-name
	   #:event-header-value
	   #:event-mail-sender
	   #:event-mail-options
	   #:event-options-version
	   #:event-options-actions
	   #:event-options-protocol
	   #:event-recipient-address
	   #:event-recipient-options
	   #:event-unknown-command
	   ;; the global variables
	   #:*log-features*
	   #:*log-file*
	   #:*max-log-size*
	   #:*default-events*
	   #:*default-actions*))

(in-package :demyltify)

(defconstant +max-body-chunk+ 65535
  "Maximum size of a replace-body message to the MTA.")

(defconstant +protocol-version+ 6
  "Protocol version number spoken by this library.")

(defconstant +minimum-protocol-version+ 2
  "Minimum protocol version accepted by this library.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGURABLE OPTIONS

(defvar *log-file* nil
  "Can be a pathname, a stram, T for standard output, or NIL for
no logging at all.  Example: #P\"/var/log/demyltify.log\".")

(defvar *max-log-size* nil
  "Maximum size of the logfile.  If NIL the log file is allowed
to grow indefinitely.")

(defvar *default-events* '(:mail :recipient)
  "List of default events required from Sendmail if not explicitly
specified in the creation of the context object.  They are among the
following:

  :CONNECT
  :HELLO
  :MAIL
  :RECIPIENT
  :BODY
  :HEADER
  :END-OF-HEADERS

These are only the optional events.  Other events, such as the
end-of-message event, are sent by default to the milter, as
without them it would make little sense to run a milter at all.

Refrain from specifying everything, if you don't need to, as it
would seriously impact Sendmail and the milter performance.
Especially the :BODY events are heavy on the wire.")

(defvar *default-actions* '()
  "List of actions this milter may carry on a message.  This
doesn't mean it /will/, but simply it /could/.  It doesn't even
mean you are allowed to; that should be negotiated with the MTA.
They are among the following:

  :ADD-HEADER
  :CHANGE-BODY
  :ADD-RECIPIENT
  :DELETE-RECIPIENT
  :CHANGE-HEADER
  :QUARANTINE

Compile this list in accordance to how you'll write the
HANDLE-EVENT method for EVENT-END-OF-MESSAGE.  That's where you
are allowed to do those operations above.")

(defvar *log-features* '(:error)
  "Features to log.  This are the keywords used with DPRINT.  If
this is T, we log everything.")

;;; End of configurable options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-size (pathname)
  (with-open-file (stream pathname)
    (file-length stream)))

(defun dprint (debug-feature fmt &rest args)
  "Output formatted message to *LOG-FILE* if DEBUG-FEATURE is among
the selected ones in *LOG-FEATURES*.  The log file is opened and
closed at each message."
  (labels ((time-tag (out)
	     (multiple-value-bind (ss mm hh day month year week-day dst tz) (get-decoded-time)
	       (declare (ignore year week-day dst tz))
	       (format out "~A ~2D ~2D:~2,'0D:~2,'0D " (subseq (month->string month) 0 3)
		       day hh mm ss)))
	   (log-to-stream (out)
	     (time-tag out)
	     (apply #'format out fmt args)
	     (terpri out)
	     (finish-output out)))
    (when (and *log-file*
	       (or (eq *log-features* t)
		   (member debug-feature *log-features*)))
      (if (pathnamep *log-file*)
	  (with-open-file (out *log-file*
			   :direction :output
			   :if-exists (if (and *max-log-size*
					       (> (file-size *log-file*) *max-log-size*))
					  :rename
					  :append)
			   :if-does-not-exist :create)
	    (log-to-stream out))
	  (log-to-stream *log-file*)))))


(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASSES

(defclass milter-context ()
  ((socket :initarg :socket
	   :reader ctx-socket)
   (macros :initform '()
	   ;; emptied at the beginning of each message
	   :accessor ctx-macros
	   :documentation
	   "Alist of alist of macros.  The primary key is the
event type, the secondary key is the macro name.")
   (events :type list
	   :initform *default-events*
	   :initarg :events
	   :documentation
	   "List of events the milter is expecting from Sendmail.")
   (actions :type list
	    :initform *default-actions*
	    :initarg :actions
	    :documentation
	    "List of actions the milter is going to perform."))
  (:documentation
   "Base class for milter contexts.  Programmes (milter
implementations) must define their own contexts inheriting from
this."))

(defclass mta-event ()
  ())

(defclass event-abort (mta-event)
  ()
  (:documentation
   "The MTA asks to abort any further processing of the current
message.  More messages may follow."))

(defclass event-body (mta-event)
  ((data :initarg :data
	 :type (vector (unsigned-byte 8))
	 :reader event-body-data))
  (:documentation
   "This constitutes a chunk (possibly all) of a message body."))

(defclass event-connect (mta-event)
  ((host-name :initarg :host-name
	      :type string
	      :reader event-conn-host-name)
   (family :initarg :family
	   :type character
	   :reader event-conn-family)
   (port :initarg :port
	 :type integer
	 :reader event-conn-port)
   (address :initarg :address
	    :type string
	    :reader event-conn-address)))

(defclass event-data (mta-event)
  ()
  (:documentation
   "This marks the beginning of the message body.  See EVENT-BODY."))

(defclass event-define-macro (mta-event)
  ((command :initarg :command
	    :type character
	    :reader event-macro-command)
   (definitions :initarg :definitions
     :type list
     :reader event-macro-definitions)))

(defclass event-end-of-message (mta-event)
  ()
  (:documentation
   "This marks the end of the message body."))

(defclass event-hello (mta-event)
  ((greeting :initarg :greeting
	     :type string
	     :reader event-hello-greeting)))

(defclass event-header (mta-event)
  ((name :initarg :name
	 :type string
	 :reader event-header-name)
   (value :initarg :value
	  :type string
	  :reader event-header-value)))

(defclass event-mail (mta-event)
  ((sender :initarg :sender
	   :type string
	   :reader event-mail-sender)
   (options :initarg :options
	    :type list
	    :reader event-mail-options)))

(defclass event-end-of-headers (mta-event)
  ())

(defclass event-options (mta-event)
  ((version :initarg :version
	    :type integer
	    :reader event-options-version)
   (actions :initarg :actions
	    :type integer
	    :reader event-options-actions)
   (protocol-mask :initarg :protocol-mask
		  :type integer
		  :reader event-options-protocol-mask)))

(defclass event-recipient (mta-event)
  ((address :initarg :address
	    :type string
	    :reader event-recipient-address)
   (options :initarg :options
	    :type list
	    :reader event-recipient-options)))

(defclass event-quit (mta-event)
  ()
  (:documentation
   "The MTA wants this milter to stop running.  By default it simply
closes the socket connection but a programme can decide to actually
stop running."))

(defclass event-disconnect (mta-event)
  ()
  (:documentation
   "Similar to EVENT-QUIT, but the milter programme should expect
further connections."))

(defclass event-unknown (mta-event)
  ((command :initarg :command
	    :type string
	    :reader event-unknown-command))
  (:documentation
   "Unknown or invalid SMTP command from client.  The default
behaviour is to ignore it."))

(defclass milter-action ()
  ())

(defclass action-add-recipient (milter-action)
  ((address :initarg :address
	    :type string)
   (parameters :initarg :parameters
	       :type (or string null)
	       :initform nil
	       :documentation "Optional ESMTP parameters.")))

(defclass action-delete-recipient (milter-action)
  ((address :initarg :address
	    :type string)))

(defclass action-accept (milter-action)
  ())

(defclass action-replace-body (milter-action)
  ((body :initarg :body
	 :type (vector (unsigned-byte 8)))))

(defclass action-continue (milter-action)
  ())

(defclass action-no-action (milter-action)
  ())

(defclass action-discard (milter-action)
  ())

(defclass action-change-sender (milter-action)
  ((address :initarg :address
	    :type string)
   (parameters :initarg :parameters
	       :type (or string null)
	       :initform nil
	       :documentation "Optional ESMTP parameters."))
  (:documentation
   "Change the envelope sender (from)."))

(defclass action-add-header (milter-action)
  ((name :initarg :name
	 :type string)
   (value :initarg :value
	  :type string)
   (position :initarg :position
	     :type (or number null)
	     :initform nil
	     :documentation
	     "Optional position in the headers list: zero is the topmost.")))

(defclass action-change-header (milter-action)
  ((index :initarg :index
	  :type integer)
   (name :initarg :name
	 :type string)
   (value :initarg :value
	  :type (or string null))))

(defclass action-progress (milter-action)
  ())

(defclass action-quarantine (milter-action)
  ((reason :initarg :reason
	   :type string)))

(defclass action-reject (milter-action)
  ())

(defclass action-temporary-failure (milter-action)
  ())

(defclass action-reply-code (milter-action)
  ((smtp-code :initarg :smtp-code
	      :type integer)
   (text :initarg :text
	 :type string)))

(defclass action-options (milter-action)
  ((version :initarg :version
	    :type integer)
   (actions :initarg :actions
	    :type integer)
   (protocol-mask :initarg :protocol-mask
	     :type integer)))

(defclass action-skip (milter-action)
  ()
  (:documentation
   "Skip further callbacks of the same type in this transaction.
Useful after an EVENT-BODY."))

(defgeneric send-action (action stream)
  (:documentation
   "Write on STREAM a byte sequence (a packet) representing the
action."))

(defgeneric handle-event (event context)
  (:documentation
   "Handle an MTA EVENT in that CONTEXT."))

;;; End of CLASSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some CONDITIONS

(define-condition milter-condition ()
  ())

(defun print-broken-communication-condition (condition stream)
  (with-slots (reason) condition
    (format stream "communication error ~A" reason)))

(define-condition milter-broken-communication (milter-condition)
  ((reason :initarg :reason))
  (:report print-broken-communication-condition))

(define-condition options-negotiation-error (milter-condition)
  ())

(defun print-actions-not-allowed-condition (condition stream)
  (with-slots (forbidden-actions) condition
    (format stream "MTA doesn't allow some milter actions ~S" forbidden-actions)))

(define-condition actions-not-allowed (options-negotiation-error)
  ((forbidden-actions :initarg :forbidden-actions))
  (:report print-actions-not-allowed-condition))

(defun print-events-not-reported-condition (condition stream)
  (with-slots (unavailable-events) condition
    (format stream "Milter requires unreported events ~S" unavailable-events)))

(define-condition events-not-reported (options-negotiation-error)
  ((unavailable-events :initarg :unavailable-events))
  (:report print-events-not-reported-condition))

(defun print-wrong-protocol-version-condition (condition stream)
  (with-slots (mta-version milter-version) condition
    (format stream "Protocol version mismatch: MTA=~A milter=~A" mta-version milter-version)))

(define-condition wrong-protocol-version (options-negotiation-error)
  ((mta-version :initarg :mta-version)
   (milter-version :initarg :milter-version))
  (:report print-wrong-protocol-version-condition))

;;; End of conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro action (context type &rest args)
  "Syntactic sugar handy at the end of message to send message
modifying actions to the MTA."
  `(send-action (make-instance ',type ,@args) ,context))

;;
;; Some shortcuts for the common return codes
;;
(define-symbol-macro keep-going (make-instance 'action-continue))
(define-symbol-macro accept (make-instance 'action-accept))
(define-symbol-macro reject (make-instance 'action-reject))
(define-symbol-macro discard (make-instance 'action-discard))
(define-symbol-macro progress (make-instance 'action-progress))
(define-symbol-macro no-action (make-instance 'action-no-action))
(define-symbol-macro temporary-failure (make-instance 'action-temporary-failure))
(define-symbol-macro skip (make-instance 'action-skip))


(defun month->string (month)
  "Return the month string corresponding to MONTH number."
  (elt #("January"
	 "February"
	 "March"
	 "April"
	 "May"
	 "June"
	 "July"
	 "August"
	 "September"
	 "October"
	 "November"
	 "December") (1- month)))

(defun position-any (bag sequence &rest position-args)
  "Find any element of bag in sequence and return its position.
Accept any argument accepted by the POSITION function."
  (apply #'position-if #'(lambda (element)
			   (find element bag)) sequence position-args))

(defun find-any (bag sequence &rest find-args)
  "Find any element of bag in sequence.  Accept any argument
accepted by the FIND function."
  (apply #'find-if #'(lambda (element)
			   (find element bag)) sequence find-args))

(defun split-at (bag sequence)
  "Split SEQUENCE at occurrence of any element from BAG.
Contiguous occurrences of elements from BAG are considered atomic;
so no empty string is returned."
  (let ((len (length sequence)))
    (labels ((split-from (start)
	       (unless (>= start len)
		 (let ((sep (position-any bag sequence :start start)))
		   (cond ((not sep)
			  (list (subseq sequence start)))
			 ((> sep start)
			  (cons (subseq sequence start sep)
				(split-from (1+ sep))))
			 (:else
			  (split-from (1+ start))))))))
      (split-from 0))))

(defun string->integer (string)
  "Convert a string into an integer.  If string doesn't represent
a proper integer, the result is undefined."
  (loop
     with value = 0
     for c across string
     do (setf value (+ (* value 10) (- (char-code c) (char-code #\0))))
     finally (return value)))

(defun split-string-at-char (string separator &key escape skip-empty)
  "Split STRING at SEPARATORs and return a list of the substrings.  If
SKIP-EMPTY is true then filter out the empty substrings.  If ESCAPE is
not nil then split at SEPARATOR only if it's not preceded by ESCAPE."
  (declare (type string string) (type character separator))
  (labels ((next-separator (beg)
             (let ((pos (position separator string :start beg)))
               (if (and escape
                        pos
                        (plusp pos)
                        (char= escape (char string (1- pos))))
                   (next-separator (1+ pos))
                   pos)))
           (parse (beg)
             (cond ((< beg (length string))
                    (let* ((end (next-separator beg))
                           (substring (subseq string beg end)))
                      (cond ((and skip-empty (string= "" substring))
                             (parse (1+ end)))
                            ((not end)
                             (list substring))
                            (:else
			     (cons substring (parse (1+ end)))))))
                   (skip-empty
		    '())
                   (:else
		    (list "")))))
    (parse 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline decode-int16 decode-int32 encode-int32))

(defun decode-int16 (buf)
  "Decode the 2 byte sequence BUF as a 16 bit integer in network
byte order."
  (declare (type (vector (unsigned-byte 8))))
  (+ (ash (elt buf 0) 8)
     (elt buf 1)))

(defun decode-int32 (buf)
  "Decode the 4 byte sequence BUF as a 32 bit integer in network
byte order."
  (declare (type (vector (unsigned-byte 8))))
  (+ (ash (elt buf 0) 24)
     (ash (elt buf 1) 16)
     (ash (elt buf 2) 8)
     (elt buf 3)))

(defun encode-int32 (value)
  "Code VALUE integer in a sequence of 4 bytes in network byte
order."
  (declare (type integer value))
  (let ((buf (make-sequence '(vector (unsigned-byte 8)) 4)))
    (macrolet ((set-byte (n)
		 `(setf (elt buf ,n)
			(logand #xff (ash value ,(* 8 (- n 3)))))))
      (set-byte 0)
      (set-byte 1)
      (set-byte 2)
      (set-byte 3))
    buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun receive-byte (stream)
  "Read a byte from STREAM.  Signal MILTER-BROKEN-COMMUNICATION
if it can't."
  (handler-case
      (let ((byte (read-byte stream)))
	(dprint :bytes ">>> ~A ~S" byte (code-char byte))
	byte)
    (t (c)
      (error 'milter-broken-communication :reason c))))

(defun receive-sequence (expected-length stream)
  "Read a sequence of bytes of EXPECTED-LENGTH length from
STREAM.  Signal an error if it can't read enough bytes."
  (loop
     with buffer = (make-sequence '(vector (unsigned-byte 8)) expected-length)
     for i from 0 to (1- expected-length) by 1
     do (setf (elt buffer i) (receive-byte stream))
     finally (return buffer)))

(defun receive-int32 (stream)
  "Read a 32bit integer in network byte order (big-endian) from STREAM."
  (decode-int32 (receive-sequence 4 stream)))

(defun receive-packet (stream)
  "Receive a protocol packet from the MTA through STREAM.  Return
two values the command (a character) and its data (a byte
sequence)."
  (let* ((len (1- (receive-int32 stream)))
	 (command (code-char (receive-byte stream)))
	 (data (receive-sequence len stream)))
    (dprint :packet "RECEIVED command ~S and ~A byte~:P of data" command len)
    (values command data)))

(defun decode-packet-data (data format-description)
  "Decode the packet DATA according to its FORMAT-DESCRIPTION,
splitting it in its subarts (fields).  FORMAT-DESCRIPTION is a
list of keywords among:

  :CHAR		a character
  :C-STRING	a NULL terminated string (the null is stripped)
  :C-STRINGS	a sequence of :C-STRING
  :INT16	a two byte integer in NBO
  :INT32	a four byte integer in NBO

If data is not laid according to FORMAT-DESCRIPTION the result is
unknown."
  (loop
     with position = 0
     for element in format-description
     collect (case element
	       (:char
		(prog1 (code-char (elt data position))
		  (incf position)))
	       (:c-string
		(let ((null-pos (position 0 data :start position)))
		  (prog1 (map 'string #'code-char (subseq data position null-pos))
		    (setf position (1+ null-pos)))))
	       (:c-strings
		(loop
		   while (< position (length data))
		   collect (let ((null-pos (position 0 data :start position)))
			     (prog1 (map 'string #'code-char (subseq data position null-pos))
			       (setf position (1+ null-pos))))))
	       (:int16
		(prog1 (decode-int16 (subseq data position (+ 2 position)))
		  (incf position 2)))
	       (:int32
		(prog1 (decode-int32 (subseq data position (+ 4 position)))
		  (incf position 4))))))

(defmacro with-packet-data (data fields &body body)
  "Execute BODY within a lexical context that binds FIELDS to the
exploded DATA packet.  DATA is a byte sequence and FIELDS is a
list of pairs; each pair is a variable name and a type specifier.
See DECODE-PACKET-DATA for further details about type
specifiers."
  `(destructuring-bind ,(mapcar #'car fields) (decode-packet-data ,data ',(mapcar #'cadr fields))
     ,@body))

(defun receive-event (stream)
  "Receive an MTA event from STREAM.  Return the corresponding
MTA-EVENT object."
  (multiple-value-bind (command data) (receive-packet stream)
    (ecase command
      (#\A (make-instance 'event-abort))
      (#\B (make-instance 'event-body :data data))
      (#\C
       (with-packet-data data
	 ((host-name :c-string) (family :char) (port :int16) (address :c-string))
	 (make-instance 'event-connect :host-name host-name :family family :port port :address address)))
      (#\D
       (with-packet-data data
	 ((command :char) (namevals :c-strings))
	 (make-instance 'event-define-macro :command command
			:definitions (flet ((strip-brackets (string)
					      (if (and (> (length string) 2)
						       (char= (elt string 0) #\{))
						  (subseq string 1 (1- (length string)))
						  string)))
				       (loop
					  for name = (pop namevals)
					  for value = (pop namevals)
					  while namevals
					  collect (cons (strip-brackets name) value))))))
      (#\E
       (make-instance 'event-end-of-message))
      (#\H
       (make-instance 'event-hello :greeting (car (decode-packet-data data '(:c-string)))))
      (#\K
       (make-instance 'event-disconnect))
      (#\L
       (with-packet-data data
	 ((name :c-string) (value :c-string))
	 (make-instance 'event-header
			:name name
			:value value)))
      (#\M
       (with-packet-data data
	 ((sender :c-string) (options :c-strings))
	 (make-instance 'event-mail :sender sender :options options)))
      (#\N
       (make-instance 'event-end-of-headers))
      (#\O
       (with-packet-data data
	 ((version :int32) (actions :int32) (protocol-mask :int32))
	 (make-instance 'event-options :version version :actions actions :protocol-mask protocol-mask)))
      (#\Q
       (make-instance 'event-quit))
      (#\R
       (with-packet-data data
	 ((address :c-string) (options :c-strings))
	 (make-instance 'event-recipient :address address :options options)))
      (#\T
       (make-instance 'event-data))
      (#\U
       (make-instance 'event-unknown :command (car (decode-packet-data data '(:c-string))))))))

(defconstant +action-masks-alist+
  '((:add-header	#x01)
    (:change-body	#x02)
    (:add-recipient	#x04)
    (:delete-recipient	#x08)
    (:change-header	#x10)
    (:quarantine	#x20)
    (:change-sender	#x40)
    (:add-recipient-par	#x80))
  "Alist of action bitmasks lifted from <libmilter/mfdef.h>.")

(defconstant +event-masks-alist+
  '((:connect			#x0001)
    (:hello			#x0002)
    (:mail			#x0004)
    (:recipient			#x0008)
    (:body			#x0010)
    (:header			#x0020)
    (:end-of-headers		#x0040)
    (:reply-headers		#x0080)
    (:unknown			#x0100)
    (:data			#x0200)
    (:skip			#x0400)
    (:rejected-to		#x0800))
  "Alist of event bitmasks lifted from <libmilter/mfdef.h>.")

(defconstant +all-events-mask+
  (reduce #'logior +event-masks-alist+ :key #'cadr))

(defun action-mask (action)
  "Lookup the action bitmask corresponding to ACTION keyword."
  (cadr (assoc action +action-masks-alist+)))

(defun event-mask (event)
  "Lookup the event bitmask corresponding to ACTION keyword."
  (cadr (assoc event +event-masks-alist+)))

(defun actions-mask (actions)
  "Compose a bitmask corresponding to the list of ACTIONS."
  (loop
     with mask = 0
     for action in actions
     do (setf mask (logior mask (action-mask action)))
     finally (return mask)))

(defun protocol-events-mask (events)
  "Compose the bitmask corresponding to the list of protocol
EVENTS."
  (loop
     with mask = 0
     for event in events
     do (setf mask (logior mask (event-mask event)))
     finally (return mask)))

(defun list-from-mask (mask table)
  "Return the list of keywords taken from TABLE corresponding to
the bitmask MASK."
  (loop
     for (symbol byte) in table
     unless (zerop (logand byte mask))
     collect symbol))

(defun events-list-from-mask (mask)
  "Return the list of event keywords corresponding to the bitmask
MASK."
  (list-from-mask mask +event-masks-alist+))

(defun actions-list-from-mask (mask)
  "Return the list of action keywords corresponding to the
bitmask MASK."
  (list-from-mask mask +action-masks-alist+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fallback handlers to guarantee a consistent default behaviour

(defmethod handle-event ((event mta-event) (ctx milter-context))
  "Fallback method.  It always sends a CONTINUE message back to
the MTA."
  (declare (ignore ctx))
  keep-going)

;; This simple function will take care of the necessary bookkeeping
;; without writing all that baroque infrastructure found inside
;; libmilter.  This works because macros are notified to the milter
;; just before the command they belong to.  The validity of those
;; macros spans until the next same command or a prerequisite command.
(defmethod handle-event ((event event-define-macro) (ctx milter-context))
  ;; Store macros in the context.
  (let* ((command (event-macro-command event))
	 (definitions (event-macro-definitions event))
	 (macros (member command (ctx-macros ctx) :key #'car)))
    (when macros
      ;; get rid of the old macros
      (setf (ctx-macros ctx) (cdr macros)))
    (push (cons command definitions) (ctx-macros ctx)))
  no-action)

(defun get-macro (ctx name)
  (declare (type milter-context ctx))
  (labels ((find-macro (macros)
	     (when macros
	     (or (cdr (assoc name (cdar macros) :test #'string-equal))
		 (find-macro (cdr macros))))))
    (find-macro (ctx-macros ctx))))

;; This method shouldn't be changed by the milter implementation as it
;; does a fairly sensible task at negotiating the connection options.
;; It checks that the milter logic won't break if the MTA doesn't
;; fully support the milter.
(defmethod handle-event ((event event-options) (ctx milter-context))
  ;; to reply the headers is the default behaviour of this milter so
  ;; we keep the protocol the way it is
  (let* ((required-events-mask (protocol-events-mask
				(cons :reply-headers
				      *default-events*)))
	 (performed-actions-mask (actions-mask *default-actions*))
	 (mta-provided-events (event-options-protocol-mask event))
	 (mta-allowed-actions (event-options-actions event))
	 (common-events-mask (logand required-events-mask mta-provided-events))
	 (common-actions-mask (logand performed-actions-mask mta-allowed-actions)))
    (dprint :options "~
milter-required-events=	~32,'0,'.,8:B~%~
mta-provided-events=	~32,'0,'.,8:B~%~
common-mask=		~32,'0,'.,8:B~2%~
milter-actions=		~32,'0,'.,8:B~%~
mta-allowed-actions=	~32,'0,'.,8:B~%~
common-mask=		~32,'0,'.,8:B~%"
	    required-events-mask
	    mta-provided-events
	    common-events-mask
	    performed-actions-mask
	    mta-allowed-actions
	    common-actions-mask)
    (when (< (event-options-version event) +minimum-protocol-version+)
      (error 'wrong-protocol-version
	     :mta-version (event-options-version event)
	     :milter-version +protocol-version+))
    (unless (= common-events-mask required-events-mask)
      (error 'events-not-reported
	     :unavailable-events (events-list-from-mask
				  (logand mta-provided-events required-events-mask))))
    (unless (= common-actions-mask performed-actions-mask)
      (error 'actions-not-allowed
	     :forbidden-actions (actions-list-from-mask
				 (logand (lognot mta-allowed-actions) performed-actions-mask))))
    (make-instance 'action-options
		   :version (min +protocol-version+
				 (event-options-version event))
		   :actions performed-actions-mask
		   :protocol-mask
		   (logand mta-provided-events
			   ;; we mask out some flags as these control
			   ;; some useless aspects of the milter
			   ;; protocol such as no-reply events
			   +all-events-mask+
			   (lognot required-events-mask)))))

(defmethod handle-event ((event event-end-of-message) (ctx milter-context))
  accept)

(defmethod handle-event ((event event-quit) (ctx milter-context))
  nil)

(defmethod handle-event ((event event-disconnect) (ctx milter-context))
  nil)

(defmethod handle-event ((event event-abort) (ctx milter-context))
  no-action)

(defmethod handle-event :before ((event mta-event) (ctx milter-context))
  (declare (ignore ctx))
  (dprint :protocol ">> ~A" event))

(defmethod handle-event ((event event-unknown) (ctx milter-context))
  (declare (ignore ctx))
  keep-going)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun send-byte (byte stream)
  "Write BYTE to STREAM.  Signal MILTER-BROKEN-COMMUNICATION if
it can't."
  (dprint :bytes "<<< ~A ~S" byte (code-char byte))
  (handler-case
      (write-byte byte stream)
    (t (c)
      (error 'milter-broken-communication :reason c))))

(defun send-sequence (sequence stream)
  "Write a SEQUENCE of bytes to STREAM.  Signal an error if it
can't."
  (loop
     for i from 0 to (1- (length sequence)) by 1
     do (send-byte (elt sequence i) stream)))

(defun send-packet (stream &rest data)
  "Send a protocol packet to the MTA through STREAM.  DATA is a
list of Lisp objects of different nature.  Try to convert them to
byte sequences before sending them down the drain... socket."
  (flet ((data-length (list)
	   (loop
	      with counter = 0
	      for item in list
	      do (etypecase item
		   (sequence (incf counter (length item)))
		   (integer (incf counter))
		   (character (incf counter)))
	      finally (return counter)))
	 (write-data (data stream)
	   (loop
	      for item in data
	      do (etypecase item
		   (null)
		   (string (send-sequence (map '(vector (unsigned-byte 8)) #'char-code item) stream))
		   (sequence (send-sequence item stream))
		   (integer (send-byte item stream))
		   (character (send-byte (char-code item) stream))))))
    
    (let ((len (data-length data)))
      (dprint :packet "SENDING ~A byte~:P packet, ~A item~:P"
	      len (length data))
      (write-data (cons (encode-int32 len) data) stream)
      (handler-case
	  (finish-output stream)
	(t (c)
	  (error 'milter-broken-communication :reason c))))))

(defmacro def-simple-printer (object fmt &rest args)
  "Define a PRINT-OBJECT method suitable for simple objects such
as the MILTER-ACTIONs."
  (let ((stream (gensym)))
    `(defmethod print-object (,object ,stream)
       (if *print-readably*
	   (call-next-method)
	   (format ,stream ,fmt ,@args)))))

(def-simple-printer (obj milter-action)
    "~A" (symbol-name (type-of obj)))

(def-simple-printer (obj mta-event)
    "~A" (symbol-name (type-of obj)))

;; To ease writing of handle-event for the end-of-message.
(defmethod send-action ((action milter-action) (context milter-context))
  (send-action action (ctx-socket context)))

(defmethod send-action ((action action-add-recipient) (stream stream))
  (with-slots (address parameters) action
    (if parameters
	(send-packet stream #\2 address #\null parameters #\null)
	(send-packet stream #\+ address #\null))))

(def-simple-printer (obj action-add-recipient)
    "ADD-RECIPIENT ~S~@[ (~A)~]"
  (slot-value obj 'address)
  (slot-value obj 'parameters))

(defmethod send-action ((action action-delete-recipient) (stream stream))
  (with-slots (address) action
    (send-packet stream #\- address #\null)))

(def-simple-printer (obj action-delete-recipient)
    "DELETE-RECIPIENT ~S" (slot-value obj 'address))

(defmethod send-action ((action action-accept) (stream stream))
  (send-packet stream #\a))

(defmethod send-action ((action action-replace-body) (stream stream))
  (with-slots (body) action
    (loop
       with len = (length body)
       for i from 0 by +max-body-chunk+
       until (>= i len)
       do (send-packet stream #\b (subseq body i (min len (+ i +max-body-chunk+)))))))

(defmethod send-action ((action action-continue) (stream stream))
  (send-packet stream #\c))

(defmethod send-action ((action action-no-action) (stream stream))
  ;; nothing to send to the MTA
  nil)

(defmethod send-action ((action action-discard) (stream stream))
  (send-packet stream #\d))

(defmethod send-action ((action action-change-sender) (stream stream))
  (with-slots (address parameters) action
    (if parameters
	(send-packet stream #\e address #\null parameters #\null)
	(send-packet stream #\e address #\null))))

(defmethod send-action ((action action-add-header) (stream stream))
  (with-slots (name value position) action
    (if position
	(send-packet stream #\i position #\null name #\null value #\null)
	(send-packet stream #\h name #\null value #\null))))

(def-simple-printer (action action-add-header)
    "ADD-HEADER ~A ~S~@[ at postion ~A~]"
  (slot-value action 'name)
  (slot-value action 'value)
  (slot-value action 'position))

(defmethod send-action ((action action-change-header) (stream stream))
  (with-slots (index name value) action
    (send-packet stream #\m (encode-int32 index) name #\null value #\null)))

(def-simple-printer (action action-change-header)
    "CHANGE-HEADER ~A ~A ~S"
  (slot-value action 'index)
  (slot-value action 'name)
  (slot-value action 'value))

(defmethod send-action ((action action-progress) (stream stream))
  (send-packet stream #\p))

(defmethod send-action ((action action-quarantine) (stream stream))
  (send-packet stream #\q (slot-value action 'reason) #\null))

(def-simple-printer (action action-quarantine)
    "QUARANTINE ~S" (slot-value action 'reason))

(defmethod send-action ((action action-reject) (stream stream))
  (send-packet stream #\r))

(defmethod send-action ((action action-skip) (stream stream))
  (send-packet stream #\s))

(defmethod send-action ((action action-temporary-failure) (stream stream))
  (send-packet stream #\t))

(defmethod send-action ((action action-reply-code) (stream stream))
  (with-slots (smtp-code text) action
    (send-packet stream #\y (format nil "~3,'0D ~A~C" smtp-code text #\null))))

(def-simple-printer (action action-reply-code)
    "REPLY-CODE ~3,'0D ~A" (slot-value action 'smtp-code) (slot-value action 'text))

(defmethod send-action ((action action-options) (stream stream))
  (with-slots (version actions protocol-mask) action
    (send-packet stream #\O
		 (encode-int32 version)
		 (encode-int32 actions)
		 (encode-int32 protocol-mask))))

(def-simple-printer (action action-options)
    "OPTIONS version=~A, actions=~32,'0,'.,8:B ~A, events=~32,'0,'.,8:B ~A"
  (slot-value action 'version)
  (slot-value action 'actions)
  (actions-list-from-mask (slot-value action 'actions))
  (slot-value action 'protocol-mask)
  (events-list-from-mask (lognot (slot-value action 'protocol-mask))))

(defmethod send-action :before ((action milter-action) stream)
  (declare (ignore stream))
  (dprint :protocol "<< ~A" action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A load of PRINT-OBJECT methods for our event objects

(def-simple-printer (event event-connect)
    "CONNECT ~S ~A ~A ~S"
  (event-conn-host-name event)
  (event-conn-family event)
  (event-conn-port event)
  (event-conn-address event))

(def-simple-printer (event event-define-macro)
    "DEFINE-MACRO ~A ~S"
  (event-macro-command event)
  (event-macro-definitions event))

(def-simple-printer (event event-hello)
    "HELLO ~A" (event-hello-greeting event))

(def-simple-printer (event event-header)
    "HEADER ~A ~S" (event-header-name event) (event-header-value event))

(def-simple-printer (event event-mail)
    "MAIL from=~S options=~S" (event-mail-sender event) (event-mail-options event))

(def-simple-printer (event event-options)
    "OPTIONS version=~A, actions=~32,'0,'.,8:B ~A, events=~32,'0,'.,8:B ~A"
  (event-options-version event)
  (event-options-actions event) (actions-list-from-mask (event-options-actions event))
  (event-options-protocol-mask event) (events-list-from-mask (event-options-protocol-mask event)))

(def-simple-printer (event event-recipient)
    "RECIPIENT to=~S options=~S" (event-recipient-address event) (event-recipient-options event))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-loop (ctx)
  "Run the milter protocol loop using CTX as context.  It returns
on a MILTER-CONDITION or if any event handler returns NIL instead
of a MILTER-ACTION object."
  (unwind-protect
       (handler-case
	   (loop
	      for event = (receive-event (ctx-socket ctx))
	      for action = (handle-event event ctx)
	      while action
	      do (send-action action (ctx-socket ctx)))
	 (milter-broken-communication (c)
	   (dprint :protocol "~A" c))
	 (options-negotiation-error (c)
	   (dprint :error "Failure in options negotiations: ~A" c)
	   ;; A protocol mismatch can break the logic of the milter,
	   ;; therefore we propagate the error.
	   (error c)))
    (handle-event :disconnection ctx)))

(defmethod handle-event ((event (eql :connection)) (ctx milter-context))
  "The purpose of this method is to enter the main server loop.
Programmes may want to specialise this method to fork a new process or
fire a new thread and eventually do a CALL-NEXT-METHOD."
  (declare (ignore event))
  (server-loop ctx))

(defmethod handle-event ((event (eql :disconnection)) (ctx milter-context))
  (declare (ignore event))
  (ignore-errors (close (ctx-socket ctx))))

(defun start-milter (socket-description &key (context-class 'milter-context)
		     (events *default-events*) (actions *default-actions*))
  "Start the milter and enter an endless loop serving connections from
the MTA.  SOCKET-DESCRIPTION is the socket the server should listen to
for MTA connections.  If it's a string it's a service name
\(/etc/services).  If it's a number must be a port number.  If it's a
pathname it's the Unix domain socket (aka local).  CONTEXT-CLASS is
the class of the context objects to be passed to the event handlers.
It must inherit from MILTER-CONTEXT."
  (do-connections (socket socket-description :keep-open t)
    (dprint :connect "Received connection from MTA")
    (let ((ctx (make-instance context-class :socket socket
					    :events events
					    :actions actions)))
      (handle-event :connection ctx))))

;; This is how I spy on other milters
#+(OR)
(defun eavesdrop-milter (mta-port milter-port)
  (labels ((fd-set (fd)
	     (ash 1 fd))
	   (fd-set? (fd set)
	     (not (zerop (logand (fd-set fd) set)))))
    (macrolet ((enqueue (obj queue)
		 `(setf ,queue (nconc ,queue (list ,obj))))
	       (dequeue (queue)
		 `(pop ,queue))
	       (empty-queue? (queue)
		 `(null ,queue)))
      (do-connections (mta-stream mta-port)
	(format t "connection from MTA (~S) ~%" mta-stream) (finish-output)
	(with-open-socket (milter-fd (connect-to-inet-socket "localhost" milter-port))
	  (format t "connected to milter (~S)~%" milter-fd) (finish-output)
	  (with-socket-stream (milter-stream milter-fd)
	    (let ((milter->mta-queue '())
		  (mta->milter-queue '())
		  (mta-fd (sys:fd-stream-fd mta-stream)))
	      (loop
		 (multiple-value-bind (ok rfd wfd xfd)
		     (unix:unix-select (1+ (max mta-fd milter-fd))
				       (the (unsigned-byte 32)
					 (logior (fd-set mta-fd)
						 (fd-set milter-fd)))
				       (the (unsigned-byte 32)
					 (logior (if (empty-queue? milter->mta-queue) 0 (fd-set mta-fd))
						 (if (empty-queue? mta->milter-queue) 0 (fd-set milter-fd))))
				       0 1)
		   (declare (ignore xfd))
		   (unless ok
		     (error "select error ~A" rfd))
		   (format t "rfd=~A wfd=~A~%" rfd wfd) (finish-output)
		   (when (fd-set? mta-fd rfd)
		     (multiple-value-bind (command data)
			 (receive-packet mta-stream)
		       (format t "MTA-> ~S ~S~%" command data)
		       (enqueue (list command data) mta->milter-queue)))
		   (when (fd-set? milter-fd rfd)
		     (multiple-value-bind (command data)
			 (receive-packet milter-stream)
		       (format t "milter-> ~S ~S~%" command data)
		       (enqueue (list command data) milter->mta-queue)))
		   (when (fd-set? mta-fd wfd)
		     (destructuring-bind (command data) (dequeue milter->mta-queue)
		       (format t "->MTA ~S ~S~%" command data)
		     (send-packet mta-stream command data)))
		   (when (fd-set? milter-fd wfd)
		     (destructuring-bind (command data) (dequeue mta->milter-queue)
		       (format t "->milter ~S ~S~%" command data)
		       (send-packet milter-stream command data))))))))))))
