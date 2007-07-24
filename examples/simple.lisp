;;;  mymilter.lisp --- sample milter with demyltify

;;;  Copyright (C) 2004, 2007 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: Demyltify

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;;  Commentary:
;;;
;;; Sample milter that gathers statistics about number of messages and
;;; their size.  Add this line to your sendmail.mc:
;;;
;;; INPUT_MAIL_FILTER(`myfilter', `S=inet:20025@localhost, F=T')

(cl:in-package :cl-user)

(defpackage :my-milter
  (:nicknames :my-lousy-milter)
  (:use :common-lisp :milter)
  (:export #:start-my-milter))

(in-package :my-milter)

(defvar *message-counter* 0)
(defvar *byte-counter* 0)

;; we specialise the context to add the byte count per message
(defclass my-context (milter-context)
  ((byte-count :accessor ctx-byte-count)))

;; here we add up the byte count per message
(defmethod handle-event ((e event-body) (ctx my-context))
  (incf (ctx-byte-count ctx) (length (event-body-data e)))
  keep-going)

;; at the beginning of each message we reset the counter
(defmethod handle-event ((e event-mail) (ctx my-context))
  (setf (ctx-byte-count ctx) 0)
  keep-going)

;; at the end of the message we update the global statistics and print
;; a brief report of the situation so far
(defmethod handle-event ((e event-end-of-message) (ctx my-context))
  (incf *byte-counter* (ctx-byte-count ctx))
  (incf *message-counter*)
  (format t "~
~:R message of ~A byte~:P~%~
the messages seen so far total ~A byte~:P~%~
for an average of ~A byte~:P per message~%"
	  *message-counter* (ctx-byte-count ctx)
	  *byte-counter*
	  (round *byte-counter* *message-counter*))
  (finish-output)
  (action ctx action-add-header :name :x-message-size
	  ;; we need a string because send-packet can't serialise
	  ;; integers (it can't know the required length)
	  :value (format nil "~A" (ctx-byte-count ctx)))
  accept)

(defun start-my-milter ()
  ;; we need to know about body events to get the body length
  (let ((*required-events* '(:mail :body :header :end-of-headers))
	(*suitable-actions* '(:add-header))
	;; (*log-features* t)
	(*log-file* #P"mymilter.log"))
    (start-milter 20025 :context-class 'my-context)))
