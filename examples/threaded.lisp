;;;  threaded.lisp --- sample milter with threads

;;;  Copyright (C) 2007, 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: Demyltify

#+cmu (ext:file-comment "$Module: threaded.lisp $")

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
;;; Sample code for CMUCL for a threaded milter.  This file adds only
;;; the necessary bits to make the milter threaded; the rest is in
;;; simple.lisp.
;;;
;;; INPUT_MAIL_FILTER(`myfilter', `S=inet:20025@localhost, F=T')

(cl:in-package :cl-user)

(unless (find-package "MY-MILTER")
  (load (merge-pathnames #P"simple"
			 #.(or *compile-file-truename*
			       *load-truename*))))

(cl:in-package :my-milter)

(defmethod send-action :after ((action milter-action) stream)
  (declare (ignore action stream))
  (mp:process-yield))

(defmethod handle-event :after ((event mta-event) (ctx my-context))
  (declare (ignore event ctx))
  (mp:process-yield))

(defmethod handle-event ((event (eql :connection)) (ctx my-context))
  (declare (ignore event ctx))
  (mp:make-process #'(lambda ()
		       (call-next-method))
		   :name "My sample milter connection."))
