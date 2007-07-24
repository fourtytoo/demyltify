;;;  demyltify.asd --- system description

;;;  Copyright (C) 2004, 2007 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: Demyltify

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

(defpackage :demyltify-system
  (:use :common-lisp :asdf))

(in-package :demyltify-system)

(defsystem demyltify
    :name "DEMYLTIFY"
    :author "Walter C. Pelissero <walter@pelissero.de>"
    :maintainer "Walter C. Pelissero <walter@pelissero.de>"
    ;; :version "0.0"
    :description "Milter library"
    :long-description
    "Demyltify is a fully Lisp library to implement
milters (Sendmail filters).  Although inspired by libmilter it
doesn't require any external C library or support for threading."
    :licence "LGPL"
    :depends-on (:net4cl)
    :components
    ((:static-file "README")
     (:file "demyltify")
     (:module "examples"
	      :components
	      ((:static-file "simple.lisp")
	       (:static-file "threaded.lisp")
	       (:static-file "forked.lisp")))))
