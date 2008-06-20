;; copyright (c) 2006-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: agdtest.cl,v 1.1 2008/06/20 18:36:17 bmacgregor Exp $

(in-package :user)

#+ignore
(eval-when (compile load eval)
  (load (compile-file-if-needed "agdirect.cl"))
  (provide :agdirect))

;; The functions below are called from the Java test program AGDTest.java
(defun test-0-1 () (values 17))
(defun test-1-1 (x) (values x))
(defun test-1-2 (x) (values x x))
(defun test-2-2 (x y) (values y x y x))

(defun start-agd (&key (port 4567) (verbose t) (debug nil))
  (db.agraph.dlink::start-server port :limit 2 :debug debug :verbose verbose))

(defun stop-agd () (db.agraph.dlink::stop-server :all))

