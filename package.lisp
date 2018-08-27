;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:cv-tests
  (:use #:cl)
  (:export #:edge-detect-image
           #:check-isbn
           #:isbn-checksum
           #:isbn-10-to-13
           #:read-bar-code-from-camera))
