;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:cv-tests
  (:use #:cl)
  (:export #:edge-detect-image
           #:read-bar-code-from-camera))
