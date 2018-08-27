;;;; cv-tests.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:cv-tests
  :description "Describe cv-tests here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cv-tests"))
  :depends-on (#:cffi #:common-cv #:trivial-main-thread)
  :in-order-to ((test-op (test-op :cv-tests.test))))
