;;;; cv-tests.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package :cl-user)
(defpackage cv-tests.test-asd
  (:use :cl :asdf))
(in-package :cv-tests.test-asd)


(defsystem cv-tests.test
  :author "Jeremiah LaRocco"
  :mailto "jeremiah_larocco@fastmail.com"
  :description "Test cv-tests."
  :license "ISC"
  :depends-on (:cv-tests
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run :cv-tests))"))))
