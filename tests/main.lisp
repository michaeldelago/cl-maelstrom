(defpackage cl-maelstrom/tests/main
  (:use :cl
        :cl-maelstrom
        :rove))
(in-package :cl-maelstrom/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-maelstrom)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
