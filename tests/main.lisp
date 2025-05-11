(defpackage logadat/tests/main
  (:use :cl
        :logadat
        :rove))
(in-package :logadat/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :logadat)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
