(cl:defpackage #:iterate-sequence.test
  (:use
   #:cl
   #:iterate
   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:iterate-sequence.test)

(def-suite iterate-sequence
  :description
  "Root test suite for the iterate-sequence system.")
(in-suite iterate-sequence)

(defun run-tests ()
  (run! 'iterate-sequence))

(defun test-with-sequence (sequence)
  ;; Smoke test.
  (is (equal '(#\1 #\2 #\3)
             (iter (for element :each sequence)
                   (collect element))))

  ;; With index.
  (is (equal '((#\1 . 0) (#\2 . 1) (#\3 . 2))
             (iter (for element :each sequence :with-index i)
                   (collect (cons element i)))))

  ;; From and downfrom
  (is (equal '(#\2 #\3)
             (iter (for element :each sequence :from 1)
                   (collect element))))
  (is (equal '(#\2 #\1)
             (iter (for element :each sequence :downfrom 1)
                   (collect element))))

  ;; To and downto
  (is (equal '(#\1 #\2)
             (iter (for element :each sequence :to 1)
                   (collect element))))
  (is (equal '(#\3 #\2)
             (iter (for element :each sequence :downto 1)
                   (collect element)))))

(test each-driver.list
  "Test list iteration."

  (test-with-sequence '(#\1 #\2 #\3)))

(test each-driver.vector
  "Test vector iteration."

  (test-with-sequence #(#\1 #\2 #\3)))

(test each-driver.string
  "Test string iteration."

  (test-with-sequence "123"))
