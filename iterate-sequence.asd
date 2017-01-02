(defsystem :iterate-sequence
  :description "An iterate driver for extended sequences"
  :license     "MIT"

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on  (:iterate
                #+later (:feature :sbcl))

  :components  ((:file "iterate-sequence"))

  :in-order-to ((test-op (test-op :iterate-sequence/test))))

(defsystem :iterate-sequence/test
  :depends-on  (:iterate-sequence

                :fiveam)

  :components  ((:file "test")))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :iterate-sequence/test))))
  (uiop:symbol-call '#:iterate-sequence.test '#:run-tests))
