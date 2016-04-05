(defsystem :iterate-sequence
  :description "An iterate driver for extended sequences"
  :license     "MIT"

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on  (:iterate
                #+later (:feature :sbcl))

  :components  ((:file "iterate-sequence")))
