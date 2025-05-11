(defsystem "logadat"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "logadat/tests"))))

(defsystem "logadat/tests"
  :author ""
  :license ""
  :depends-on ("logadat"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for logadat"
  :perform (test-op (op c) (symbol-call :rove :run c)))
