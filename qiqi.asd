(defsystem "qiqi"
  :version "0.0.1"
  :author "Lihui Zhang"
  :mailto "zlihui486@gmail.com"
  :license "GPL3"
  :depends-on ("iterate"
               "qiku")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A Chess engine in Common Lisp"
  :in-order-to ((test-op (test-op "qiqi/tests"))))

(defsystem "qiqi/tests"
  :author "Lihui Zhang"
  :license "GPL3"
  :depends-on ("qiqi"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for qiqi"
  :perform (test-op (op c) (symbol-call :rove :run c)))
