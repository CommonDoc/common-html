(defsystem common-html
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/CommonDoc/common-html"
  :bug-tracker "https://github.com/CommonDoc/common-html/issues"
  :source-control (:git "git@github.com:CommonDoc/common-html.git")
  :depends-on (:common-doc
               :plump)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "template")
                 (:file "toc")
                 (:file "emitter")
                 (:file "multi-emit")
                 (:file "common-html"))))
  :description "An HTML parser/emitter for CommonDoc."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op common-html-test))))
