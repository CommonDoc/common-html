(defsystem common-html
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :cl-markup
               :plump)
  :components ((:module "src"
                :serial t
                :components
                ((:file "emitter"))))
  :description "An HTML parser/emitter for CommonDoc."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op common-html-test))))
