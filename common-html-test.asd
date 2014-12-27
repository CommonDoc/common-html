(defsystem common-html-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:cd-html
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "common-html")))))
