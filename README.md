# CommonHTML

[![Build Status](https://travis-ci.org/CommonDoc/common-html.svg)](https://travis-ci.org/CommonDoc/common-html)

An HTML parser/emitter for [CommonDoc](https://github.com/CommonDoc/common-doc).

# Usage

```lisp
(defvar node
  (doc
   (<document>
    (:title "My Document"
     :creator "me"
     :keywords (list "test" "test1"))
    (<paragraph>
     ()
     (<text-node>
      (:text "test"))))))

(common-html.emitter:node-to-html-string node) ;; => "<p>test</p>"
```

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.
