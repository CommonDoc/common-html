# cd-html

[![Build Status](https://travis-ci.org/CommonDoc/cd-html.svg)](https://travis-ci.org/CommonDoc/cd-html)

An HTML parser/emitter for [CommonDoc](https://github.com/CommonDoc/common-doc).

# Usage

```lisp
CL-USER> (defvar *node*
  (make-instance 'common-doc:<paragraph>
                 :children
                 (list
                  (make-instance 'common-doc:<text-node>
                                 :text "test"))))

*NODE*
CL-USER> (common-doc.html:node-to-html-string *node*)
"<p>test</p>"
```

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.
