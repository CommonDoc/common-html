# CommonHTML

[![Build Status](https://travis-ci.org/CommonDoc/common-html.svg)](https://travis-ci.org/CommonDoc/common-html)

An HTML parser/emitter for [CommonDoc](https://github.com/CommonDoc/common-doc).

# Usage

```lisp
(defvar node
  (doc
   (document
    (:title "My Document"
     :creator "me"
     :keywords (list "test" "test1"))
    (paragraph
     ()
     (text-node
      (:text "test"))))))

(common-html.emitter:node-to-html-string node) ;; => "<p>test</p>"
```

# Multi-file emission

Normally, a document is emitted into HTML as a single file. You can also perform
Texinfo/Sphinx style emission, where a document is broken up into sections, and
each section (Up to a certain depth, or any depth) is emitted as a different
file.

Non-section nodes between sections are omitted.

## How it Works

Multi-part file emission can be complicated.

First, some obvious choices, and how CommonHTML chooses:

1. Should the directory structure of the HTML output mirror that of the
   sections? Or should all HTML files be emitted within the same directory?
   Answer: For simplicity (Users might not expect or want nested files), all
   HTML files are emitted into the same directory.

2. What should the name of the resulting HTML files be? The pure name of the
   section? The result of calling `common-doc.util:string-to-slug` on the
   section? Or an autogenerated ID? Answer: The first option might produce
   invalid pathnames, and the last option is an inconceivable abomination, so we
   just go with slugifying the section text.

Now, the actual emission algorithm:

1. We recursively go through the docuement. When we find a section, we compute
   the name of the file it will be stored in (i.e. slugify the section title).

2. We add the filename to a hash table to make sure two different sections don't
   collide into the same HTML file.

3. We go through the body, collecting ordinary nodes and sections into two
   separate lists. We make a content node out of the list of sections and render
   it into the file.

4. Finally, we go through the list of sections and start the process again.

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.
