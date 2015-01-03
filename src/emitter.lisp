(in-package :cl-user)
(defpackage common-html.emitter
  (:use :cl :common-doc)
  (:export :node-to-stream
           :node-to-html-string)
  (:documentation "Emit HTML5 from a CommonDoc document."))
(in-package :common-html.emitter)

;;; Utilities

(defmacro html (&rest body)
  "A wrapper around cl-markup's `markup` macro which works better in recursive
contexts."
  `(progn
     (markup:markup ,@body)
     nil))

(defvar *section-depth*
  "The depth of `<section>` classes. Used to produce header numbers, e.g. `h1, `h3`.")

;;; Emit methods

(defgeneric emit (node)
  (:documentation "Create an HTML representation of a CommonDoc document."))

(defmethod emit ((list list))
  "Emit a list."
  (loop for elem in list do (emit elem)))

(defmacro define-emitter (class &rest body)
  "Define an emitter method."
  `(defmethod emit ((node ,class))
     ,@body))

(defmacro define-child-emitter (class &rest tag)
  "Define a simple emitter for elements with `children`."
  `(define-emitter ,class
       (html (,@tag (emit (children node))))))

(define-emitter <content-node>
  "The generic emitter for content nodes."
  (loop for child in (children node) do
    (emit child)))

(define-emitter <text-node>
    (progn
      (write-string (text node) markup:*output-stream*)
      nil))

(define-child-emitter <paragraph> :p)
(define-child-emitter <bold> :b)
(define-child-emitter <italic> :i)
(define-child-emitter <underline> :u)
(define-child-emitter <strikethrough> :strike)
(define-child-emitter <code> :code)
(define-child-emitter <superscript> :sup)
(define-child-emitter <subscript> :sub)

(define-child-emitter <code-block>
  :code :language (language node))

(define-child-emitter <inline-quote> :q)
(define-child-emitter <block-quote> :blockquote)

(define-child-emitter <document-link>
  :a :href (let ((sec-ref (section-reference node))
                 (doc-ref (document-reference node)))
             (if doc-ref
                 (format nil "~A.html/#~A" doc-ref sec-ref)
                 (format nil "#~A" sec-ref))))

(define-child-emitter <web-link>
  :a :href (quri:render-uri (uri node)))

(define-child-emitter <list-item> :li)

(define-emitter <definition>
  (html (:dt (emit (term node)))
        (:dd (emit (definition node)))))

(define-child-emitter <unordered-list> :ul)
(define-child-emitter <ordered-list> :ol)
(define-child-emitter <definition-list> :dl)

(define-emitter <image>
    (html (:img :src (source node)
                :alt (description node)
                :title (description node))))

(define-emitter <figure>
  (html (:figure
         (emit (image node))
         (:figcaption (emit (description node))))))

(define-emitter <table>
    (html (:table (emit (rows node)))))

(define-emitter <row>
    (html (:tr (emit (cells node)))))

(define-child-emitter <cell> :td)

(define-emitter <section>
  (macrolet ((section-emitter (tag)
               `(progn
                  (html (,tag (emit (title node))))
                  (incf *section-depth*)
                  (if (slot-boundp node 'children)
                      (emit (children node)))
                  (decf *section-depth*))))
    (case *section-depth*
      (1 (section-emitter :h1))
      (2 (section-emitter :h2))
      (3 (section-emitter :h3))
      (4 (section-emitter :h4))
      (5 (section-emitter :h5))
      (6 (section-emitter :h6))
      (t (section-emitter :h6)))))

(define-emitter <document>
    (progn
      (markup:html5
       (:head
        (:title (title node)))
       (:body
        (emit (children node))))
      nil))

(defun node-to-stream (node stream)
  "Emit a node into a stream."
  (let ((markup:*output-stream* stream)
        (*section-depth* 1))
    (emit node)))

(defun node-to-html-string (node)
  "Return an HTML string from a node."
  (with-output-to-string (stream)
    (let ((markup:*output-stream* stream)
          (*section-depth* 1))
      (emit node))))
