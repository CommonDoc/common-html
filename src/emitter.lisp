(in-package :cl-user)
(defpackage common-html.emitter
  (:use :cl :common-doc)
  (:export :node-to-stream
           :node-to-html-string)
  (:documentation "Emit HTML5 from a CommonDoc document."))
(in-package :common-html.emitter)

;;; Variables

(defvar *output-stream* nil
  "The stream the HTML will be written to.")

(defvar *section-depth*
  "The depth of `section` classes. Used to produce header numbers, e.g. `h1, `h3`.")

;;; Utilities

(defun print-attribute (key value)
  (format *output-stream* " ~A=~S" key value))

(defun emit-metadata (hash-table)
  (loop for key being the hash-keys of hash-table
        for value being the hash-values of hash-table
        do
           (print-attribute key value)))

(defmacro with-tag ((tag-name node &optional attributes) &rest body)
  `(let ((tag-name ,tag-name))
     (format *output-stream* "<~A" tag-name)
     (emit-metadata (metadata ,node))
     (loop for attribute in ,attributes do
       (print-attribute (first attribute) (rest attribute)))
     (write-string ">" *output-stream*)
     ,@body
     (format *output-stream* "</~A>" tag-name)))

;;; Emit methods

(defgeneric emit (node)
  (:documentation "Create an HTML representation of a CommonDoc document."))

(defmethod emit ((list list))
  "Emit a list."
  (loop for elem in list do (emit elem)))

(defmacro define-emitter ((node class) &rest body)
  "Define an emitter method."
  `(defmethod emit ((node ,class))
     ,@body))

(defmacro define-simple-emitter (class tag-name)
  "Define a simple emitter."
  `(define-emitter (node ,class)
       (with-tag (,tag-name node)
         (emit (children node)))))

(define-emitter (node content-node)
  "The generic emitter for content nodes."
  (loop for child in (children node) do
    (emit child)))

(define-emitter (node text-node)
    (if (metadata node)
        (with-tag ("span" node)
          (write-string (text node) *output-stream*))
        (write-string (text node) *output-stream*)))

(define-simple-emitter paragraph "p")
(define-simple-emitter bold "b")
(define-simple-emitter italic "i")
(define-simple-emitter underline "u")
(define-simple-emitter strikethrough "strike")
(define-simple-emitter code "code")
(define-simple-emitter superscript "sup")
(define-simple-emitter subscript "sub")

(define-emitter (code code-block)
  (with-tag ("code" code (list (cons "language"
                                     (language code))))
    (emit (children code))))

(define-simple-emitter inline-quote "q")
(define-simple-emitter block-quote "blockquote")

(define-child-emitter (ref document-link)
  (let* ((sec-ref (section-reference ref))
         (doc-ref (document-reference ref))
         (url (if doc-ref
                  (format nil "~A.html/#~A" doc-ref sec-ref)
                  (format nil "#~A" sec-ref))))
    (with-tag ("a" ref
                   (list (cons "href" url)))
      (emit (children ref)))))

(define-child-emitter (link web-link)
  (with-tag ("a" link (list (cons "href"
                                  (quri:render-uri (uri node)))))
    (emit (children link))))

(define-simple-emitter list-item "li")

(define-emitter definition
  (html (:dt (emit (term node)))
        (:dd (emit (definition node)))))

(define-simple-emitter unordered-list "ul")
(define-simple-emitter ordered-list "ol")
(define-simple-emitter definition-list "dl")

(define-emitter (image image)
  (with-tag ("img" image
                   (list (cons "src" (source node))
                         (cons "alt" (description node))
                         (cons "title" (description node))))))

(define-emitter figure
  (html (:figure
         (emit (image node))
         (:figcaption (emit (description node))))))

(define-emitter (table table)
    (with-tag ("table" table)
      (emit (rows table))))

(define-emitter (row row)
    (with-tag ("tr" row)
      (emit (cells row))))

(define-simple-emitter cell "td")

(define-emitter (section section)
  (macrolet ((section-emitter (tag)
               `(progn
                  (with-tag (,tag section)
                    (emit (title section)))
                  (incf *section-depth*)
                  (if (slot-boundp node 'children)
                      (emit (children section)))
                  (decf *section-depth*))))
    (case *section-depth*
      (1 (section-emitter "h1"))
      (2 (section-emitter "h2"))
      (3 (section-emitter "h3"))
      (4 (section-emitter "h4"))
      (5 (section-emitter "h5"))
      (6 (section-emitter "h6"))
      (t (section-emitter "h6")))))

(define-emitter (doc document)
  (progn
      (markup:html5
       (:head
        (:title (title doc)))
       (:body
        (emit (children doc))))
      nil))

(defun node-to-stream (node stream)
  "Emit a node into a stream."
  (let ((markup:*output-stream* stream)
        (*section-depth* 1))
    (emit node)))

(defun node-to-html-string (node)
  "Return an HTML string from a node."
  (with-output-to-string (stream)
    (let ((*output-stream* stream)
          (*section-depth* 1))
      (emit node))))
