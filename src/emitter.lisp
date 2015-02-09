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
  "Print a key value pair as HTML."
  (format *output-stream* " ~A=~S" key value))

(defun emit-metadata (hash-table)
  "Print a hash table as HTML attributes."
  (when hash-table
    (loop for key being the hash-keys of hash-table
          for value being the hash-values of hash-table
          do
             (print-attribute key value))))

(defmacro with-tag ((tag-name node &key attributes self-closing-p)
                    &rest body)
  "Execute body after opening and before closing a tag."
  `(let ((tag-name ,tag-name))
     (format *output-stream* "<~A" tag-name)
     (when ,node
       (emit-metadata (metadata ,node)))
     (loop for attribute in ,attributes do
       (print-attribute (first attribute) (rest attribute)))
     (if ,self-closing-p
         (write-string "/>" *output-stream*)
         (progn
           (write-string ">" *output-stream*)
           ,@body
           (format *output-stream* "</~A>" tag-name)))))

;;; Emit methods

(defgeneric emit (node)
  (:documentation "Create an HTML representation of a CommonDoc document."))

(defmethod emit ((list list))
  "Emit a list."
  (loop for elem in list do (emit elem)))

(defmacro define-emitter ((node class) &rest body)
  "Define an emitter method."
  `(defmethod emit ((,node ,class))
     ,@body))

(defmacro define-simple-emitter (class tag-name)
  "Define a simple emitter."
  `(define-emitter (node ,class)
       (with-tag (,tag-name node)
         (emit (children node)))))

(define-emitter (node content-node)
  "The generic emitter for content nodes."
  (if (metadata node)
      (with-tag ("div" node)
        (loop for child in (children node) do
          (emit child)))
      (loop for child in (children node) do
        (emit child))))

(define-emitter (node text-node)
  "Emit a text node."
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
  "Emit a code block."
  (with-tag ("code" code
             :attributes (list (cons "language"
                                     (language code))))
    (emit (children code))))

(define-simple-emitter inline-quote "q")
(define-simple-emitter block-quote "blockquote")

(define-emitter (ref document-link)
  "Emit a document link."
  (let* ((sec-ref (section-reference ref))
         (doc-ref (document-reference ref))
         (url (if doc-ref
                  (format nil "~A.html/#~A" doc-ref sec-ref)
                  (format nil "#~A" sec-ref))))
    (with-tag ("a" ref
               :attributes (list (cons "href" url)))
      (emit (children ref)))))

(define-emitter (link web-link)
  "Emit a web link."
  (with-tag ("a" link
                 :attributes (list
                              (cons "href"
                                    (quri:render-uri (uri link)))))
    (emit (children link))))

(define-simple-emitter list-item "li")

(define-emitter (definition definition)
  "Emit a definition list item."
  (with-tag ("dt" (term definition))
    (emit (term definition)))
  (with-tag ("dd" (definition definition))
    (emit (definition definition))))

(define-simple-emitter unordered-list "ul")
(define-simple-emitter ordered-list "ol")
(define-simple-emitter definition-list "dl")

(define-emitter (image image)
  "Emit an image."
  (with-tag ("img" image
                   :attributes (list (cons "src" (source image))
                                     (cons "alt" (description image))
                                     (cons "title" (description image)))
                   :self-closing-p t)))

(define-emitter (fig figure)
  "Emit a figure."
  (with-tag ("figure" fig)
    (emit (image fig))
    (with-tag ("figcaption" nil)
      (emit (description fig)))))

(define-emitter (table table)
  "Emit a table."
  (with-tag ("table" table)
    (emit (rows table))))

(define-emitter (row row)
  "Emit a row."
  (with-tag ("tr" row)
    (emit (cells row))))

(define-simple-emitter cell "td")

(define-emitter (section section)
  "Emit a section."
  (macrolet ((section-emitter (tag)
               `(progn
                  (with-tag (,tag section)
                    (emit (title section)))
                  (incf *section-depth*)
                  (if (slot-boundp section 'children)
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

(defun node-to-stream (node stream)
  "Emit a node into a stream."
  (let ((*output-stream* stream)
        (*section-depth* 1))
    (emit node)))

(defun node-to-html-string (node)
  "Return an HTML string from a node."
  (with-output-to-string (stream)
    (node-to-stream node stream)))

(define-emitter (doc document)
  "Emit a full document."
  (let ((children-string (node-to-html-string (children doc))))
    (write-string (common-html.template:template doc children-string)
                  *output-stream*)))
