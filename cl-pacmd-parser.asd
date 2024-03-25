;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; -*-

(defsystem :cl-pacmd-parser
  :description "Parser for pacmd's various listings"
  :license "MIT"
  :author "Mihail Ivanchev"
  :depends-on (:esrap)
  :version "0.1"
  :serial t
  :components ((:file "package")
               (:file "parser")))
