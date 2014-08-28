(cl:in-package #:common-lisp-user)

(asdf:defsystem :transclime
  :depends-on (:mcclim :split-sequence :mcclim-truetype)
  :serial t
  :components
  ((:file "packages")
   (:file "main")))
