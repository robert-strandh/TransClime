(cl:in-package #:asdf-user)

(defsystem :transclime
  :depends-on (:mcclim :split-sequence :mcclim-truetype)
  :serial t
  :components
  ((:file "packages")
   (:file "main")))
