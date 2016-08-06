(cl:in-package #:asdf-user)

(defsystem :transclime
  :description "CLIM II application for learning foreign languages."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD, see file LICENSE.text"
  :depends-on (:mcclim :split-sequence)
  :serial t
  :components
  ((:file "packages")
   (:file "main")))
