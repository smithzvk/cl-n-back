
(asdf:defsystem :cl-n-back
  :author "Zach Smith"
;  :date "June 25, 2008"
  :license "GPL v3 (See the COPYING file)."
  :components ((:file "package")
               (:file "n-back") )
  :serial t
  :depends-on (:ltk :funds :iterate :bordeaux-threads
                    :toolbox :clommand ))
