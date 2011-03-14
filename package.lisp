
(defpackage :cl-n-back
  (:nicknames :n-back)
  (:use :cl :iterate :ltk :funds :toolbox)
  ;; (:shadowing-import-from :toolbox after)
  ;; (:shadowing-import-from :iterate in for while)
  (:export :run-n-back) )
