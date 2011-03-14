
(defpackage :cl-n-back
  (:nicknames :n-back)
  (:use :cl :iterate :ltk :trivial-shell :funds :toolbox)
  ;; (:shadowing-import-from :toolbox after)
  ;; (:shadowing-import-from :iterate in for while)
  (:export :run-n-back) )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :cl-n-back)

  (import 'trivial-shell::create-shell-process) )
