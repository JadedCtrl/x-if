(defsystem "x-if"
	   :version "0.0"
           :license "GPLv3"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
	   :description "A flexible IF engine."
	   :depends-on (:cl-earley-parser :bknr.datastore :arnesi :cl-strings)
	   :components ((:file "x-if")))
