(asdf::defsystem :pacman
    :depends-on (:optima)
    :components ((:file "package")
		 (:file "labels" :depends-on ("package"))
		 (:file "dump"   :depends-on ("package"))
                 ))
