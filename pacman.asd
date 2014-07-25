(asdf::defsystem :pacman
    :depends-on (:optima)
    :components ((:file "package")
		 (:file "labels" :depends-on ("package"))
                 (:file "lisp-compiler" :depends-on ("package"))
                 ))
