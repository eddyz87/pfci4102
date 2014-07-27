(asdf::defsystem :pacman
<<<<<<< HEAD
  :depends-on (:optima)
  :components ((:file "package")
               (:file "labels" :depends-on ("package"))
               (:file "dump"   :depends-on ("package"))
               (:file "lisp-compiler" :depends-on ("package" "labels" "dump"))
               (:file "c-compiler" :depends-on ("package" "ghc-asm"))
               (:file "ghc-asm" :depends-on ("package"))
               (:file "data-test")
               (:file "lambda-ai" :depends-on ("data-test"))))
=======
    :depends-on (:optima)
    :components ((:file "package")
		 (:file "labels" :depends-on ("package"))
		 (:file "dump"   :depends-on ("package"))
		 (:file "lisp-compiler" :depends-on ("package" "labels" "dump"))
                 (:file "c-compiler" :depends-on ("package" "ghc-asm"))
                 (:file "ghc-asm" :depends-on ("package"))
                 (:file "data-test")
                 (:file "lambda-ai" :depends-on ("data-test"))))
>>>>>>> e5c005a80271867d67f7ddb4a6f64b9164f5d295
