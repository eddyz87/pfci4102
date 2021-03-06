(asdf::defsystem :pacman
                 :depends-on (:optima)
                 :components ((:file "package")
                              (:file "labels" :depends-on ("package"))
                              (:file "dump"   :depends-on ("package"))
                              (:file "lisp-compiler" :depends-on ("package" "labels" "dump"))
                              (:file "c-compiler" :depends-on ("package" "ghc-asm" "labels"))
                              (:file "ghc-asm" :depends-on ("package"))
                              (:file "data-test")
                              (:file "lambda-ai" :depends-on ("data-test"))))

