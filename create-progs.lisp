(in-package :pc)

(with-open-file (*standard-output* "lambdaman.gcc" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (compile-program 'wave-main "std-lib.lisp" "data-test.lisp" "lambda-ai.lisp"))

(with-open-file (*standard-output* "ghost0.ghc" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (full-c-compliation *ghost-prog2*))

(with-open-file (*standard-output* "ghost1.ghc" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (full-c-compliation *ghost-prog-greedy*))
