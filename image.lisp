(load #P"~/quicklisp/setup.lisp")
(proclaim '(optimize (debug 3) (safety 3)))
(require 'asdf)
(in-package :cl-user)
(load "pacman.asd")
(asdf::compile-system :pacman)
(asdf::load-system :pacman)

(print "Welcome to PFCI 4102 Contest's Pacman bot")
(sb-ext:save-lisp-and-die "pacman.image" :executable t)


