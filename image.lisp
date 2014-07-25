(load #P"~/quicklisp/setup.lisp")
(declaim (optimize debug))
(require 'asdf)
(in-package :cl-user)
(load "pacman.asd")
(asdf::compile-system :pacman)

(print "Welcome to PFCI 4102 Contest's Pacman bot")
(sb-ext:save-lisp-and-die "pacman.image" :executable t)


