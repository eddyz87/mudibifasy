#!/bin/bash
(cd $(dirname "$0"); rlwrap sbcl --load src/init.lisp)
