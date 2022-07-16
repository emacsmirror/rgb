#!/bin/bash
EMACS=emacs
OPTIONS="-L . -L $HOME/emacs/lisp"
OUTPUT=/tmp/.el-expectations
$EMACS -q --no-site-file --batch $OPTIONS -l rgb-test.el -f batch-expectations $OUTPUT "$@"
ret=$?
exit $ret
