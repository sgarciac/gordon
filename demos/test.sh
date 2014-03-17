#!/bin/bash
sbcl --eval "(progn (load-system :gordon) (load \"$1\") (quit))"
