#!/bin/bash
for file in *.lisp; do
echo \"$file\"
sbcl --eval "(progn (load-system :gordon) (load \"$file\") (quit))"
done
