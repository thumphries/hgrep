#!/bin/sh

tempfile=`mktemp`

for file in stack-*.yaml
do
    if
        stack --stack-yaml "$file" build
    then
        echo -e "Build with $file\t: success." >> "$tempfile"
    else
        echo -e "Build with $file\t: failure." >> "$tempfile"
    fi
done

cat "$tempfile"
