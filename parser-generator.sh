#!/bin/sh

sbcl \
    --noinform \
    --end-runtime-options \
    --eval '(require :com.nklein.parser-generator)' \
    --eval '(com.nklein.parser-generator:main)' \
    --eval '(sb-ext:quit)' \
    --end-toplevel-options \
    "$@" \
    --language lisp \
    --output-directory ./src/parser/ \
    --file examples/parser_generator_parser.xml \
    --prefix pg- \
    --types-package com.nklein.parser-generator.types \
    --reader-package com.nklein.parser-generator.reader


