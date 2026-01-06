#!/bin/bash

# TESTS="tests/"
# ANSWERS="answers/"

GOOD=1
for DIR in tests/*; do
    DIR_PASSED=0
    DIR_TOTAL=0
    echo "$DIR: "
    for FILE in $DIR/*.nasm; do
        NAME=${FILE%.*}
        cabal run exes -- "$NAME" "$NAME"
        cargo run --quiet -- "$NAME.nano" > dummy.ok
        FILE_PASSED=1
        diff "dummy.ok" "$NAME.ok" >> /dev/null
        if [ $? -ne 0 ]; then
            FILE_PASSED=0
        fi
        FILE_STATUS="✗"
        if [ $FILE_PASSED -eq 1 ]; then
            FILE_STATUS="✓"
        fi
        DIR_PASSED=$((DIR_PASSED+FILE_PASSED))
        DIR_TOTAL=$((DIR_TOTAL+1))
        echo "  $NAME: $FILE_STATUS"
    done
    DIR_STATUS="passed"
    if [ "$DIR_PASSED" -ne "$DIR_TOTAL" ]; then
        GOOD=0
        DIR_STATUS="failed"
    fi
    echo "$DIR: $DIR_STATUS [$DIR_PASSED / $DIR_TOTAL]"
    echo ""
done
rm -f dummy.ok
if [ $GOOD -eq 1 ]; then
    echo "success :)"
    exit 0
else
    echo "failure :("
    exit 1
fi