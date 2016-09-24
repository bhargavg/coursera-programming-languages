#!/usr/bin/env bash

# Usage: 
#   $ ./bin/test.sh WEEK_NUMBER
# Example:
#   $ ./bin/test.sh 2    # to run week 2 tests


if [ -z "$1" ]
then
    echo "Usage: "
    echo "./bin/test.sh WEEK_NUMBER"
    echo ""
    echo "Example: "
    echo "./bin/test.sh 2"
    exit 1
fi

sml <"PartA/Assignments/0$1_Week_tests.sml"
