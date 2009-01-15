#!/bin/bash
# takes a processname as a parameter and checks to see if it is running

TestApp=$1

ps -A | grep $TestApp

echo $CommandLine

exit $?

# end.



