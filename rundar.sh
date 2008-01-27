#!/bin/sh

CommandLine=$@

echo $CommandLine
$CommandLine
  
DarStatus=$?
  
echo $? > /tmp/dar_exit

echo "--------------------------------------------------"
echo "Operation complete: Press enter"
read anyinput

exit $DarStatus

# end.
