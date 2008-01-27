#!/bin/bash

CommandLine=$@

echo $CommandLine
$CommandLine
  
DarStatus=$?
  
echo $? > /tmp/dar_exit

echo "--------------------------------------------------"
echo "Operation complete: Press enter"
echo -ne "\e]2;DarGUI: Operation complete. Please press ENTER \a"
read anyinput

exit $DarStatus

# end.
