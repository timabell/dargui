#!/bin/bash

p=$@

CommandLine=${p%;*}
Logfile=${p#*;}

echo $CommandLine > $Logfile

$CommandLine | /home/malcolm/lazarus/dargui/darlogger $Logfile
  
DarStatus=$?
  
echo $? > /tmp/dar_exit

echo "--------------------------------------------------"
echo "Operation complete: Press enter"
echo -ne "\e]2;DarGUI: Operation complete. Please press ENTER \a"
read anyinput

exit $DarStatus

# end.


