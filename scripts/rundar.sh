#!/bin/bash

p=$@

CommandLine=${p%;*}
Logfile=${p#*;}

echo $CommandLine
echo $CommandLine > $Logfile

chmod +x /tmp/dargui-$USER/darcommand.sh
/tmp/dargui-$USER/darcommand.sh  | /usr/share/dargui/darlogger $Logfile
DarStatus=$?
rm /tmp/dargui-$USER/darcommand.sh

echo $DarStatus > /tmp/dargui-$USER/dar_exit
#echo DarStatus is $DarStatus

echo "--------------------------------------------------"
echo "Operation complete: Press enter"
echo -ne "\e]2;DarGUI: Operation complete. Please press ENTER \a"
read anyinput

exit $DarStatus

# end.



