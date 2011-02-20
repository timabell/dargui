#!/bin/bash

CommandLine="$1"

echo $CommandLine
echo $CommandLine > /tmp/dargui/darcommand.sh
chmod +x /tmp/dargui/darcommand.sh
/tmp/dargui/darcommand.sh  | /usr/share/dargui/darlogger $Logfile
rm /tmp/dargui/darcommand.sh
DarStatus=$?

echo "--------------------------------------------------"
echo "Operation complete: Press enter"
echo -ne "\e]2;Operation complete. Please press ENTER \a"
read anyinput

exit $DarStatus

# end.



