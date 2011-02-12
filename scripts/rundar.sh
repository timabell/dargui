#!/bin/bash

Logfile=""

args=("$@")
for ((i=0; i < $#; i++)) {
  if [ "${args[$i]}" == "-c" ] ; then CommandLine="${args[$i+1]}"; fi
  if [ "${args[$i]}" == "-l" ] ; then Logfile="${args[$i+1]}"; fi
 #echo "${args[$i]}"
}

#echo $@
echo $CommandLine
#echo $Logfile
if [ "$Logfile" != "" ] ; then
  echo $CommandLine > $Logfile
fi

chmod +x /tmp/dargui-$USER/darcommand.sh
if [ "$Logfile" != "" ] ; then
  /tmp/dargui-$USER/darcommand.sh  | /usr/share/dargui/darlogger $Logfile
else
  /tmp/dargui-$USER/darcommand.sh
fi

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



