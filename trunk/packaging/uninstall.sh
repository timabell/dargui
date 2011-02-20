# /bin/sh

 if ! test -w /usr/bin ; then 
	echo You need to be root to run this script
	exit 0
	fi

rm -fR /usr/share/doc/dargui
rm -f /usr/share/man/man1/dargui.1.gz
rm -f /usr/share/applications/dargui.*
rm -f /usr/share/pixmaps/dargui*
rm -f /usr/share/menu/dargui

rm -fR /usr/share/dargui
rm -f /usr/bin/dargui

echo removing user files
if [ -e $HOME/.dargui ] ; then
	rm -rfv $Home/.dargui
	fi
if  [ -e $HOME/.config/dargui ] ; then
	rm -rfv $HOME/.config/dargui
	fi

if [ -x "`which update-menus 2>/dev/null`" ]; then update-menus ; fi

echo DarGUI has been removed
