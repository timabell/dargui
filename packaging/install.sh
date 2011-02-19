# /bin/sh

cd `dirname $0`

if ! test -w /usr/bin ; then
	echo You need to be root to install dargui
	exit 0
	fi

mkdir /usr/share/doc/dargui
cp -vR doc/* /usr/share/doc/dargui/
#cp -v doc/copyright /usr/share/doc/dargui/
cp -v man/dargui.1.gz /usr/share/man/man1/

cp -v menu/* /usr/share/menu/
chmod 644 /usr/share/menu/dargui
cp -v applications/* /usr/share/applications/
chmod 644 /usr/share/applications/dargui.desktop
cp -v pixmaps/* /usr/share/pixmaps/

mkdir /usr/share/dargui
cp -v scripts/* /usr/share/dargui/
cp -v darlogger /usr/share/dargui/

mkdir /usr/share/dargui/locales
cp -v locales/* /usr/share/dargui/locales/
cp -v dargui /usr/bin/

if [ -x "`which update-menus 2>/dev/null`" ]; then
	update-menus
fi

echo DarGUI has been installed successfully
