# /bin/sh

cd `dirname $0`

if ! test -w /usr/bin ; then
	echo You need to be root to install libdargui
	exit 0
	fi

if [ -e /usr/lib/libdargui.so ]; then rm /usr/lib/libdargui.so ; fi

cp -fv libdargui.so.1.0.1 /usr/lib/

ldconfig -nv /usr/lib | grep libdargui

ln -s /usr/lib/libdargui.so.1.0.1 /usr/lib/libdargui.so

echo done!