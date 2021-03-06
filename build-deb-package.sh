#!/bin/bash -v

if [ -d package ]; then
	rm -rf package #harsh but effective. feel free to improve on this
fi
mkdir -p package/usr/bin/
mkdir -p package/usr/share/dargui/
cp -r DEBIAN package
cp -r menu/* pixmaps package/usr/share/
cp -r rundar.sh locales darlogger/darlogger package/usr/share/dargui/
mkdir -p package/usr/share/doc/dargui
cp changelog docs/* package/usr/share/doc/dargui/
mkdir -p package/usr/share/man/man1
gzip -c docs/man/dargui.1 > package/usr/share/man/man1/dargui.1.gz
echo '2.0' > package/debian-binary
#you need to have the project compiled into folder 'compiled' for this bit to work.
cp compiled/dargui package/usr/bin/
dpkg -b package dargui.deb
mv dargui.deb package/
