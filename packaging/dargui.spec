%define name    dargui
%define version 0.5.6
%define release 1

Summary:   A GUI for the dar archive utility
Name:      %{name}
Version:   %{version}
Release:   %{release}
URL:       http://sourceforge.net/projects/dargui/
License:   GPL
Group:     Productivity/Archiving/Backup
Packager:  Malcolm Poole <mgpoole@users.sourceforge.net>
BuildRoot: %{root}
Requires:  dar

%description
DarGUI is a simple graphical interface for Denis Corbin's Disk ARchive utility (DAR)

%post
if [ "$1" = "configure" ] && [ -x "`which update-menus 2>/dev/null`" ]; then
	update-menus
fi

%postun
if [ -x "`which update-menus 2>/dev/null`" ]; then update-menus ; fi

%files
%attr(0755, root, root) /usr/bin/dargui
%attr(0755, root, root) /usr/share/dargui/darlogger
%attr(0755, root, root) /usr/share/dargui/rundar.sh
%attr(0755, root, root) /usr/share/dargui/checkprocess.sh
%attr(0755, root, root) /usr/share/dargui/locales/dargui.es.po
%attr(0755, root, root) /usr/share/dargui/locales/dargui.fr.po
%attr(0755, root, root) /usr/share/dargui/locales/dargui.pot
%attr(0755, root, root) /usr/share/dargui/locales/dargui.sv.po
%attr(0755, root, root) /usr/share/dargui/locales/lclstrconsts.es.po
%attr(0755, root, root) /usr/share/dargui/locales/lclstrconsts.fr.po
%attr(0755, root, root) /usr/share/dargui/locales/lclstrconsts.sv.po
%attr(0755, root, root) /usr/share/dargui/runscript.sh
%attr(0755, root, root) /usr/share/applications/dargui.desktop
%attr(0755, root, root) /usr/share/doc/dargui/changelog
%attr(0755, root, root) /usr/share/doc/dargui/copyright
%attr(0755, root, root) /usr/share/doc/dargui/index.html
%attr(0755, root, root) /usr/share/doc/dargui/backup-schedules.html
%attr(0755, root, root) /usr/share/doc/dargui/comparing-files.html
%attr(0755, root, root) /usr/share/doc/dargui/creating-archives1.html
%attr(0755, root, root) /usr/share/doc/dargui/creating-archives2.html
%attr(0755, root, root) /usr/share/doc/dargui/creating-archives3.html
%attr(0755, root, root) /usr/share/doc/dargui/creating-archives4.html
%attr(0755, root, root) /usr/share/doc/dargui/creating-archives5.html
%attr(0755, root, root) /usr/share/doc/dargui/creating-archives6.html
%attr(0755, root, root) /usr/share/doc/dargui/creating-archives7.html
%attr(0755, root, root) /usr/share/doc/dargui/faq.html
%attr(0755, root, root) /usr/share/doc/dargui/features.html
%attr(0755, root, root) /usr/share/doc/dargui/images/compare-files.png
%attr(0755, root, root) /usr/share/doc/dargui/images/file-filter-dialog.png
%attr(0755, root, root) /usr/share/doc/dargui/images/isolate-catalog.png
%attr(0755, root, root) /usr/share/doc/dargui/images/load-settings.png
%attr(0755, root, root) /usr/share/doc/dargui/images/newarchivetab1.png
%attr(0755, root, root) /usr/share/doc/dargui/images/newarchivetab2.png
%attr(0755, root, root) /usr/share/doc/dargui/images/newarchivetab3.png
%attr(0755, root, root) /usr/share/doc/dargui/images/newarchivetab4.png
%attr(0755, root, root) /usr/share/doc/dargui/images/newarchivetab5.png
%attr(0755, root, root) /usr/share/doc/dargui/images/newarchivetab6.png
%attr(0755, root, root) /usr/share/doc/dargui/images/restore-files-dialog.png
%attr(0755, root, root) /usr/share/doc/dargui/images/save-settings.png
%attr(0755, root, root) /usr/share/doc/dargui/images/schedule-manager.png
%attr(0755, root, root) /usr/share/doc/dargui/installation.html
%attr(0755, root, root) /usr/share/doc/dargui/introduction.html
%attr(0755, root, root) /usr/share/doc/dargui/isolating-catalogs.html
%attr(0755, root, root) /usr/share/doc/dargui/kde-default.css
%attr(0755, root, root) /usr/share/doc/dargui/limitations.html
%attr(0755, root, root) /usr/share/doc/dargui/restoring-files.html
%attr(0755, root, root) /usr/share/doc/dargui/template.html
%attr(0755, root, root) /usr/share/doc/dargui/testing-archives.html
%attr(0755, root, root) /usr/share/doc/dargui/using-dargui.html
%attr(0755, root, root) /usr/share/doc/dargui/viewing-archives.html
%attr(0755, root, root) /usr/share/man/man1/dargui.1.gz
%attr(0755, root, root) /usr/share/pixmaps/dargui.png
%attr(0755, root, root) /usr/share/pixmaps/dargui16.xpm
%attr(0755, root, root) /usr/share/pixmaps/dargui32.xpm
%attr(0755, root, root) /usr/share/menu/dargui


%changelog

* Wed Feb 13 2008 0.1.0.i386.rpm

- initial release
