%define name    dargui
%define version 0.5.2
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
Requires:  dar libvte9 

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
%attr(0755, root, root) /usr/share/dargui/locales/dargui.de.po
%attr(0755, root, root) /usr/share/dargui/locales/lclstrconsts.de.po
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
%attr(0755, root, root) /usr/share/doc/dargui/preferences.html
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
%attr(0755, root, root) /usr/share/doc/dargui/images/user-preferences.png
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
* Sun Feb 20 2011 0.5.2.i386.rpm
	- Bug fix: cannot restore files from encrypted archive
	- User preferences dialog added
	- Location of dar executable can be set by user
	- Cron no longer unavailable as a result of no crontab found
	- Added Swedish translation files provided by Peter Landgren
	- fixed bug #2773134 Missing switch for encrypted reference archive
	- Improved pre-opening routine to detect multiple slice archives and errors
	- Source now includes packaging script and associated files

* Fri Mar 27 2009 0.5.1.i386.rpm
	- Improved handling of larger than normal GUI fonts
	- Implemented ability to handle French and German output from Dar
	- Now handles encrypted files which have spaces in the password
	- Lists files marked as "Removed" in differential backup archives
	- Main window position remembered between sessions
	- Now accepts an archive name as a commandline parameter and opens it on startup
	- bug causing filenames to overflow into adjacent column fixed
	- Added some checks for possible problems with cron, atd and dar

* Fri Feb 27 2009 0.5.0.i386.rpm
	- Fixed bug preventing opening files with aspace in the path
	- Fixed a number of bugs in the New Archive dialog
	- Backup settings can now be saved for reuse
	- Can now save backup scripts
	- It is now possible to schedule backups using at or cron
	- HTML Help added

* Tue Sep 16 2008 0.4.1.i386.rpm
	- Fixed bug preventing loading of differential backups containing REMOVED entries
	- Fixed bug where scrollbars not initially visible
	- Menu option Show Selected now works as expected
	- Added support for Blowfish encryption

* Wed Aug 20 2008 0.4.0.i386.rpm
	- Open archive routine rewritten to give greatly improved loading of large archives
	- Differential backup now possible
	- Can include timestamp in archive name
	- Improved error messages when creating archives
	- Translated to French by Gwen Trellu
	- Compare Archive and Filesystem window can now be translated
	- Spaces in archive names are now replaced by underscores _
	- Changed start command in KDE menu entry
	- Added option to avoid loading filelist of newly created archives

* Sun Apr 6 2008 0.3.0.i386.rpm
	- Improvements to display of files in archive
	- Select files in archive using filters
	- Improved checking of user input
	- Open recent archives added
	- Spanish translation added
	- Some minor improvements to interface

* Sun Mar 9 2008 0.2.0.i386.rpm
	- files in archive are now listed alphabetically
	- improved handling of operation logs
	- fixed a memory leak related to allocating names for batch files
	- added dar function for isolating catalogue from archive
	- added dar function for comparing archive contents with filesystem
	- added README files to binary and source tarballs

* Wed Feb 13 2008 0.1.0.i386.rpm
        - initial release
