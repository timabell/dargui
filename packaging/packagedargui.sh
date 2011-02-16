# /bin/bash

# this script creates packages for dargui from the standard directory structure as found in the SVN repository and source archive
# all four types of package will be created unless one or more type of package is specified as a parameter

# Parameters: 
#	-rpm : create RPM package
#	-deb : create Debian packages
#	-tar : create binary gzip archive with install script
#	-src : create source gzip archive
#	-v : verbose mode, reports copying of most files
all=0 # assume we're doing a full build
rpm=1; deb=1; tar=1; src=1;
for var in "$@"
do
  if [ "$var" == "-rpm" ] ; then  rpm=0; all=1; echo "making rpm"; fi
  if [ "$var" == "-deb" ] ; then deb=0; all=1; echo "making deb"; fi
  if [ "$var" == "-tar" ] ; then tar=0; all=1; echo "making tar"; fi
  if [ "$var" == "-src" ] ; then src=0; all=1; echo "making src"; fi
  if [ "$var" == "-v" ] ; then VERBOSITY="-v" ; fi
done

if test -w /usr/bin ; then
  echo "WARNING! - running packaging tools as root is not recommended!"
  echo "Script aborted"
  exit 0
fi

which fakeroot > /dev/null
if [ ! $? -eq 0 ] ; then
   echo "Please install fakeroot and try again."
   exit 0
fi

# let's find out what editors we have
echo "testing for presence of gedit or kwrite"
which gedit > /dev/null
[ $? -eq 0 ] && editor="gedit"
if [ "$editor" = "" ] ; then
  which kwrite > /dev/null
  [ $? -eq 0 ] && editor="kwrite"
  fi
[ "$editor" = "" ] && ( echo "no editor found"; ) || ( echo "Using $editor as text editor"; )

read -p "Enter the number of this release, followed by [ENTER]: " RELEASENUM

RELEASENAME="dargui-"$RELEASENUM

#move to the packaging directory if we're not there already
cd `dirname $0`

[ -e "../dargui.lpr" ] && cd .. || ( echo "project file dargui.lpr not found! Exiting..."; exit 1; )
BASEDIR=`pwd`

EXECDIR=`grep "<UnitOutputDirectory Value=" dargui.lpi`
EXECDIR=`echo $EXECDIR | cut --delimiter='"' -f 2 -s`

if [ ! -e $EXECDIR/dargui ] ; then echo "dargui executable not found! Exiting..."; exit 1; else echo "dargui executable found in $EXECDIR"; fi
[ ! -d "./releases" ] && mkdir `pwd`"/releases"
if [ -d "./releases" ] ; then RELEASEDIR=`pwd`"/releases/"$RELEASENAME; else echo "releases directory not found! Exiting..."; exit 1; fi
if [ ! -d $RELEASEDIR ] ; then
   mkdir $RELEASEDIR
   if [ ! $? -eq 0 ] ; then echo "could not create directory $RELEASEDIR! Exiting..."; exit 1; fi
fi

PACKAGEDIR=`pwd`/packaging
strip -s $EXECDIR/dargui

# remove any gz archives in directory ( why?)
files=$(ls $RELEASEDIR/*.gz 2> /dev/null | wc -l)
if [ "$files" != "0" ]
  then
  rm $VERBOSITY $RELEASEDIR/*.gz
fi

# update the changelog
$editor $BASEDIR/docs/changelog

# tarball
if [ $tar -eq 0 ]  || [ $all -eq 0 ] ; then
  TARDIR=$RELEASEDIR"/tar"
  if [ -e $TARDIR ] ; then
    echo "removing existing directory '$TARDIR'"
    rm -rf $TARDIR
  fi
  mkdir $TARDIR
  TARDIR=$TARDIR"/"$RELEASENAME
  mkdir $TARDIR
  cp $VERBOSITY $EXECDIR/dargui $TARDIR/
  cp $VERBOSITY $EXECDIR/darlogger $TARDIR/
  cp $VERBOSITY $PACKAGEDIR/*install.sh $TARDIR/
  $editor $PACKAGEDIR/README_bin > /dev/null
  cp $VERBOSITY $PACKAGEDIR/README_bin $TARDIR/README
  mkdir $TARDIR/doc
  cp $VERBOSITY $BASEDIR/docs/* $TARDIR/doc/
  #mkdir $TARDIR/doc/images
  cp $VERBOSITY -R `pwd`/docs/images $TARDIR/doc/
  mkdir $TARDIR/man
  cp $VERBOSITY `pwd`/docs/man/dargui.1.gz $TARDIR/man/
  mkdir $TARDIR/locales
  cp $VERBOSITY `pwd`/locales/*.*.po $TARDIR/locales/
  cp $VERBOSITY `pwd`/locales/dargui.pot $TARDIR/locales/
  mkdir $TARDIR/scripts
  cp $VERBOSITY $BASEDIR/scripts/*.sh $TARDIR/scripts/
  cp $VERBOSITY -R $BASEDIR/packaging $TARDIR/packagingtools
  cp $VERBOSITY `pwd`/docs/man/dargui.gz $TARDIR/doc/
  cp $VERBOSITY -R $PACKAGEDIR/deb_common/applications $TARDIR/
  cp $VERBOSITY -R $PACKAGEDIR/deb_common/menu $TARDIR/
  cp $VERBOSITY -R $PACKAGEDIR/deb_common/pixmaps $TARDIR/
  cp $VERBOSITY $BASEDIR/docs/changelog $TARDIR/doc/
  rm -f $TARDIR/*~
  rm -f $TARDIR/doc/*~
  find $TARDIR -name ".svn" -exec rm -rf {} \ > /dev/null   # remove SVN directories
  find $TARDIR -name ".directory" -exec rm -rf {} \ > /dev/null #remove .directory files
  echo creating tarball $RELEASENAME-bin.tar.gz
  cd $TARDIR/..
  fakeroot tar -cvzf ../$RELEASENAME-bin.tar.gz $RELEASENAME/ > /dev/null
  if [ $? -eq 0 ] ; then
  	echo binary tarball created successfully
  else
  	echo ERROR: tar returned error number $? when creating tarball
  fi
fi

cd $BASEDIR

# deb
if [ $deb -eq 0 ] || [ $all -eq 0 ] || [ $rpm -eq 0 ] ; then
  DEBDIR=$RELEASEDIR"/deb"
  if [ -e $DEBDIR ] ; then
     echo "removing existing directory '$DEBDIR'"
     rm -rf $DEBDIR;
  fi
  mkdir $DEBDIR
  DEBDIR=$DEBDIR"/"$RELEASENAME
  mkdir $DEBDIR
  if [ -e "$DEBDIR" ] ; then
    echo creating new deb directory
    mkdir "$DEBDIR/usr"
    mkdir "$DEBDIR/usr/bin"
    mkdir "$DEBDIR/usr/share"
    mkdir "$DEBDIR/usr/share/dargui"
    mkdir "$DEBDIR/usr/share/doc"
    mkdir "$DEBDIR/usr/share/doc/dargui"
    cp $VERBOSITY $EXECDIR/dargui $DEBDIR/usr/bin/
    cp $VERBOSITY $BASEDIR/docs/* $DEBDIR/usr/share/doc/dargui/
    rm $DEBDIR/usr/share/doc/dargui/dargui*
    mkdir $DEBDIR/usr/share/doc/dargui/images
    cp $VERBOSITY $BASEDIR/docs/images/* $DEBDIR/usr/share/doc/dargui/images
    mkdir -p $DEBDIR/usr/share/man/man1/
    cp $VERBOSITY $BASEDIR/docs/man/*.gz $DEBDIR/usr/share/man/man1/
    cp $VERBOSITY $EXECDIR/darlogger $DEBDIR/usr/share/dargui/
    cp $VERBOSITY $BASEDIR/scripts/*.sh $DEBDIR/usr/share/dargui/
    mkdir $DEBDIR/usr/share/dargui/locales
    cp $VERBOSITY $BASEDIR/locales/*.*.po $DEBDIR/usr/share/dargui/locales/
    cp $VERBOSITY $BASEDIR/locales/dargui.pot $DEBDIR/usr/share/dargui/locales/
    mkdir $DEBDIR/DEBIAN/
    $editor $PACKAGEDIR/deb_common/DEBIAN/control
    cp $VERBOSITY $PACKAGEDIR/deb_common/DEBIAN/* $DEBDIR/DEBIAN/
    cp $VERBOSITY $BASEDIR/docs/changelog $DEBDIR/usr/share/doc/dargui/changelog
    cp $VERBOSITY -R $PACKAGEDIR/deb_common/applications $DEBDIR/usr/share/
    cp $VERBOSITY -R $PACKAGEDIR/deb_common/menu $DEBDIR/usr/share/
    cp $VERBOSITY -R $PACKAGEDIR/deb_common/pixmaps $DEBDIR/usr/share/
    rm -f $DEBDIR/DEBIAN/*~
    rm -f $DEBDIR/usr/share/doc/dargui/*~
    rm -f $DEBDIR/usr/share/menu/*~
    find $DEBDIR -name .svn -print0 | xargs -0 rm -rf


    #find $DEBDIR -name ".svn" -exec rm -rf {} \  > /dev/null    # remove SVN directories
    find $DEBDIR -name ".directory" -exec rm -rf {} \ > /dev/null #remove .directory files

    echo "deb contents created in '$DEBDIR'"
  fi
  if [ $deb -eq 0 ] || [ $all -eq 0 ] ; then
    #is dpkg installed?
    which dpkg > /dev/null
    if [ $? -eq 0 ] ; then
       cd $RELEASEDIR
       fakeroot dpkg -b $DEBDIR ./$RELEASENAME.deb
       [ $? -eq 0 ] && echo "deb package created successfully in $RELEASEDIR" || echo "ERROR: dpkg failed with error number $? when creating deb package"
    else
      echo "dpkg not found - unable to create deb package"
    fi
  fi
fi

cd $BASEDIR

# rpm-tarball
if [ $all -eq 0 ] || [ $rpm -eq 0 ] ; then
  which rpmbuild > /dev/null
  if [ $? -eq 0 ] ; then
     $editor $PACKAGEDIR/dargui.spec
     cd $DEBDIR
     rm -R $DEBDIR/DEBIAN
     rpmbuild -bb --target i386-any-linux --buildroot $DEBDIR --define "_rpmdir /tmp" $PACKAGEDIR/dargui.spec
     if [  $? -eq 0 ] ; then
        RPM=`ls /tmp/i386/$RELEASENAME*`
        [ -e $RPM ] && mv $RPM $RELEASEDIR/
     else
         echo ERROR: rpmbuild returned error number $? when creating rpm package
     fi
  else
    echo "rpmbuild not found - unable to make rpm package"
  fi
fi


#src
if [ $src -eq 0 ] || [ $all -eq 0 ] ; then
  SRCDIR=$RELEASEDIR/src
  if [ -e $SRCDIR ] ; then
     echo "removing existing directory '$SRCDIR'"
     rm -rf $SRCDIR
  fi
  mkdir $SRCDIR
  SRCDIR=$SRCDIR"/"$RELEASENAME-src
  mkdir $SRCDIR
  cp $BASEDIR/docs/changelog $SRCDIR
  $editor $PACKAGEDIR/README_src
  cp $VERBOSITY -R $PACKAGEDIR $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.pas $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.lfm $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.lpr $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.lpi $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.lrs $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.ico $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.res $SRCDIR/
  cp $VERBOSITY $BASEDIR/*.inc $SRCDIR/
  mkdir $SRCDIR/scripts
  cp $VERBOSITY $BASEDIR/scripts/*.sh $SRCDIR/scripts
  cp $VERBOSITY -R $BASEDIR/docs $SRCDIR/
  mkdir $SRCDIR/Vte
  cp $VERBOSITY $BASEDIR/Vte/*.pas $SRCDIR/Vte/
  #mkdir -p $SRCDIR/docs/images
  #	cp $VERBOSITY ../../docs/* $SRCDIR/docs/
  #	cp $VERBOSITY ../../docs/images/* $SRCDIR/docs/images
  #	mkdir $SRCDIR/docs/man
  #	cp $VERBOSITY ../../docs/man/* $SRCDIR/docs/man/
  #	rm -f $SRCDIR/docs/*~
  #	mkdir $SRCDIR/scripts
  #	cp $VERBOSITY ../../scripts/* $SRCDIR/scripts
  #	mkdir $SRCDIR/locales
  #	cp $VERBOSITY ../../locales/*.*.po $SRCDIR/locales/
  #	cp $VERBOSITY ../../locales/dargui.pot $SRCDIR/locales/
  cp $VERBOSITY -R $BASEDIR/locales $SRCDIR/
  find $SRCDIR -name ".svn" -exec rm -rf {} \  > /dev/null    # remove SVN directories
  find $SRCDIR -name ".directory" -exec rm -rf {} \ > /dev/null #remove .directory files
  cd $SRCDIR/..
  echo "creating source tarball $RELEASENAME-src.tar.gz"
  tar -cvzf ../$RELEASENAME-src.tar.gz $RELEASENAME-src > /dev/null
  if [ $? -eq 0 ] ; then
     echo "source tarball created successfully"
  else
      echo "ERROR: tar returned error number $? when creating source tarball"
  fi
fi

echo "Finished all"
