unit dgStrConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
// Config file
rsCfgShowToolbar = 'ShowToolbar';
rsCfgUserPrefs = 'User Preferences';
rsCfgRecentFiles = 'Recent Files';
rsCfgRecentX = 'Recent';

// Errors
rsErrDarNotFound = 'Unable to locate dar executable: nothing will work!';
rsErrRestoreAborted = 'Operation aborted: Restore selected files';
rsErrNoFilesSelected = 'Error: no files were selected';
rsErrNoXterm = 'Unable to find xterm: some features disabled';
rsErrNoBash = 'Unable to find bash: some features disabled';
rsErrNoXtermBash = 'Unable to find xterm or bash: some features disabled';


// Batch File Comments
rsNotCompressThese = '# Do not compress these files';
rsNotCompressSmaller = '# Do not compress files smaller than this';
rsDARBatchFile = '# DAR batch file written by DarGUI';
rsDryRun = '# Dry run: ths archive will not be written to file unless the '
  +'next option is removed';
rsIncDirectories = '# Directories to include in archive';
rsIncFiles = '# Files to include in archive';
rsExclDirectories = '# Directories to exclude from archive';
rsExclFiles = '# Files to exclude from archive';
rsUseGzipCompr = '# Use gzip compression';
rsUseBzip2Comp = '# Use bzip2 compression';
rsNotReadDARcfg = '# Do not read DAR configuration files (~/.darrc or /etc/'
  +'darrc)';
rsPreserveDirs = '# Preserve ignored directory names';
rsCreateSlices = '# Create slices of size in Mb';
rsPauseBetween = '# Pause between slices';
rsFilesToRestore = '# Files to restore';

// Information
rsCptCreatingArchive = 'creating archive...';
rsStatusNodeSelect = ' %s node(s) selected';
rsInformationF = 'Information for archive: %s';
rsInfoFormLocation = 'Location: %s';
rsAboutNoDAR = '* No DAR executable found! *';
rsAboutDARVersion = 'Using DAR version %s';
rsCptIsolating = 'DarGUI: Isolating catalogue...';
rsCptRestoreNodes = 'Restore %s nodes...';
rsCptRestoringFiles = 'restoring files...';
rsCptRestoreSelected = 'Restore selected files';

// Dialog titles etc.
rsOpenExisting = 'Open existing archive';
rsFilterDARArchives = 'DAR archives';
rsFCptSelectbyFilter = 'Select using filters...';

// Mainform Interface
rsMenuSelectByFilter = 'Select by Filter';
rsMenuToggleSelect = 'Toggle Selection';
rsMenuShowToolbar = 'Show Toolbar';
rsMenuOpenRecent = 'Open Recent...';
rsMenuDarDocumentation = 'Dar Documentation';
rsHintCompareArchiveFile = 'Compare archive and filesystem';
rsHintOpenAnArchive = 'Open an archive';
rsHintCreateANewArchive = 'Create a new archive';
rsHintIsolateCatalogue = 'Isolate catalogue from archive';
rsMenuHelp = 'Help';
rsMenuIsolateCatalogue = 'Isolate Catalogue';
rsMenuOperationLogs = 'Operation Logs';
rsMenuInformation = 'Information';
rsMenuOptions = 'Options';
rsMenuFile = 'File';
rsColFileName = 'File';
rsMenuArchive = 'Archive';
rsMenuRestoreSelected = 'Restore Selected';
rsMenuRestoreAll = 'Restore All';
rsMenuExit = 'Exit';
rsMenuNew = 'New';
rsMenuOpen = 'Open';
rsColDate = 'Date';
rsColSize = 'Size';
rsColUser = 'User';
rsColGroup = 'Group';
rsColStatus = 'Status';

// Messages
rsMessBUSY = 'BUSY...';



implementation

end.

