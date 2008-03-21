unit dgStrConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
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
rsCptDarGUICheckingArchive = 'DarGUI: checking archive integrity...';

// Dialog titles etc.
rsOpenExisting = 'Open existing archive';
rsFilterDARArchives = 'DAR archives';
rsFCptSelectbyFilter = 'Select using filters...';

//Common Interface
rsButtonAdd = 'Add';
rsButtonCancel = 'Cancel';
rsButtonRemove = 'Remove';
rsButtonClear = 'Clear';
rsButtonOK = 'OK';

// Mainform
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
rsMenuHelpAbout = 'About DarGUI';
rsMenuIsolateCatalogue = 'Isolate Catalogue';
rsMenuOperationLogs = 'Operation Logs';
rsMenuCheckIntegrity = 'Check Integrity';
rsMenuInformation = 'Information';
rsMenuOptions = 'Options';
rsMenuFile = 'File';
rsColFileName = 'File';
rsMenuArchive = 'Archive';
rsMenuRestoreSelected = 'Restore Selected';
rsMenuRestoreAll = 'Restore All';
rsMenuExit = 'Exit';
rsMenuHideAll = 'Hide All';
rsMenuShowAll = 'Show All';
rsMenuNew = 'New';
rsMenuOpen = 'Open';
rsMenuShowSelected = 'Show Selected';
rsColDate = 'Date';
rsColSize = 'Size';
rsColUser = 'User';
rsColGroup = 'Group';
rsColStatus = 'Status';

//ArchiveForm
rsDirectoriesAreExcluded = 'These directories are already selected to be '
  +'excluded from the archive. %sDo you really want to include them?';
rsDirectoriesAreIncluded = 'These directories are already selected to be '
  +'included in the archive.%sDo you really want to exclude them?';
rsFilesAlreadyExcluded = 'These files are already selected to be excluded '
  +'from the archive.%sDo you really want to include them?';
rsFilesAlreadyIncluded = 'These files are already selected to be included in '
  +'the archive.%sDo you really want to exclude them?';

//SelectFilterForm
rsMatchAllFilters = 'Match all filters';
rsApplyFiltersToFilena = 'Apply filters to filenames only';
rsCptAddFilter = 'Add filter';


// Filemaskdialog
rsFileMask = 'File mask';


// ExtractSelected
rsRestoreFilesTo = 'Restore files to';
rsIgnoreDirectories = 'Ignore directories';
rsOverwriteExistingFil = 'Overwrite existing files';


// Messages
rsMessBUSY = 'BUSY...';
rsErrUnableToFindArchive = 'Unable to find archive';
rsErrUnableToOpenArchive = 'Unable to open archive';
rsConfirmBeforeOverwri = 'Confirm before overwriting';
rsDoNotOverwriteFiles = 'Do not overwrite files';
rsCptAddFileMask = 'Add file mask';

implementation

end.

