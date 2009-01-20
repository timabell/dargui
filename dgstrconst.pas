unit dgStrConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gettext, translations, darintf;
  
procedure SetupLanguage;

var
  MonthNames: array[1..12] of string;
  WeekdayNames: array[1..7] of string;

resourcestring
rsErrScriptNotSetUp = 'Unable to schedule script execution.%sError when '
  +'executing at: %s%s';
//Time
rsJan = 'Jan';
rsFeb = 'Feb';
rsMar = 'Mar';
rsApr = 'Apr';
rsMay = 'May';
rsJun = 'Jun';
rsJul = 'Jul';
rsAug = 'Aug';
rsSep = 'Sep';
rsOct = 'Oct';
rsNov = 'Nov';
rsDec = 'Dec';

rsMon = 'Mon';
rsTue = 'Tue';
rsWed = 'Wed';
rsThu = 'Thu';
rsFri = 'Fri';
rsSat = 'Sat';
rsSun = 'Sun';

// Errors
rsErrDarNotFound = 'Unable to locate dar executable: nothing will work!';
rsErrRestoreAborted = 'Operation aborted: Restore selected files';
rsErrNoFilesSelected = 'Error: no files were selected';
rsErrNoXterm = 'Unable to find xterm: some features disabled';
rsErrNoBash = 'Unable to find bash: some features disabled';
rsErrNoXtermBash = 'Unable to find xterm or bash: some features disabled';
rsErrWritingScript = 'Error writing script';


// Batch File Comments
rsNotCompressThese = '# Do not compress these files';
rsNotCompressSmaller = '# Do not compress files smaller than this';
rsDARBatchFile = '# DAR batch file written by DarGUI';
rsDryRun = '# Dry run: this archive will not be written to file unless the next option is removed';
rsIncDirectories = '# Directories to include in archive';
rsIncFiles = '# Files to include in archive';
rsExclDirectories = '# Directories to exclude from archive';
rsExclFiles = '# Files to exclude from archive';
rsUseGzipCompr = '# Use gzip compression';
rsUseBzip2Comp = '# Use bzip2 compression';
rsNotReadDARcfg = '# Do not read DAR configuration files (~/.darrc or /etc/darrc)';
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
rsScriptExecuteTime = 'DAR script will be executed at %s:%s on %s';
rsQueryExecuteScript = 'A Dar backup script has been saved to %s%s%sDo you '
  +'want to execute the script now?';
rsScriptSetUpOK = 'Script successfully scheduled for execution';


// Dialog titles etc.
rsOpenExisting = 'Open existing archive';
rsFilterDARArchives = 'DAR archives';
rsSelectReferenceArch = 'Select reference archive';
rsFilterDarGUIFiles = 'DarGUI files';
rsFCptSelectbyFilter = 'Select using filters...';

//Common Interface
rsButtonAdd = 'Add';
rsNewArchive = 'New archive';
rsInvalidInput = 'Invalid input';
rsErrDateCannotBeInPast = 'Date cannot be in the past';
rsButtonCancel = 'Cancel';
rsEnterPassword = 'Enter Password';
rsIncorrectPasswordEnt = 'Incorrect password entered';
rsTheFileIsEncryptedEn = 'The file %s is encrypted. %sEnter the correct password for this archive.';
rsSourceArchive = 'Source archive';
rsSaveCatalogueAs = 'Save catalogue as';
rsButtonBrowse = 'Browse';
rsButtonAddFile = 'Add file';
rsMessUnableToFind1Dar = 'Unable to find %s.1.dar';
rsErrErrorNotFound = 'Error: %s not found';
rsCptComparingFiles = 'comparing files...';
rsInodesCheckedDiffere = '%s inodes checked : %s differences found';
rsMessNoStatisticsAvailabl = 'No statistics available - operation aborted?';
rsButtonRemove = 'Remove';
rsArchiveBaseName = 'Archive base name';
rsButtonClear = 'Clear';
rsButtonOK = 'OK';
rsCreateArchiveImmedia = 'Create archive immediately';
rsOpenArchiveWhenCompl = 'Open archive when created';
rsEncryptArchive = 'Encrypt archive using blowfish';
rsButtonClose = 'Close';
rsScript = 'Script';
rsTime = 'Time';
rsOptions2 = ' Options ';

//IsloateForm
rsCptIsolateArchiveCatalo = 'Isolate archive catalogue';

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
rsCheckArchiveForErrors = 'Check the archive for errors';
rsMenuHelp = 'Help';
rsMenuHelpAbout = 'About DarGUI';
rsVersionNumber = 'Version number: %s';
rsSVNRevision = 'SVN Revision: %s';
rsAuthor = 'Author: %s';
rsLicenceGPL = 'Licence: GPL';
rsDarGUIHasBeenDevelop = 'DarGUI has been developed using Freepascal and the '
  +'Lazarus IDE';
rsMenuIsolateCatalogue = 'Isolate Catalogue';
rsMenuDarGUIHelp = 'DarGUI Help';
rsMenuOperationLogs = 'Operation Logs';
rsMenuCheckIntegrity = 'Check Integrity';
rsMenuScheduleManager = 'Schedule Manager';
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
rsMenuCompareFiles = 'Compare files';
rsColDate = 'Date';
rsHour = 'Hour';
rsMinutes = 'Minutes';
rsDayOfWeek = 'Day of week';
rsDayInMonth = 'Day';
rsMonth = 'Month';
rsCptSelectDate = 'Select date';
rsColSize = 'Size';
rsColUser = 'User';
rsColGroup = 'Group';
rsColStatus = 'Status';

//ArchiveForm
rsDirectoriesAreExcluded = 'These directories are already selected to be excluded from the archive. %sDo you really want to include them?';
rsButtonAddMask = 'Add mask';
rsDirectoriesAreIncluded = 'These directories are already selected to be included in the archive.%sDo you really want to exclude them?';
rsFilesAlreadyExcluded = 'These files are already selected to be excluded from the archive.%sDo you really want to include them?';
rsFilesAlreadyIncluded = 'These files are already selected to be included in the archive.%sDo you really want to exclude them?';
rsCptResolveConflict = 'Resolve conflict';
rsOverwriteExistingArc = 'Overwrite existing archive?';
rsErrInvalidArchiveName = 'Invalid archive name';
rsPasswordWarning = 'You have chosen to encrypt this scheduled backup.%sThe '
  +'password for the encrypted archive will be included in the backup script.%'
  +'sAre you sure that you want to do this?';
rsFilterAllFiles = 'All files';
rsErrInvalidDirectory = 'Invalid directory';
rsErrRefArchiveNotFound = 'Reference archive not found';
rsScriptWarning1 = 'The backup script will run every minute on the days '
  +'selected%sAre you sure that you want to do this?';
rsScriptWarning2 = 'The backup script will run every minute of every day%sAre '
  +'you sure that you want to do this?';
rsErrCompressionLevelMust = 'Compression level must be between 0 and 9';
rsErrInvalidFileSize = 'Invalid number for file size';
rsUseBzip2Compression = 'use bzip2 compression';
rsSliceSize = 'Slice size';
rsMb = 'Mb';
rsUseSlices = 'Use slices';
rsSaveScriptAs = 'Save script as';
rsPauseBetweenSlices = 'Pause between slices';
rsBatchFile = 'Batch file';
rsPreserveDirectories = 'Preserve directories';
rsTestRunOnly = 'Test run only';
rsCompression = 'Compression';
rsDoNotCompress = ' Do not compress ... ';
rsCompressionType = ' Compression type ';
rsUseGzipCompression = 'use gzip compression';
rsCompressionLevel = 'Compression level';
rsLowerSizeLimitForCom = 'Lower size limit for compression:';
rsAttemptToReadDARConf = 'Attempt to read DAR configuration files';
rsSaveArchiveIn = 'Save archive in';
rsIncludeDirectories = 'Include directories:';
rsExcludeDirectories = 'Exclude directories';
rsIncludeFiles = 'Include files:';
rsExcludeFiles = 'Exclude files:';
rsSelectFile = 'Select file';
rsOptions = 'Options';
rsDirectories = 'Directories';
rsFiles = 'Files';
rsScheduling = 'Scheduling';
rsIncludeTimeInName = 'Add timestamp to name';
rsDifferentialBackup = 'Differential backup';
rsReferenceArchive = 'Reference archive';

//SelectFilterForm
rsMatchAllFilters = 'Match all filters';
rsApplyFiltersToFilena = 'Apply filters to filenames only';
rsCptAddFilter = 'Add filter';
rsRemoveAllFilters = 'Remove all filters?';

// Filemaskdialog
rsFileMask = 'File mask';

//OpLogForm
rsCommands = 'Commands';
rsCptRepeatingAction = 'Repeating action...';

// ExtractSelected
rsRestoreFilesTo = 'Restore files to';
rsIgnoreDirectories = 'Ignore directories';
rsOverwriteExistingFil = 'Overwrite existing files';
rsExistingFiles = 'Existing files';
rsMessDirectoryNotFound = 'Destination directory not found';
rsMessInvalidDestinationDir = 'Invalid destination directory';

//DiffForm
rsArchive = 'Archive';
rsBaseDirectory = 'Base directory';
rsResults = 'Results';
rsVerboseOutput = 'Verbose output';

// Messages
rsMessBUSY = ' BUSY...';
rsErrUnableToFindArchive = 'Unable to find archive';
rsErrUnableToOpenArchive = 'Unable to open archive';
rsConfirmBeforeOverwri = 'Confirm before overwriting';
rsDoNotOverwriteFiles = 'Do not overwrite files';
rsCptAddFileMask = 'Add file mask';
rsYes = 'Yes';
rsNo = 'No';
rsMessInvalidArchiveName = 'Invalid archive name';
rsMessInvalidCatalogueName = 'Invalid catalogue name';
rsMessUnableToFindArchive = 'Unable to find archive ''%s''';
rsCptScheduledBackups = 'Scheduled backups';
rsErrDeletingCron = 'Error when deleting crontab entry: %s%s';
rsErrDeletingSched = 'Error when deleting scheduled backup';

implementation

procedure SetupLanguage;
var
  Lang: string;
  FallbackLang: string;
  PODirectory: string;
begin
  PODirectory := TOOLDIR + 'locales/';
  if DirectoryExists(PODirectory) then
     begin
      GetLanguageIDs(Lang, FallbackLang); // in unit gettext
      TranslateUnitResourceStrings('dgStrConst', PODirectory + 'dargui.%s.po', Lang, FallbackLang);
      TranslateUnitResourceStrings('LCLStrConsts', PODirectory + 'lclstrconsts.%s.po', Lang, FallbackLang);
     end;
  WeekdayNames[1] := rsMon;
  WeekdayNames[2] := rsTue;
  WeekdayNames[3] := rsWed;
  WeekdayNames[4] := rsThu;
  WeekdayNames[5] := rsFri;
  WeekdayNames[6] := rsSat;
  WeekdayNames[7] := rsSun;
  MonthNames[1] := rsJan;
  MonthNames[2] := rsFeb;
  MonthNames[3] := rsMar;
  MonthNames[4] := rsApr;
  MonthNames[5] := rsMay;
  MonthNames[6] := rsJun;
  MonthNames[7] := rsJul;
  MonthNames[8] := rsAug;
  MonthNames[9] := rsSep;
  MonthNames[10] := rsOct;
  MonthNames[11] := rsNov;
  MonthNames[12] := rsDec;
end;

end.

