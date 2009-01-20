unit archive;

{$mode objfpc}{$H+}
{ TODO: This is a Lazarus feature: if you select 2 buttons, the Object Inspector does not allow you to change their Tag.
}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Calendar, EditBtn;

type

  { TArchiveForm }

  TArchiveForm = class ( TForm )
    EmptyDirCheck: TCheckBox;
    EncryptArchiveCheck: TCheckBox;
    OpenCreatedArchiveCheck: TCheckBox;
    IncludeDirsRadioButton: TRadioButton;
    ExcludeDirsRadioButton: TRadioButton;
    RepeatMonthLabel: TLabel;
    RepeatMonthDayLabel: TLabel;
    RepeatWeekDayLabel: TLabel;
    RunOncePanel: TPanel;
    RepeatPanel: TPanel;
    RepeatHourBox: TComboBox;
    RepeatMinuteBox: TComboBox;
    RepeatMonthBox: TComboBox;
    RepeatMonthDayBox: TComboBox;
    RepeatWeekDayBox: TComboBox;
    RunOnceDateEdit: TDateEdit;
    RunOnceDateLabel: TLabel;
    RunOnceHourBox: TComboBox;
    RunOnceHourLabel: TLabel;
    RepeatHourLabel: TLabel;
    RunOnceMinuteBox: TComboBox;
    RunOnceMinuteLabel: TLabel;
    RepeatMinuteLabel: TLabel;
    SaveButton: TSpeedButton;
    LoadButton: TSpeedButton;
    TimestampCheck: TCheckBox;
    DiffRefButton: TButton;
    DiffFileCheck: TCheckBox;
    CompressMasks: TButton;
    ExcludeFileMasks: TButton;
    IncludeFileMasks: TButton;
    BatchFileButton: TButton;
    BatchFileBox: TEdit;
    DiffReference: TLabeledEdit;
    RepeatRadioButton: TRadioButton;
    RunOnceRadioButton: TRadioButton;
    SchedulePage: TPage;
    PauseCheck: TCheckBox;
    BatchLabel: TLabel;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    ArchiveDirButton: TButton;
    AddIncludeFileButton: TButton;
    BaseDirButton: TButton;
    ArchiveDirectory: TEdit;
    BaseDirectory: TEdit;
    ArchiveName: TEdit;
    AddIncludeDirButton: TButton;
    Bzip2Check: TCheckBox;
    NowRadioButton: TRadioButton;
    SaveScriptCheckBox: TCheckBox;
    ScriptFileButton: TButton;
    ScriptFilenameBox: TEdit;
    SliceSize: TEdit;
    SliceSizeLabel: TLabel;
    SliceMbLabel: TLabel;
    SlicesCheck: TCheckBox;
    CompLwrLimitCombo: TComboBox;
    CompressionLevel: TEdit;
    DelCompressMaskButton: TButton;
    CompressionLwrLimit: TEdit;
    DryRunCheck: TCheckBox;
    CompressionPage: TPage;
    CompressionExceptions: TGroupBox;
    CompressionTypes: TGroupBox;
    GZipCheck: TCheckBox;
    CompLvlLabel: TLabel;
    CompSizeLabel: TLabel;
    NoCompressList: TListBox;
    ReadConfigCheck: TCheckBox;
    DelIncludeDirButton: TButton;
    DelExcludeFileButton: TButton;
    AddExcludeFileButton: TButton;
    DelIncludeFileButton: TButton;
    ArchiveBaseLabel: TLabel;
    BaseDirLabel: TLabel;
    SaveArchiveInLabel: TLabel;
    IncludeFilesLabel: TLabel;
    ExcludeFilesLabel: TLabel;
    IncludeDirectories: TListBox;
    IncludeFiles: TListBox;
    ExcludeFiles: TListBox;
    ArchiveNotebook: TNotebook;
    ArchivePage: TPage;
    OpenDialog: TOpenDialog;
    OptionsPage: TPage;
    DirectoriesPage: TPage;
    FilesPage: TPage;
    OKCancelPanel: TPanel;
    NoteBookPanel: TPanel;
    SaveDialog: TSaveDialog;
    IncludeDirList: TStringList;
    ExcludeDirList: TStringList;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure ArchiveNameExit ( Sender: TObject ) ;
    procedure CompressMasksClick ( Sender: TObject ) ;
    procedure AddIncludeDirButtonClick ( Sender: TObject ) ;
    procedure AddFileButtonClick ( Sender: TObject ) ;
    procedure ArchiveDirButtonClick ( Sender: TObject ) ;
    procedure BaseDirButtonClick ( Sender: TObject ) ;
    procedure BatchFileButtonClick ( Sender: TObject ) ;
    procedure CompressionLevelExit ( Sender: TObject ) ;
    procedure CompressionLwrLimitExit ( Sender: TObject ) ;
    procedure DelCompressMaskButtonClick ( Sender: TObject ) ;
    procedure DiffFileCheckChange ( Sender: TObject ) ;
    procedure DiffRefButtonClick ( Sender: TObject ) ;
    procedure DirectoryRBChange ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure LoadButtonClick ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
    procedure DelExcludeFileButtonClick ( Sender: TObject ) ;
    procedure DelIncludeDirButtonClick ( Sender: TObject ) ;
    procedure DelIncludeFileButtonClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure RunOnceDateEditAcceptDate ( Sender: TObject;
      var ADate: TDateTime; var AcceptDate: Boolean ) ;
    procedure SaveButtonClick ( Sender: TObject ) ;
    procedure SaveScriptCheckBoxClick ( Sender: TObject ) ;
    procedure ScheduleBoxExit ( Sender: TObject ) ;
    procedure ScheduleRadioChange ( Sender: TObject ) ;
    procedure ScriptFileButtonClick ( Sender: TObject ) ;
    procedure SlicesCheckClick ( Sender: TObject ) ;
    procedure ZipCheckChange ( Sender: TObject ) ;
    procedure InitialiseInterface;
  private
    { private declarations }
    LoadingSettings: Boolean;
    BackupNotes: string;
    BackupFilename: string;
    LastBackupTime: TDateTime;
    function CheckParameters: Boolean;
    function IsInBaseDirectory(aDir: string): Boolean;
    procedure ResolveConflicts( Sender, RefList: TObject; ConflictMessage: string );
  public
    { public declarations }
    function CreateBatchfile: Boolean;
    function CreateScript: Boolean;
    function GetUniqueScriptName(aDir: string): string;
    BatchFile: TStringList;
    ArchiveBaseName: string;
  end;

var
  ArchiveForm: TArchiveForm;

implementation

uses main, dgStrConst, darintf, filemaskdlg, prefs, fileconflict, backupsavedlg;

{ TArchiveForm }

procedure TArchiveForm.DelIncludeDirButtonClick ( Sender: TObject ) ;
var
  ix: LongInt;
begin
  ix := IncludeDirectories.ItemIndex;
  if IncludeDirectories.Count > 0 then
     IncludeDirectories.Items.Delete(IncludeDirectories.ItemIndex);
  TButton(Sender).Enabled := IncludeDirectories.Count > 0;
  if IncludeDirectories.Count > 0 then
     begin
       if ix > 0 then Dec(ix);
       IncludeDirectories.ItemIndex := ix;
     end;
end;

procedure TArchiveForm.DelIncludeFileButtonClick ( Sender: TObject ) ;
var
  ix: LongInt;
begin
  ix := IncludeFiles.ItemIndex;
  if IncludeFiles.Count > 0 then
     IncludeFiles.Items.Delete(IncludeFiles.ItemIndex);
  DelIncludeFileButton.Enabled := IncludeFiles.Count > 0;
  if IncludeFiles.Count > 0 then
       begin
         if ix > 0 then Dec(ix);
         IncludeFiles.ItemIndex := ix;
       end;
end;

procedure TArchiveForm.FormCreate ( Sender: TObject ) ;
var
  x: Integer;
  test: string;
begin
  ArchiveDirectory.Text := SysUtils.GetEnvironmentVariable('HOME');
  BaseDirectory.Text := SysUtils.GetEnvironmentVariable('HOME');
  SelectDirectoryDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  OpenDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  SaveDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  IncludeDirList := TStringList.Create;
  ExcludeDirList := TStringList.Create;
  for x := 0 to 23 do
      begin
        RunOnceHourBox.Items.Add(IntToStr(x));
        RepeatHourBox.Items.Add(IntToStr(x));
      end;
  RunOnceHourBox.ItemIndex := 0;
  RepeatHourBox.ItemIndex := 0;
  for x := 0 to 59 do
      begin
        RunOnceMinuteBox.Items.Add(IntToStr(x));
        RepeatMinuteBox.Items.Add(IntToStr(x));
      end;
  RunOnceHourBox.ItemIndex := 0;
  RunOnceMinuteBox.ItemIndex := 0;
  RunOnceDateEdit.Date := Date;
  for x := 1 to 31 do
      RepeatMonthDayBox.Items.Add(IntToStr(x));
  RepeatMonthDayBox.ItemIndex := 0;
  for x := 1 to 7 do
      RepeatWeekDayBox.Items.Add(WeekdayNames[x]);
  RepeatWeekDayBox.ItemIndex := 0;
  for x := 1 to 12 do
      RepeatMonthBox.Items.Add(MonthNames[x]);
  RepeatMonthBox.ItemIndex := 0;
  BatchFile := TStringList.Create;
  LoadingSettings := false;
  RunOnceRadioButton.Enabled := (ProcessRunning('atd'))
     and (ShellCommand('atq -V', test) = 0);
  RepeatRadioButton.Enabled :=  (ProcessRunning('cron'))
     and (ShellCommand('crontab -l', test) = 0);
  InitialiseInterface;
end;

procedure TArchiveForm.RunOnceDateEditAcceptDate ( Sender: TObject;
  var ADate: TDateTime; var AcceptDate: Boolean ) ;
begin
  if ADate < Date then
     begin
       ShowMessage ( rsErrDateCannotBeInPast ) ;
       AcceptDate := false;
       RunOnceDateEdit.Date := Now;
     end;
end;

procedure TArchiveForm.SaveButtonClick ( Sender: TObject ) ;
var
  SaveDlg: TCreateSaveDialog;
  SaveContent: TFileStream;
  datasize: smallint;
  datatype: TControlType;
  dataname: string;
  datatext: string;
  x: Integer;
  y: Integer;
begin
  SaveDlg := TCreateSaveDialog.Create(nil);
  SaveDlg.NotesMemo.Text := BackupNotes;
  SaveDlg.FilenameEdit.Text := BackupFilename;
  if SaveDlg <> nil then
     try
       if SaveDlg.ShowModal = mrOK then
          begin
            SaveContent := TFileStream.Create(SaveDlg.FilenameEdit.Text, fmCreate);
            datatext := 'DarGUI' + SVN_REVISION + #32;
            SaveContent.Write(datatext[1], Length(datatext));
            SaveContent.Write(LastBackupTime, SizeOf(LastBackupTime));
            datatext := SaveDlg.NotesMemo.Text;
            datasize := Length(datatext);
            SaveContent.Write(datasize, SizeOf(datasize));
            SaveContent.Write(datatext[1], datasize);
            for x := 0 to ComponentCount-1 do
                begin
                  datatype := ctNone;
                  if Components[x] is TEdit then
                     if TEdit(Components[x]) <> BatchFileBox then
                       begin
                         datatype := ctEdit;
                         dataname := TEdit(Components[x]).Name;
                         datatext := TEdit(Components[x]).Text;
                       end;
                  if Components[x] is TComboBox then
                     begin
                       datatype := ctCombobox;
                       dataname := TComboBox(Components[x]).Name;
                       datatext := IntToStr(TComboBox(Components[x]).ItemIndex);
                     end;
                  if Components[x] is TCheckBox then
                     begin
                       datatype := ctCheckbox;
                       dataname := TCheckBox(Components[x]).Name;
                       if TCheckBox(Components[x]).Checked
                          then datatext := '1'
                          else datatext := '0';
                     end;
                  if Components[x] is TRadioButton then
                     begin
                       datatype := ctRadiobutton;
                       dataname := TRadioButton(Components[x]).Name;
                       if TRadioButton(Components[x]).Checked
                          then datatext := '1'
                          else datatext := '0';
                     end;
                  if Components[x] is TDateEdit then
                     begin
                       datatype := ctDateEdit;
                       dataname := TDateEdit(Components[x]).Name;
                       datatext := FloatToStr(TDateEdit(Components[x]).Date);
                     end;
                  if Components[x] is TListBox then
                     begin
                       datatype := ctListBox;
                       dataname := TListBox(Components[x]).Name;
                       datatext := IntToStr(TListBox(Components[x]).Count);
                    end;
                  if datatype <> ctNone then
                     begin
                       SaveContent.Write(datatype, Sizeof(datatype));
                       datasize := Length(dataname);
                       SaveContent.Write(datasize, SizeOf(datasize));
                       SaveContent.Write(dataname[1], datasize);
                       datasize := Length(datatext);
                       SaveContent.Write(datasize, SizeOf(datasize));
                       SaveContent.Write(datatext[1], datasize);
                       if datatype=ctListBox then
                          for y := 0 to TListBox(Components[x]).Count-1 do
                              begin
                                datatext := TListBox(Components[x]).Items[y];
                                datasize := Length(datatext);
                                SaveContent.Write(datasize, SizeOf(datasize));
                                SaveContent.Write(datatext[1], datasize);
                              end;
                     end;
              end;
            SaveContent.Free;
          end;
     finally
       SaveDlg.Free;
     end;
end;

procedure TArchiveForm.SaveScriptCheckBoxClick ( Sender: TObject ) ;
begin
  ScriptFilenameBox.Enabled := SaveScriptCheckBox.Checked;
  ScriptFileButton.Enabled := SaveScriptCheckBox.Checked;
end;

procedure TArchiveForm.ScheduleBoxExit ( Sender: TObject ) ;
begin
  with Sender as TComboBox do
       begin
         if TComboBox(Sender).Items.IndexOf(TComboBox(Sender).Text) > -1
            then TComboBox(Sender).ItemIndex := TComboBox(Sender).Items.IndexOf(TComboBox(Sender).Text)
         else begin
                ShowMessage(rsInvalidInput);
                TComboBox(Sender).SetFocus;
              end;
       end;
end;

procedure TArchiveForm.ScheduleRadioChange ( Sender: TObject ) ;
    procedure EnablePanel( Panel: TPanel; Setting: Boolean );
    var
      x: Integer;
    begin
      with Panel as TPanel do
             for x := 0 to Panel.ControlCount-1 do
                 TWinControl(Panel.Controls[x]).Enabled := Setting;
    end;
begin
  EnablePanel(RunOncePanel, RunOnceRadioButton.Checked);
  EnablePanel(RepeatPanel, RepeatRadioButton.Checked);
end;

procedure TArchiveForm.ScriptFileButtonClick ( Sender: TObject ) ;
begin
  if SaveDialog.Execute
     then ScriptFilenameBox.Text := SaveDialog.FileName;
end;

procedure TArchiveForm.SlicesCheckClick ( Sender: TObject ) ;
begin
  SliceSizeLabel.Enabled :=  TCheckBox(Sender).Checked;
  SliceSize.Enabled :=  TCheckBox(Sender).Checked;
  SliceMbLabel.Enabled :=  TCheckBox(Sender).Checked;
  PauseCheck.Enabled := TCheckBox(Sender).Checked;
end;

procedure TArchiveForm.ZipCheckChange ( Sender: TObject ) ;
begin
  if Sender=GZipCheck then
      if TCheckBox(Sender).Checked then Bzip2Check.Checked := false;
  if Sender=Bzip2Check then
     if TCheckBox(Sender).Checked then GZipCheck.Checked := false;
  CompressionLevel.Enabled := ( GZipCheck.Checked or Bzip2Check.Checked);
  CompLvlLabel.Enabled := CompressionLevel.Enabled;
end;

procedure TArchiveForm.InitialiseInterface;
begin
  Caption := rsNewArchive;
  CompressMasks.Caption := rsButtonAdd;
  ExcludeFileMasks.Caption := rsButtonAddMask;
  IncludeFileMasks.Caption := rsButtonAddMask;
  BatchFileButton.Caption := rsButtonBrowse;
  PauseCheck.Caption := rsPauseBetweenSlices;
  BatchLabel.Caption :=   rsBatchFile;
  ScriptFileButton.Caption := rsButtonBrowse;
  SaveScriptCheckBox.Caption := rsSaveScriptAs;
  OKButton.Caption := rsButtonOK;
  CancelButton.Caption := rsButtonCancel;
  ArchiveDirButton.Caption := rsButtonBrowse;
  AddIncludeFileButton.Caption := rsButtonAddFile;
  BaseDirButton.Caption := rsButtonBrowse;
  AddIncludeDirButton.Caption := rsButtonAdd;
  Bzip2Check.Caption := rsUseBzip2Compression;
  SliceSizeLabel.Caption := rsSliceSize;
  SliceMbLabel.Caption := rsMb;
  SlicesCheck.Caption := rsUseSlices;
  DelCompressMaskButton.Caption := rsButtonRemove;
  EmptyDirCheck.Caption := rsPreserveDirectories;
  DryRunCheck.Caption := rsTestRunOnly;
  CompressionPage.Caption := rsCompression;
  CompressionExceptions.Caption := rsDoNotCompress;
  CompressionTypes.Caption := rsCompressionType;
  GZipCheck.Caption := rsUseGzipCompression;
  CompLvlLabel.Caption := rsCompressionLevel;
  CompSizeLabel.Caption := rsLowerSizeLimitForCom;
  ReadConfigCheck.Caption := rsAttemptToReadDARConf;
  DelIncludeDirButton.Caption := rsButtonRemove;
  DelExcludeFileButton.Caption := rsButtonRemove;
  AddExcludeFileButton.Caption := rsButtonAddFile;
  DelIncludeFileButton.Caption := rsButtonRemove;
  ArchiveBaseLabel.Caption := rsArchiveBaseName;
  BaseDirLabel.Caption := rsBaseDirectory;
  SaveArchiveInLabel.Caption := rsSaveArchiveIn;
  IncludeDirsRadioButton.Caption := rsIncludeDirectories;
  ExcludeDirsRadioButton.Caption := rsExcludeDirectories;
  IncludeFilesLabel.Caption := rsIncludeFiles;
  ExcludeFilesLabel.Caption := rsExcludeFiles;
  ArchivePage.Caption := rsArchive;
  OpenDialog.Title := rsSelectFile;
  OptionsPage.Caption := rsOptions;
  DirectoriesPage.Caption := rsDirectories;
  FilesPage.Caption := rsFiles;
  SchedulePage.Caption := rsScheduling;
  TimestampCheck.Caption := rsIncludeTimeInName;
  DiffFileCheck.Caption := rsDifferentialBackup;
  DiffReference.EditLabel.Caption := rsReferenceArchive;
  DiffRefButton.Caption := rsButtonBrowse;
  RunOnceDateLabel.Caption := rsColDate;
  RunOnceHourLabel.Caption := rsHour;
  RunOnceMinuteLabel.Caption := rsMinutes;
  RepeatHourLabel.Caption := rsHour;
  RepeatMinuteLabel.Caption := rsMinutes;
  RepeatWeekDayLabel.Caption := rsDayOfWeek;
  RepeatMonthDayLabel.Caption := rsDayInMonth;
  RepeatMonthLabel.Caption := rsMonth;
  RunOnceDateEdit.DialogTitle := rsCptSelectDate;
  RunOnceDateEdit.CancelCaption := rsButtonCancel;
  RunOnceDateEdit.OKCaption := rsButtonOK;
  NowRadioButton.Caption := rsCreateArchiveImmedia;
  OpenCreatedArchiveCheck.Caption := rsOpenArchiveWhenCompl;
  EncryptArchiveCheck.Caption := rsEncryptArchive;
end;

procedure TArchiveForm.AddIncludeDirButtonClick ( Sender: TObject ) ;
var
  LB: TListBox;
  ConflictList: TListBox;
  ConflictMessage: string;
  DelBn: TButton;
  x: Integer;
begin
  SelectDirectoryDialog.Options := SelectDirectoryDialog.Options+[ofAllowMultiSelect];
  if Pos(BaseDirectory.Text, SelectDirectoryDialog.InitialDir) <> 1 then
     SelectDirectoryDialog.InitialDir := BaseDirectory.Text;
  if SelectDirectoryDialog.Execute then
    //if IsInBaseDirectory(SelectDirectoryDialog.FileName) then
      begin
        LB := IncludeDirectories;
        DelBn := DelIncludeDirButton;
        for x := 0 to SelectDirectoryDialog.Files.Count -1 do
            if LB.Items.IndexOf(SelectDirectoryDialog.Files[x]) < 0
               then LB.Items.Add(SelectDirectoryDialog.Files[x]);
        DelBn.Enabled := true;
      end;
end;

procedure TArchiveForm.AddFileButtonClick ( Sender: TObject ) ;
var
  LB: TListBox;
  DelBn: TButton;
  ConflictList: TListBox;
  ConflictMessage: string;
  x: Integer;
  fn: String;
begin
  OpenDialog.Options := OpenDialog.Options+[ofAllowMultiSelect];
  if Pos(BaseDirectory.Text, OpenDialog.InitialDir) <> 1 then
     OpenDialog.InitialDir := BaseDirectory.Text;
  if OpenDialog.Execute then
      begin
        case TButton(Sender).Tag of
           0: begin LB := IncludeFiles;
                    ConflictList := ExcludeFiles;
                    ConflictMessage := Format ( rsFilesAlreadyExcluded, [ #10 ] ) ;
                    DelBn := DelIncludeFileButton;
                    end;
           1: begin LB := ExcludeFiles;
                    ConflictList := IncludeFiles;
                    ConflictMessage := Format ( rsFilesAlreadyIncluded, [ #10 ] ) ;
                    DelBn := DelExcludeFileButton;
                    end;
           end;
        ResolveConflicts(LB, ConflictList, ConflictMessage );
        for x := 0 to OpenDialog.Files.Count -1 do
            begin
              fn := ExtractFileName( OpenDialog.Files[x] );
              if LB.Items.IndexOf( fn ) < 0
                  then LB.Items.Add( fn );
            end;
        DelBn.Enabled := true;
      end;
end;

procedure TArchiveForm.CompressMasksClick ( Sender: TObject ) ;
var
  recentmasks: String;
  newmask: string;
begin
  recentmasks := Preferences.ReadString(CfgUserPrefs,TButton(Sender).Name,'') ;
  FileMaskDialog.PopulateFilterList(recentmasks);
  FileMaskDialog.Caption := rsCptAddFileMask;
  FileMaskDialog.SetPosition(Self);
  if FileMaskDialog.ShowModal = mrOk then
     if FileMaskDialog.FileMask.Text <> '' then
       begin
        newmask := Trim(FileMaskDialog.FileMask.Text);
        case TComponent(Sender).Tag of
            0: if NoCompressList.Items.IndexOf(newmask) < 0 then
               begin
                 NoCompressList.Items.Add(newmask);
                 NoCompressList.ItemIndex := NoCompressList.Items.Count-1;
                 DelCompressMaskButton.Enabled := true;
               end;
            1: if IncludeFiles.Items.IndexOf(newmask) < 0 then
               begin
                 IncludeFiles.Items.Add(newmask);
                 IncludeFiles.ItemIndex := IncludeFiles.Items.Count-1;
                 DelIncludeFileButton.Enabled := true;
               end;
            2: if ExcludeFiles.Items.IndexOf(newmask) < 0 then
               begin
                 ExcludeFiles.Items.Add(newmask);
                 ExcludeFiles.ItemIndex := ExcludeFiles.Items.Count-1;
                 DelExcludeFileButton.Enabled := true;
               end;
         end;  //case
         if FileMaskDialog.Masks <> ''
          then Preferences.WriteString(CfgUserPrefs,TButton(Sender).Name, FileMaskDialog.Masks);
       end;
end;

procedure TArchiveForm.ArchiveNameExit ( Sender: TObject ) ;
var
  Archname: string;
  x: Integer;
begin
  Archname := TEdit(Sender).Text;
  for x := 1 to Length(Archname) do
      if Archname[x]=#32 then Archname[x] := '_';
  TEdit(Sender).Text := Archname;
end;

procedure TArchiveForm.ArchiveDirButtonClick ( Sender: TObject ) ;
begin
  SelectDirectoryDialog.Options := SelectDirectoryDialog.Options-[ofAllowMultiSelect];
  SelectDirectoryDialog.InitialDir := ArchiveDirectory.Text;
  if SelectDirectoryDialog.Execute
     then ArchiveDirectory.Text := SelectDirectoryDialog.FileName;
end;

procedure TArchiveForm.BaseDirButtonClick ( Sender: TObject ) ;
begin
  SelectDirectoryDialog.Options := SelectDirectoryDialog.Options-[ofAllowMultiSelect];
  SelectDirectoryDialog.InitialDir := BaseDirectory.Text;
  if SelectDirectoryDialog.Execute
     then BaseDirectory.Text := SelectDirectoryDialog.FileName;
end;

procedure TArchiveForm.BatchFileButtonClick ( Sender: TObject ) ;
begin
  SaveDialog.FileName := BatchFileBox.Text;
  if SaveDialog.Execute
     then BatchFileBox.Text := SaveDialog.FileName;
end;

procedure TArchiveForm.CompressionLevelExit ( Sender: TObject ) ;
begin
  if not isInteger(CompressionLevel.Text) then
     begin
     ShowMessage ( rsErrCompressionLevelMust ) ;
     CompressionLevel.Text := '9';
     CompressionLevel.SetFocus;
     end;
end;

procedure TArchiveForm.CompressionLwrLimitExit ( Sender: TObject ) ;
begin
  if (not isInteger(TEdit(Sender).Text))
  or (TEdit(Sender).Text='') then
     begin
     ShowMessage ( rsErrInvalidFileSize ) ;
     TEdit(Sender).SetFocus;
     end;
end;

procedure TArchiveForm.DelCompressMaskButtonClick ( Sender: TObject ) ;
var
  ix: LongInt;
begin
  ix := NoCompressList.ItemIndex;
  if NoCompressList.Count > 0  then
     if ix > -1 then
     begin
     NoCompressList.Items.Delete(NoCompressList.ItemIndex);
     DelCompressMaskButton.Enabled := NoCompressList.Count > 0;
     if NoCompressList.Count> 0 then
        begin
          if ix > 0 then Dec(ix);
          NoCompressList.ItemIndex := ix;
        end;
     end;
end;

procedure TArchiveForm.DiffFileCheckChange ( Sender: TObject ) ;
begin
  DiffReference.Enabled := DiffFileCheck.Checked;
  DiffRefButton.Enabled := DiffFileCheck.Checked;
end;

procedure TArchiveForm.DiffRefButtonClick ( Sender: TObject ) ;
begin
  OpenDialog.Filter := rsFilterDARArchives + '|*.1.dar';
  OpenDialog.FileName := DiffReference.Text;
  OpenDialog.Title := rsSelectReferenceArch;
  if OpenDialog.Execute
     then DiffReference.Text := OpenDialog.FileName;
end;

procedure TArchiveForm.DirectoryRBChange ( Sender: TObject ) ;
begin
 if LoadingSettings then exit;
 if TRadioButton(Sender).Checked then
    begin
      ExcludeDirList.Assign(IncludeDirectories.Items);
      IncludeDirectories.Items.Assign(IncludeDirList);
    end
 else
    begin
      IncludeDirList.Assign(IncludeDirectories.Items);
      IncludeDirectories.Items.Assign(ExcludeDirList);
    end;
 DelIncludeDirButton.Enabled := IncludeDirectories.Count>0;
end;

procedure TArchiveForm.FormDestroy ( Sender: TObject ) ;
begin
  BatchFile.Free;
  IncludeDirList.Free;
  ExcludeDirList.Free;
end;

procedure TArchiveForm.LoadButtonClick ( Sender: TObject ) ;
var
  SavedSettings: TFileStream;
  datasize: smallint;
  datatype: TControlType;
  dataname: string;
  datatext: string;
  x: Integer;
  bytesread: LongInt;
  p: Integer;
  controlindex: LongInt;
  
  function GetComponentByName( cn: string ): integer;
  var
    c: integer;
  begin
    Result := -1;
    for c := 0 to ComponentCount-1 do
        if Components[c] is TWinControl then
           if TWinControl(Components[c]).Name = cn
              then Result := c;
  end;
  
  function GetDataChunk: string;
  var
    datalength: smallint;
    databuffer: string;
  begin
   Result := '';
   SavedSettings.Read(datalength, SizeOf(datalength));
   SetLength(databuffer, datalength);
   SavedSettings.Read(databuffer[1], datalength);
   Result := databuffer;
  end;

begin
  OpenDialog.Filter := rsFilterDarGUIFiles + '|*.dargui|' + rsFilterAllFiles + '|*';
  OpenDialog.FilterIndex := 0;
  if OpenDialog.Execute then
     begin
       bytesread := 0;
       SavedSettings := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
       SetLength(datatext, 6);
       bytesread := bytesread + SavedSettings.Read(datatext[1], 6);
       if datatext = 'DarGUI' then
          try
            LoadingSettings := true;
            p := 1;
            SetLength(datatext, p);
            bytesread := bytesread + SavedSettings.Read(datatext[p], 1);
            while (datatext[p] <> #32) and (bytesread < SavedSettings.Size) do
                  begin
                    Inc(p);
                    SetLength(datatext, p);
                    bytesread := bytesread + SavedSettings.Read(datatext[p], 1);
                  end;
            if TryStrToInt(Trim(datatext), p) then
               begin
                 SavedSettings.Read(LastBackupTime, SizeOf(LastBackupTime));
                 SavedSettings.Read(datasize,SizeOf(datasize));
                 if datasize > 0 then
                    begin
                      SetLength(BackupNotes, datasize);
                      SavedSettings.Read(BackupNotes[1], datasize);
                    end;
                 while SavedSettings.Position < SavedSettings.Size do
                   begin
                     SavedSettings.Read(datatype, Sizeof(datatype));
                     dataname := GetDataChunk;
                     datatext := GetDataChunk;
                     controlindex := GetComponentByName(dataname);
                     if controlindex > -1 then
                        case datatype of
                             ctEdit:        TEdit(Components[controlindex]).Text := datatext;
                             ctCombobox:    TComboBox(Components[controlindex]).ItemIndex := StrToInt(datatext);
                             ctDateEdit:    TDateEdit(Components[controlindex]).Date := StrToFloat(datatext);
                             ctRadiobutton: TRadioButton(Components[controlindex]).Checked := datatext = '1';
                             ctCheckbox:    TCheckBox(Components[controlindex]).Checked := datatext = '1';
                             ctListBox:     begin
                                              p := StrToInt(datatext);
                                              TListBox(Components[controlindex]).Clear;
                                              for x := 1 to p do
                                                  begin
                                                   datatext := GetDataChunk;
                                                   TListBox(Components[controlindex]).Items.Add(datatext);
                                                  end;
                                            end;
                             end;
                   end;
               end;
          finally
            SavedSettings.Free;
            BackupFilename := OpenDialog.FileName;
            DelIncludeDirButton.Enabled := IncludeDirectories.Count > 0;
            DelIncludeFileButton.Enabled := IncludeFiles.Count > 0;
            DelExcludeFileButton.Enabled := ExcludeFiles.Count > 0;
            DelCompressMaskButton.Enabled := NoCompressList.Count > 0;
            LoadingSettings := false;
          end;
     end;
  OpenDialog.Filter := rsFilterAllFiles + '|*';
end;

procedure TArchiveForm.OKButtonClick ( Sender: TObject ) ;
var
  isScript: Boolean;
begin
  if CheckParameters then
     begin
       isScript := SaveScriptCheckBox.Checked or (not NowRadioButton.Checked);
       ArchiveBaseName := ArchiveName.Text;
       if ( TimestampCheck.Checked ) and ( not isScript ) // WriteScript in main.pas takes care of scripts
          then ArchiveBaseName := ArchiveBaseName + FormatDateTime('_yyyymmddhhnn', Now);
       ModalResult := mrOk;
     end;
end;

procedure TArchiveForm.DelExcludeFileButtonClick ( Sender: TObject ) ;
var
  ix: LongInt;
begin
  ix := ExcludeFiles.ItemIndex;
  if ExcludeFiles.Count > 0 then
     if ExcludeFiles.ItemIndex > -1 then
        ExcludeFiles.Items.Delete(ExcludeFiles.ItemIndex);
  DelExcludeFileButton.Enabled := ExcludeFiles.Count > 0;
  if ExcludeFiles.Count > 0 then
     begin
       if ix > 0 then Dec(ix);
       ExcludeFiles.ItemIndex := ix;
     end;
end;

function TArchiveForm.CheckParameters: Boolean;
var
  x: Integer;
begin        //TODO: check that script is not to be created within base directory
  Result := true;
  if not NowRadioButton.Checked then
     if EncryptArchiveCheck.Checked then
         if MessageDlg(Format(rsPasswordWarning, [#10, #10]), mtConfirmation,
           mbYesNo, 0) = mrNo
                then begin
                       ArchiveNotebook.PageIndex := 4;
                       EncryptArchiveCheck.SetFocus;
                       Result := false;
                       exit;
                     end;
  if ArchiveName.Text = '' then
     begin
       ShowMessage ( rsErrInvalidArchiveName ) ;
       ArchiveNotebook.PageIndex := 0;
       ArchiveName.SetFocus;
       Result := false;
       exit;
     end;
  if ArchiveDirectory.Text[Length(ArchiveDirectory.Text)] <> DirectorySeparator
     then ArchiveDirectory.Text := ArchiveDirectory.Text + DirectorySeparator;
  if BaseDirectory.Text[Length(BaseDirectory.Text)] <> DirectorySeparator
     then BaseDirectory.Text := BaseDirectory.Text + DirectorySeparator;
  if not FileExists(ArchiveDirectory.Text) then
     begin
       ShowMessage ( rsErrInvalidDirectory ) ;
       ArchiveNotebook.PageIndex := 0;
       ArchiveDirectory.SetFocus;
       Result := false;
       exit;
     end;
  if not FileExists(BaseDirectory.Text)  then
     begin
       ShowMessage ( rsErrInvalidDirectory ) ;
       ArchiveNotebook.PageIndex := 0;
       BaseDirectory.SetFocus;
       Result := false;
       exit;
     end;
  if not (TimestampCheck.Checked or RepeatRadioButton.Checked) then
    begin
     if FileExists(ArchiveDirectory.Text
                          + ArchiveName.Text + '.1.dar')
        then if MessageDlg ( rsOverwriteExistingArc, mtWarning, [ mbYes, mbNo
          ] , 0 ) = mrNo
             then begin
                    Result := false;
                    ArchiveNotebook.PageIndex := 0;
                    ArchiveName.SetFocus;
                    exit;
                  end
             else DeleteFilesByMask(ArchiveDirectory.Text
                            + ArchiveName.Text + '.*.dar');
    end;
  if DiffFileCheck.Checked
     then if not FileExists(DiffReference.Text) then
           begin
             ShowMessage ( rsErrRefArchiveNotFound ) ;
             ArchiveNotebook.PageIndex := 0;
             DiffReference.SetFocus;
             Result := false;
             exit;
           end;
  if CompLwrLimitCombo.ItemIndex< 0 then CompLwrLimitCombo.ItemIndex := 0;
  for x := 0 to IncludeDirectories.Count-1 do
      if IncludeDirectories.Items[x] = BaseDirectory.Text
         then IncludeDirectories.Items.Delete(x);
  if RepeatRadioButton.Checked then
     begin
       if (RepeatMinuteBox.ItemIndex < 1)
           and (RepeatHourBox.ItemIndex < 1)
           and (RepeatMonthBox.ItemIndex < 1)
           and (RepeatMonthDayBox.ItemIndex < 1)
           and (RepeatWeekDayBox.ItemIndex < 1) then
              begin
                if MessageDlg(Format(rsScriptWarning2, [#10]), mtWarning,
                  mbYesNo, 0) = mrNo
                then begin
                       ArchiveNotebook.PageIndex := 5;
                       RepeatMinuteBox.SetFocus;
                       Result := false;
                       exit;
                     end;
               end
          else
               if RepeatMinuteBox.ItemIndex < 1 then
                  if MessageDlg(Format(rsScriptWarning1, [#10]), mtWarning,
                    mbYesNo, 0) = mrNo
                  then begin
                         ArchiveNotebook.PageIndex := 5;
                         RepeatMinuteBox.SetFocus;
                         Result := false;
                         exit;
                       end;
     end;
end;

function TArchiveForm.IsInBaseDirectory(aDir: string): Boolean;
begin
  Result := true;
  if Pos( BaseDirectory.Text, aDir) <> 1 then
     begin
     Result := false;
     ShowMessage('Unable to add directory' + #13#10
                         + 'Not in Base Directory');
     end;
end;

function TArchiveForm.GetUniqueScriptName ( aDir: string ) : string;
var
  counter: Integer;
  script: String;
begin
    Result := '';
    counter := 1;
    script := aDir + ArchiveForm.ArchiveBaseName + '.sh';
    if FileExists(script) then
       begin
         while FileExists(script + '.' + IntToStr(counter)) do
              Inc(counter);
           script := script + '.' + IntToStr(counter);
         end;
    Result := script;
end;

//TODO: simplify this code - now only needs to compare IncludeFiles and ExcludeFiles
procedure TArchiveForm.ResolveConflicts ( Sender, RefList: TObject;
  ConflictMessage: string ) ;
var
  LB: TListBox;
  RB : TListBox;
  x: Integer;
begin
  if not (Sender is TListBox) then exit;
  FileConflictForm := TFileConflictForm.Create(Self);
  FileConflictForm.Caption := rsCptResolveConflict;
  FileConflictForm.InstructionLabel.Caption := ConflictMessage;
  LB := TListBox(Sender);
  RB := TlistBox(RefList);
  for x := 0 to OpenDialog.Files.Count-1 do
      if RB.Items.IndexOf( ExtractFileName( OpenDialog.Files[x] ) ) > -1
         then FileConflictForm.FileListBox.Items.Add( ExtractFileName( OpenDialog.Files[x] ) );
  if FileConflictForm.FileListBox.Items.Count > 0 then
       if FileConflictForm.ShowModal = mrNo then
          begin
            for x := OpenDialog.Files.Count-1 downto 0 do
              if FileConflictForm.FileListBox.Items.IndexOf(ExtractFileName( OpenDialog.Files[x] )) > -1
                 then OpenDialog.Files.Delete(x);
          end
       else
          begin
            for x := RB.Items.Count-1 downto 0 do
              if FileConflictForm.FileListBox.Items.IndexOf(RB.Items[x]) > -1
                 then RB.Items.Delete(x);
          end;
  FileConflictForm.Free;
end;

function TArchiveForm.CreateBatchfile : Boolean;
var
  x: integer;

  procedure AddCompressionOptions;
  var
    x: integer;
    B: string[1];
  begin
  B := '';
  if ArchiveForm.NoCompressList.Count > 0 then
    begin
    BatchFile.Add('');
    BatchFile.Add ( rsNotCompressThese ) ;
    for x := 0 to NoCompressList.Count-1 do
        BatchFile.Add('-Z ' + NoCompressList.Items[x]);
    end;
  if CompLwrLimitCombo.ItemIndex > -1
     then B := CompLwrLimitCombo.Items[CompLwrLimitCombo.ItemIndex][1];
  if not ((CompressionLwrLimit.Text = '100') and (B = 'b')) then
     begin
       BatchFile.Add(#10 + rsNotCompressSmaller);
       BatchFile.Add('-m ' + CompressionLwrLimit.Text + B);
     end;
  end;

  function RemoveBaseDirectory(aFilePath: string): string;
  begin
    if Pos(BaseDirectory.Text, aFilePath) = 1 then
       begin
       Result := Copy(aFilePath, Length(BaseDirectory.Text)+1, READ_BYTES);
       end
       else
       begin
       Result := aFilePath;
       end;
  end;

begin
     BatchFile.Clear;
     BatchFile.Add ( rsDARBatchFile ) ;
     BatchFile.Add('-R "' + BaseDirectory.Text + '"' + #10);
     if DryRunCheck.Checked then
        begin
          BatchFile .Add ( rsDryRun ) ;
          BatchFile .Add('--empty' + #10);
        end;
     if IncludeDirectories.Count > 0 then
        begin
          if IncludeDirsRadioButton.Checked then
              begin
                BatchFile .Add('');
                BatchFile .Add ( rsIncDirectories ) ;
                for x := 0 to IncludeDirectories.Count-1 do
                    BatchFile.Add('-g "' + RemoveBaseDirectory(IncludeDirectories.Items[x]) + '"');
              end
          else
              begin
                BatchFile.Add('');
                BatchFile.Add ( rsExclDirectories ) ;
                for x := 0 to IncludeDirectories.Count-1 do
                    BatchFile.Add('-P "' + RemoveBaseDirectory(IncludeDirectories.Items[x]) + '"');
              end;
         end;
     if IncludeFiles.Count > 0 then
        begin
          BatchFile.Add('');
          BatchFile.Add ( rsIncFiles ) ;
          for x := 0 to IncludeFiles.Count-1 do
              BatchFile.Add('-I "' + RemoveBaseDirectory(IncludeFiles.Items[x]) + '"');
        end;
     if ExcludeFiles.Count > 0 then
        begin
          BatchFile.Add('');
          BatchFile.Add ( rsExclFiles ) ;
          for x := 0 to ExcludeFiles.Count-1 do
              BatchFile.Add('-X "' + RemoveBaseDirectory(ExcludeFiles.Items[x]) + '"');
        end;
     if GZipCheck.Checked
        then begin
               BatchFile.Add('');
               BatchFile.Add ( rsUseGzipCompr ) ;
               BatchFile.Add('--gzip=' + CompressionLevel.Text);
               AddCompressionOptions;
             end
     else if Bzip2Check.Checked
        then begin
               BatchFile.Add('');
               BatchFile.Add ( rsUseBzip2Comp ) ;
               BatchFile.Add('--bzip2=' + CompressionLevel.Text);
               AddCompressionOptions;
             end;
     if not ReadConfigCheck.Checked then
        begin
          BatchFile.Add ( rsNotReadDARcfg + #10 + '-N' + #10 ) ;
        end;
     if EmptyDirCheck.Checked then
        begin
          BatchFile.Add ( rsPreserveDirs + #10 + '-D' + #10 ) ;
        end;
     if SlicesCheck.Checked then
        begin
        //TODO: move this check to CheckParameters
          try
            StrToInt(ArchiveForm.SliceSize.Text);
          except
            SliceSize.Text := '650';
          end;
          BatchFile.Add ( rsCreateSlices  + #10 + '--slice ' +
            SliceSize.Text + 'M' + #10 ) ;
          if PauseCheck.Checked then
             BatchFile.Add ( rsPauseBetween + #10 + '--pause ' + #10 ) ;
        end;
end;

function TArchiveForm.CreateScript: Boolean;
var
  script: Textfile;
begin
  if CreateBatchfile then
     begin
       AssignFile(script, ScriptFilenameBox.Text);
          try
            Rewrite(script);
            
          finally

          end;
       
     end;
end;


initialization
  {$I archive.lrs}

end.

