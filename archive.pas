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
    RepeatMonthBox: TComboBox;
    RepeatMonthDayBox: TComboBox;
    RepeatWeekDayBox: TComboBox;
    RepeatHourBox: TComboBox;
    RunOnceMinuteBox: TComboBox;
    RunOnceHourBox: TComboBox;
    RunOnceDateEdit: TDateEdit;
    RepeatMinuteBox: TComboBox;
    TimestampCheck: TCheckBox;
    DiffRefButton: TButton;
    DiffFileCheck: TCheckBox;
    CompressMasks: TButton;
    ExcludeFileMasks: TButton;
    IncludeFileMasks: TButton;
    BatchFileButton: TButton;
    BatchFile: TEdit;
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
    EmptyDirCheck: TCheckBox;
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
    AddExcludeDirButton: TButton;
    DelExcludeDirButton: TButton;
    DelExcludeFileButton: TButton;
    AddExcludeFileButton: TButton;
    DelIncludeFileButton: TButton;
    ArchiveBaseLabel: TLabel;
    BaseDirLabel: TLabel;
    SaveArchiveInLabel: TLabel;
    IncludeDirLabel: TLabel;
    ExcludeDirLabel: TLabel;
    IncludeFilesLabel: TLabel;
    ExcludeFilesLabel: TLabel;
    IncludeDirectories: TListBox;
    ExcludeDirectories: TListBox;
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
    SelectDirectoryDialog: TSelectDirectoryDialog;
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
    procedure OKButtonClick ( Sender: TObject ) ;
    procedure DelExcludeFileButtonClick ( Sender: TObject ) ;
    procedure DelIncludeDirButtonClick ( Sender: TObject ) ;
    procedure DelIncludeFileButtonClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure SaveScriptCheckBoxClick ( Sender: TObject ) ;
    procedure ScriptFileButtonClick ( Sender: TObject ) ;
    procedure SlicesCheckClick ( Sender: TObject ) ;
    procedure ZipCheckChange ( Sender: TObject ) ;
    procedure InitialiseInterface;
  private
    { private declarations }
    function CheckParameters: Boolean;
    function IsInBaseDirectory(aDir: string): Boolean;
    procedure ResolveConflicts( Sender, RefList: TObject; ConflictMessage: string );
  public
    { public declarations }
  end; 

var
  ArchiveForm: TArchiveForm;

implementation

uses dgStrConst, darintf, filemaskdlg, prefs, fileconflict;

{ TArchiveForm }

procedure TArchiveForm.DelIncludeDirButtonClick ( Sender: TObject ) ;
var
  LB: TListBox;
begin
  case TButton(Sender).Tag of
       0: LB := IncludeDirectories;
       1: LB := ExcludeDirectories;
       end;
  if LB.Count > 0 then
     LB.Items.Delete(LB.ItemIndex);
  TButton(Sender).Enabled := LB.Count > 0;
end;

procedure TArchiveForm.DelIncludeFileButtonClick ( Sender: TObject ) ;
begin
  if IncludeFiles.Count > 0 then
     IncludeFiles.Items.Delete(IncludeFiles.ItemIndex);
  DelIncludeFileButton.Enabled := IncludeFiles.Count > 0;
end;

procedure TArchiveForm.FormCreate ( Sender: TObject ) ;
var
  x: Integer;
begin
  ArchiveDirectory.Text := SysUtils.GetEnvironmentVariable('HOME');
  BaseDirectory.Text := SysUtils.GetEnvironmentVariable('HOME');
  SelectDirectoryDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  OpenDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  SaveDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  for x := 0 to 23 do
      begin
        RunOnceHourBox.Items.Add(IntToStr(x));
        RepeatHourBox.Items.Add(IntToStr(x));
      end;
  for x := 0 to 59 do
      begin
        RunOnceMinuteBox.Items.Add(IntToStr(x));
        RepeatMinuteBox.Items.Add(IntToStr(x));
      end;
  for x := 1 to 31 do
      RepeatMonthDayBox.Items.Add(IntToStr(x));
  for x := 1 to 7 do
      RepeatWeekDayBox.Items.Add(IntToStr(x));
  for x := 1 to 12 do
      RepeatMonthBox.Items.Add(IntToStr(x));
  InitialiseInterface;
end;

procedure TArchiveForm.SaveScriptCheckBoxClick ( Sender: TObject ) ;
begin
  ScriptFilenameBox.Enabled := SaveScriptCheckBox.Checked;
  ScriptFileButton.Enabled := SaveScriptCheckBox.Checked;
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
  AddExcludeDirButton.Caption := rsButtonAdd;
  DelExcludeDirButton.Caption := rsButtonRemove;
  DelExcludeFileButton.Caption := rsButtonRemove;
  AddExcludeFileButton.Caption := rsButtonAddFile;
  DelIncludeFileButton.Caption := rsButtonRemove;
  ArchiveBaseLabel.Caption := rsArchiveBaseName;
  BaseDirLabel.Caption := rsBaseDirectory;
  SaveArchiveInLabel.Caption := rsSaveArchiveIn;
  IncludeDirLabel.Caption := rsIncludeDirectories;
  ExcludeDirLabel.Caption := rsExcludeDirectories;
  IncludeFilesLabel.Caption := rsIncludeFiles;
  ExcludeFilesLabel.Caption := rsExcludeFiles;
  ArchivePage.Caption := rsArchive;
  OpenDialog.Title := rsSelectFile;
  OptionsPage.Caption := rsOptions;
  DirectoriesPage.Caption := rsDirectories;
  FilesPage.Caption := rsFiles;
  TimestampCheck.Caption := rsIncludeTimeInName;
  DiffFileCheck.Caption := rsDifferentialBackup;
  DiffReference.EditLabel.Caption := rsReferenceArchive;
  DiffRefButton.Caption := rsButtonBrowse;
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
  if SelectDirectoryDialog.Execute then
    //if IsInBaseDirectory(SelectDirectoryDialog.FileName) then
      begin
        case TButton(Sender).Tag of
             0: begin LB := IncludeDirectories;
                      ConflictList := ExcludeDirectories;
                      DelBn := DelIncludeDirButton;
                      ConflictMessage := Format ( rsDirectoriesAreExcluded, [ #10 ] ) ;
                      end;
             1: begin LB := ExcludeDirectories;
                      ConflictList := IncludeDirectories;
                      ConflictMessage := Format ( rsDirectoriesAreIncluded, [ #10 ] ) ;
                      DelBn := DelExcludeDirButton;
                      end;
             end;
        ResolveConflicts(LB, ConflictList, ConflictMessage );
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
begin
  OpenDialog.Options := OpenDialog.Options+[ofAllowMultiSelect];
  if OpenDialog.Execute then
//    if IsInBaseDirectory(OpenDialog.FileName) then
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
            if LB.Items.IndexOf(OpenDialog.Files[x]) < 0
               then LB.Items.Add(OpenDialog.Files[x]);
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
  SaveDialog.FileName := BatchFile.Text;
  if SaveDialog.Execute
     then BatchFile.Text := SaveDialog.FileName;
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
begin
  if NoCompressList.Count > 0  then
     if NoCompressList.ItemIndex > -1 then
     begin
     NoCompressList.Items.Delete(NoCompressList.ItemIndex);
     DelCompressMaskButton.Enabled := NoCompressList.Count > 0;
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

procedure TArchiveForm.OKButtonClick ( Sender: TObject ) ;
begin
  if CheckParameters then
     begin
     ModalResult := mrOk;
     if FileExists(ArchiveDirectory.Text
                            + ArchiveName.Text + '.1.dar')
        then if MessageDlg ( rsOverwriteExistingArc, mtWarning, [ mbYes, mbNo
          ] , 0 ) = mrNo
             then ModalResult := mrCancel
             else DeleteFilesByMask(ArchiveDirectory.Text
                            + ArchiveName.Text + '.*.dar');
     end
     else ShowMessage('Please check all input');
end;

procedure TArchiveForm.DelExcludeFileButtonClick ( Sender: TObject ) ;
begin
  if ExcludeFiles.Count > 0 then
     if ExcludeFiles.ItemIndex > -1 then
        ExcludeFiles.Items.Delete(ExcludeFiles.ItemIndex);
  DelExcludeFileButton.Enabled := ExcludeFiles.Count > 0;
end;

function TArchiveForm.CheckParameters: Boolean;
begin
  Result := true;
  if ArchiveName.Text = '' then Result := false;
  if ArchiveDirectory.Text[Length(ArchiveDirectory.Text)] <> DirectorySeparator
     then ArchiveDirectory.Text := ArchiveDirectory.Text + DirectorySeparator;
  if BaseDirectory.Text[Length(BaseDirectory.Text)] <> DirectorySeparator
     then BaseDirectory.Text := BaseDirectory.Text + DirectorySeparator;
  if not FileExists(ArchiveDirectory.Text) then Result := false;
  if not FileExists(BaseDirectory.Text)  then Result := false;
  if CompLwrLimitCombo.ItemIndex< 0 then CompLwrLimitCombo.ItemIndex := 0;
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

procedure TArchiveForm.ResolveConflicts ( Sender, RefList: TObject;
  ConflictMessage: string ) ;
var
  LB: TListBox;
  RB : TListBox;
  x: Integer;
  Dialog: TOpenDialog;
begin
  if not (Sender is TListBox) then exit;
  FileConflictForm := TFileConflictForm.Create(Self);
  FileConflictForm.Caption := rsCptResolveConflict;
  FileConflictForm.InstructionLabel.Caption := ConflictMessage;
  LB := TListBox(Sender);
  RB := TlistBox(RefList);
  if LB.Tag = 0 then Dialog := SelectDirectoryDialog
     else Dialog := OpenDialog;
  for x := 0 to Dialog.Files.Count-1 do
      if RB.Items.IndexOf(Dialog.Files[x]) > -1
         then FileConflictForm.FileListBox.Items.Add(Dialog.Files[x]);
  if FileConflictForm.FileListBox.Items.Count > 0 then
       if FileConflictForm.ShowModal = mrNo then
          begin
            for x := Dialog.Files.Count-1 downto 0 do
              if FileConflictForm.FileListBox.Items.IndexOf(Dialog.Files[x]) > -1
                 then Dialog.Files.Delete(x);
          end
       else
          begin
            for x := RB.Items.Count-1 downto 0 do
              if FileConflictForm.FileListBox.Items.IndexOf(RB.Items[x]) > -1
                 then RB.Items.Delete(x);
          end;
  FileConflictForm.Free;
end;


initialization
  {$I archive.lrs}

end.

