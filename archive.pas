unit archive;

{$mode objfpc}{$H+}
{ TODO: This is a Lazarus feature: if you select 2 buttons, the Object Inspector does not allow you to change their Tag.
}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TArchiveForm }

  TArchiveForm = class ( TForm )
    AddCompressMaskButton: TButton;
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
    Label9: TLabel;
    NoCompressList: TListBox;
    ReadConfigCheck: TCheckBox;
    DelIncludeDirButton: TButton;
    AddExcludeDirButton: TButton;
    DelExcludeDirButton: TButton;
    DelExcludeFileButton: TButton;
    AddExcludeFileButton: TButton;
    DelIncludeFileButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
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
    procedure AddCompressMaskButtonClick ( Sender: TObject ) ;
    procedure AddIncludeDirButtonClick ( Sender: TObject ) ;
    procedure AddIncludeFileButtonClick ( Sender: TObject ) ;
    procedure ArchiveDirButtonClick ( Sender: TObject ) ;
    procedure BaseDirButtonClick ( Sender: TObject ) ;
    procedure CompressionLevelExit ( Sender: TObject ) ;
    procedure CompressionLwrLimitExit ( Sender: TObject ) ;
    procedure DelCompressMaskButtonClick ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
    procedure DelExcludeFileButtonClick ( Sender: TObject ) ;
    procedure DelIncludeDirButtonClick ( Sender: TObject ) ;
    procedure DelIncludeFileButtonClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure SlicesCheckClick ( Sender: TObject ) ;
    procedure ZipCheckChange ( Sender: TObject ) ;
  private
    { private declarations }
    function CheckParameters: Boolean;
    function IsInBaseDirectory(aDir: string): Boolean;
  public
    { public declarations }
  end; 

var
  ArchiveForm: TArchiveForm;

implementation

uses darintf, filemaskdlg;

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
begin
  ArchiveDirectory.Text := GetCurrentDir;
  BaseDirectory.Text := GetCurrentDir;
end;

procedure TArchiveForm.SlicesCheckClick ( Sender: TObject ) ;
begin
  SliceSizeLabel.Enabled :=  TCheckBox(Sender).Checked;
  SliceSize.Enabled :=  TCheckBox(Sender).Checked;
  SliceMbLabel.Enabled :=  TCheckBox(Sender).Checked;
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


procedure TArchiveForm.AddIncludeDirButtonClick ( Sender: TObject ) ;
var
  LB: TListBox;
  DelBn: TButton;
begin
  if SelectDirectoryDialog.Execute then
    if IsInBaseDirectory(SelectDirectoryDialog.FileName) then
      begin
      case TButton(Sender).Tag of
           0: begin LB := IncludeDirectories; DelBn := DelIncludeDirButton; end;
           1: begin LB := ExcludeDirectories; DelBn := DelExcludeDirButton; end;
           end;
      LB.Items.Add(SelectDirectoryDialog.FileName);
      DelBn.Enabled := true;
      end;
end;

procedure TArchiveForm.AddCompressMaskButtonClick ( Sender: TObject ) ;
begin
  FileMaskDialog.FileMask.Text := '';
  if FileMaskDialog.ShowModal = mrOk then
     if FileMaskDialog.FileMask.Text <> '' then
       begin
       NoCompressList.Items.Add(FileMaskDialog.FileMask.Text);
       NoCompressList.ItemIndex := NoCompressList.Items.Count-1;
       DelCompressMaskButton.Enabled := true;
       end;
end;

procedure TArchiveForm.AddIncludeFileButtonClick ( Sender: TObject ) ;
var
  LB: TListBox;
  DelBn: TButton;
begin
  if OpenDialog.Execute then
    if IsInBaseDirectory(OpenDialog.FileName) then
      begin
      case TButton(Sender).Tag of
           0: begin LB := IncludeFiles; DelBn := DelIncludeFileButton; end;
           1: begin LB := ExcludeFiles; DelBn := DelExcludeFileButton; end;
           end;
      LB.Items.Add(OpenDialog.FileName);
      DelBn.Enabled := true;
      end;
end;

procedure TArchiveForm.ArchiveDirButtonClick ( Sender: TObject ) ;
begin
  if SelectDirectoryDialog.Execute
     then ArchiveDirectory.Text := SelectDirectoryDialog.FileName;
end;

procedure TArchiveForm.BaseDirButtonClick ( Sender: TObject ) ;
begin
  if SelectDirectoryDialog.Execute
     then BaseDirectory.Text := SelectDirectoryDialog.FileName;
end;

procedure TArchiveForm.CompressionLevelExit ( Sender: TObject ) ;
begin
  if not isInteger(CompressionLevel.Text) then
     begin
     ShowMessage('Compression level must be between 0 and 9');
     CompressionLevel.Text := '9';
     CompressionLevel.SetFocus;
     end;
end;

procedure TArchiveForm.CompressionLwrLimitExit ( Sender: TObject ) ;
begin
  if (not isInteger(TEdit(Sender).Text))
  or (TEdit(Sender).Text='') then
     begin
     ShowMessage('Invalid number for file size');
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

procedure TArchiveForm.OKButtonClick ( Sender: TObject ) ;
begin
  if CheckParameters then ModalResult := mrOk;
end;

procedure TArchiveForm.DelExcludeFileButtonClick ( Sender: TObject ) ;
begin
  if ExcludeFiles.Count > 1 then
     if ExcludeFiles.ItemIndex > 0 then
        ExcludeFiles.Items.Delete(ExcludeFiles.ItemIndex);
  DelExcludeFileButton.Enabled := IncludeFiles.Count > 1;
end;

function TArchiveForm.CheckParameters: Boolean;
begin
  Result := true;
  if ArchiveName.Text = '' then Result := false;
  if not FileExists(ArchiveDirectory.Text) then Result := false;
  if not FileExists(BaseDirectory.Text)  then Result := false;
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


initialization
  {$I archive.lrs}

end.

