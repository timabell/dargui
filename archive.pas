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
    Slicesize: TEdit;
    Label11: TLabel;
    Label8: TLabel;
    SlicesCheck: TCheckBox;
    CompLwrLimitCombo: TComboBox;
    CompressionLevel: TEdit;
    DelCompressMaskButton: TButton;
    CompressionLwrLimit: TEdit;
    EmptyDirCheck: TCheckBox;
    DryRunCheck: TCheckBox;
    CompressionPage: TPage;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GZipCheck: TCheckBox;
    CompLvlLabel: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
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
    procedure AddIncludeDirButtonClick ( Sender: TObject ) ;
    procedure AddIncludeFileButtonClick ( Sender: TObject ) ;
    procedure ArchiveDirButtonClick ( Sender: TObject ) ;
    procedure BaseDirButtonClick ( Sender: TObject ) ;
    procedure CompressionLevelExit ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
    procedure DelExcludeFileButtonClick ( Sender: TObject ) ;
    procedure DelIncludeDirButtonClick ( Sender: TObject ) ;
    procedure DelIncludeFileButtonClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
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

procedure TArchiveForm.AddIncludeFileButtonClick ( Sender: TObject ) ;
var
  LB: TListBox;
  DelBn: TButton;
begin
  OpenDialog.Tag := 1;
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
  if saveDialog.Execute
     then ArchiveDirectory.Text := OpenDialog.FileName;
end;

procedure TArchiveForm.BaseDirButtonClick ( Sender: TObject ) ;
begin
  if OpenDialog.Execute
     then BaseDirectory.Text := OpenDialog.FileName;
end;

procedure TArchiveForm.CompressionLevelExit ( Sender: TObject ) ;
begin
  if not(CompressionLevel.Text[1] in ['0'..'9']) then
     begin
     ShowMessage('Compression level must be between 0 and 9');
     CompressionLevel.Text := '9';
     CompressionLevel.SetFocus;
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

