unit selectrestore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, EditBtn;

type

  { TExtractSelectedForm }

  TExtractSelectedForm = class(TForm)
    Bevel1: TBevel;
    FlatRestoreCheckBox: TCheckBox;
    Label1: TLabel;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    ButtonPanel: TPanel;
    OverwriteOptions: TRadioGroup;
    RestoreDirectoryEdit: TDirectoryEdit;
    SelectedFiles: TStringList;
    ExistingFiles: TStringList;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FullRestore: Boolean;
    function ConfirmOverwriteFiles: TModalResult;
  end; 
  

implementation

{ TExtractSelectedForm }

uses main, FileOverwrite, darintf;


procedure TExtractSelectedForm.FormCreate(Sender: TObject);
begin
  SelectedFiles := TStringList.Create;
  ExistingFiles := TStringList.Create;
  FullRestore := true;
end;

function TExtractSelectedForm.ConfirmOverwriteFiles: TModalResult;
var
  fp: String;
  fn: String;
  x: Integer;
  fd: TFileData;
begin
  fp := '';
  Result := mrNone;
  OverwriteFilesForm.FileCheckListBox.Clear;
  if SelectedFiles.Count > 0 then
     begin   // Restore Selected
       for x := SelectedFiles.Count -1 downto 0 do
          begin
            if not FlatRestoreCheckBox.Checked
               then fp := ExtractFilePath(SelectedFiles.Strings[x]);
            fn := RestoreDirectoryEdit.Text + DirectorySeparator + fp + ExtractFileName(SelectedFiles.Strings[x]);
            if FileExists(fn) then
               begin
                 ExistingFiles.Add(SelectedFiles.Strings[x]); // file as it exists in archive
                 OverwriteFilesForm.FileCheckListBox.Items.Add(fn); // file as it exists on disk
                 SelectedFiles.Delete(x);
               end;
          end;
     end
     else
     begin   // Restore All
       for x := MainForm.ArchiveTreeView.Items.Count-1 downto 1 do
          begin
            fp := '';
            fd := TFileData(MainForm.ArchiveTreeView.Items[x].Data);
            if not FlatRestoreCheckBox.Checked
               then fp := fd.item[SEGFILEPATH];
            fn := RestoreDirectoryEdit.Text + DirectorySeparator + fp + fd.item[SEGFILENAME];
writeln('Checking overwrite for ' + fn);
            if FileExists(fn) then
               begin
                 ExistingFiles.Add(fp + fd.item[SEGFILENAME]); // file as it exists in archive
                 OverwriteFilesForm.FileCheckListBox.Items.Add(fn); // file as it exists on disk
               end;
          end;
     end;
  case OverwriteFilesForm.ShowModal of
      mrAbort : Result := mrCancel;
      mrYes : for x := ExistingFiles.Count-1 downto 0 do     // Overwrite checked items
              begin
                if ((not OverwriteFilesForm.FileCheckListBox.Checked[x]) xor FullRestore)
                   then ExistingFiles.Delete(x);
              end;
      mrAll : if FullRestore then ExistingFiles.Clear;                                             // Overwrite all
      mrNoToAll : if not FullRestore then ExistingFiles.Clear;                       // Overwrite none
      end;
  if Result = mrNone then Result := mrOK;
end;

procedure TExtractSelectedForm.OkButtonClick(Sender: TObject);
begin
  if OverwriteOptions.ItemIndex = 1
    then
     begin
       ModalResult := ConfirmOverwriteFiles;
       SelectedFiles.Text := SelectedFiles.Text + ExistingFiles.Text;
     end;
  if ModalResult <> mrCancel then ModalResult := mrOk;
end;

procedure TExtractSelectedForm.FormDestroy ( Sender: TObject ) ;
begin
  SelectedFiles.Free;
  ExistingFiles.Free;
end;

initialization
  {$I selectrestore.lrs}

end.

