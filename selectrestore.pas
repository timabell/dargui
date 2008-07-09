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
    RestoreFilesLabel: TLabel;
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
    procedure InitialiseInterface;
  public
    { public declarations }
    FullRestore: Boolean;
  end;
  

implementation

{ TExtractSelectedForm }

uses
  dgStrConst, darintf;


procedure TExtractSelectedForm.FormCreate(Sender: TObject);
begin
  SelectedFiles := TStringList.Create;
  ExistingFiles := TStringList.Create;
  FullRestore := true;
  InitialiseInterface;
  RestoreDirectoryEdit.Directory := SysUtils.GetEnvironmentVariable('HOME');
end;

procedure TExtractSelectedForm.InitialiseInterface;
begin
  FlatRestoreCheckBox.Caption := rsIgnoreDirectories;
  RestoreFilesLabel.Caption:=rsRestoreFilesTo;
  OkButton.Caption := rsButtonOK;
  CancelButton.Caption := rsButtonCancel;
  OverwriteOptions.Caption := rsExistingFiles;
  OverwriteOptions.Items[0] := rsOverwriteExistingFil;
  OverwriteOptions.Items[1] := rsConfirmBeforeOverwri;
  OverwriteOptions.Items[2] := rsDoNotOverwriteFiles;
end;


procedure TExtractSelectedForm.OkButtonClick(Sender: TObject);
begin
  if RestoreDirectoryEdit.Text <> ''
     then
     begin
       if FileExists(RestoreDirectoryEdit.Text)
          then ModalResult := mrOk
          else ShowMessage ( rsMessDirectoryNotFound ) ;
      end
      else
      begin
        ShowMessage ( rsMessInvalidDestinationDir ) ;
        RestoreDirectoryEdit.SetFocus;
      end;
end;

procedure TExtractSelectedForm.FormDestroy ( Sender: TObject ) ;
begin
  SelectedFiles.Free;
  ExistingFiles.Free;
end;

initialization
  {$I selectrestore.lrs}

end.

