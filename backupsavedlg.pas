unit backupsavedlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TCreateSaveDialog }

  TCreateSaveDialog = class ( TForm )
    BrowseButton: TButton;
    CancelButton: TBitBtn;
    DialogButtonPanel: TPanel;
    FilenameEdit: TEdit;
    FilenameLabel: TLabel;
    NotesLabel: TLabel;
    NotesMemo: TMemo;
    OKButton: TBitBtn;
    SaveDialog: TSaveDialog;
    procedure BrowseButtonClick ( Sender: TObject ) ;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure InitialiseInterface;
  public
    { public declarations }
  end; 

var
  CreateSaveDialog: TCreateSaveDialog;

implementation

uses dgStrConst;

{ TCreateSaveDialog }

procedure TCreateSaveDialog.BrowseButtonClick ( Sender: TObject ) ;
begin
  SaveDialog.FileName := FilenameEdit.Text;
  if SaveDialog.Execute
     then FilenameEdit.Text := SaveDialog.FileName;
end;

procedure TCreateSaveDialog.FormCreate(Sender: TObject);
begin
  InitialiseInterface;
end;

procedure TCreateSaveDialog.InitialiseInterface;
begin
  Caption := rsCptSaveBackupSettings;
  FilenameLabel.Caption := rsSaveSettingsAs;
  NotesLabel.Caption := rsNotes;
  OKButton.Caption := rsButtonOK;
  CancelButton.Caption := rsButtonCancel;
end;

initialization
  {$I backupsavedlg.lrs}

end.

