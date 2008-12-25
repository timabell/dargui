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
    SaveDialog1: TSaveDialog;
    procedure BrowseButtonClick ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  CreateSaveDialog: TCreateSaveDialog;

implementation

{ TCreateSaveDialog }

procedure TCreateSaveDialog.BrowseButtonClick ( Sender: TObject ) ;
begin
  if SaveDialog1.Execute
     then FilenameEdit.Text := SaveDialog1.FileName;
end;

initialization
  {$I backupsavedlg.lrs}

end.

