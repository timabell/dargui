unit scriptsavedlg;

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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  CreateSaveDialog: TCreateSaveDialog;

implementation

initialization
  {$I scriptsavedlg.lrs}

end.

