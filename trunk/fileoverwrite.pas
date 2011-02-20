unit FileOverwrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, Buttons;

type

  { TOverwriteFilesForm }

  TOverwriteFilesForm = class ( TForm )
    OverwriteAllButton: TBitBtn;
    OverwriteNoneButton: TBitBtn;
    OverwriteMarkedButton: TBitBtn;
    AbortButton: TBitBtn;
    FileCheckListBox: TCheckListBox;
    InstructionLabel: TLabel;
    InstructionPanel: TPanel;
    ButtonPanel: TPanel;
    FileListPanel: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  OverwriteFilesForm: TOverwriteFilesForm;

implementation

initialization
  {$I fileoverwrite.lrs}

end.

