unit FileConflict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, Buttons;

type

  { TFileConflictForm }

  TFileConflictForm = class ( TForm )
    FileListBox: TListBox;
    OverwriteAllButton: TBitBtn;
    OverwriteNoneButton: TBitBtn;
    InstructionLabel: TLabel;
    InstructionPanel: TPanel;
    ButtonPanel: TPanel;
    FileListPanel: TPanel;
    MainPanel: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FileConflictForm: TFileConflictForm;

implementation

initialization
  {$I fileconflict.lrs}

end.

