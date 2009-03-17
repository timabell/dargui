unit FileConflict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TFileConflictForm }

  TFileConflictForm = class ( TForm )
    FileListBox: TListBox;
    YesButton: TBitBtn;
    NoButton: TBitBtn;
    InstructionLabel: TLabel;
    InstructionPanel: TPanel;
    ButtonPanel: TPanel;
    FileListPanel: TPanel;
    MainPanel: TPanel;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormResize ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FileConflictForm: TFileConflictForm;

implementation

uses dgStrConst;

{ TFileConflictForm }

procedure TFileConflictForm.FormCreate ( Sender: TObject ) ;
begin
  YesButton.Caption := rsYes;
  NoButton.Caption := rsNo;
end;

procedure TFileConflictForm.FormResize ( Sender: TObject ) ;
begin
  YesButton.Left := (ButtonPanel.Width div 2) - YesButton.Width - 20;
  NoButton.Left := (ButtonPanel.Width div 2) + 20;
end;

initialization
  {$I fileconflict.lrs}

end.

