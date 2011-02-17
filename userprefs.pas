unit userprefs;

{
 Set user preferences:
- default compression exceptions
- temporary directory ?
- number of recent files to remember
- show toolbar
- default directory for saving archives
- Size column format
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Spin, Buttons, EditBtn;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    CancelButton: TBitBtn;
    DarLocationEdit: TFileNameEdit;
    DarLocationLabel: TLabel;
    ToolbarCheck: TCheckBox;
    DefaultConfigEdit: TFileNameEdit;
    FilesOpenedLbl: TLabel;
    DefConfigLabel: TLabel;
    OKButton: TBitBtn;
    RememberLbl: TLabel;
    MainFormPosition: TCheckBox;
    MainFormSize: TCheckBox;
    MainNotebook: TNotebook;
    ButtonPanel: TPanel;
    DisplayPage: TPage;
    RecentFilesSpinEdit: TSpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure InitialiseInterface;
  public
    { public declarations }
  end; 

var
  OptionsForm: TOptionsForm;

implementation

uses dgStrConst;

{ TOptionsForm }

 procedure TOptionsForm.FormCreate(Sender: TObject);
 begin
   InitialiseInterface;
 end;

 procedure TOptionsForm.InitialiseInterface;
 begin
   Caption := 'User preferences';
   DisplayPage.Caption := 'General';
   MainFormPosition.Caption := 'Remember main window position on exit';
   MainFormSize.Caption := 'Remember main window size on exit';
   ToolbarCheck.Caption := rsMenuShowToolbar;
   RememberLbl.Caption := 'Remember last';
   FilesOpenedLbl.Caption := 'files opened';
   RecentFilesSpinEdit.Left := RememberLbl.Left + RememberLbl.Width + 10;
   FilesOpenedLbl.Left := RecentFilesSpinEdit.Left + RecentFilesSpinEdit.Width + 10;
   DefConfigLabel.Caption := 'Default archive creation settings';
   DarLocationLabel.Caption := 'Dar executable';
   DefaultConfigEdit.Filter := rsFilterDarGUIFiles + '|*.dargui|' + rsFilterAllFiles + '|*';
   DefaultConfigEdit.FilterIndex := 0;
   OKButton.Caption := rsButtonOK;
   CancelButton.Caption := rsButtonCancel;
 end;

initialization
  {$I userprefs.lrs}

end.

