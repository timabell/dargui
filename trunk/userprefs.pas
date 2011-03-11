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
    DefaultConfigEdit: TFileNameEdit;
    DefConfigLabel: TLabel;
    FilesOpenedLbl: TLabel;
    MainFormPosition: TCheckBox;
    MainFormSize: TCheckBox;
    OptsPageControl: TPageControl;
    DisplayPage: TTabSheet;
    OKButton: TBitBtn;
    ButtonPanel: TPanel;
    RecentFilesSpinEdit: TSpinEdit;
    RememberLbl: TLabel;
    ScrollBox1: TScrollBox;
    ToolbarCheck: TCheckBox;
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
   Caption := rsUserPreferen;
   DisplayPage.Caption := rsGeneral;
   MainFormPosition.Caption := rsRememberWinPos;
   MainFormSize.Caption := rsRememberWinSize;
   ToolbarCheck.Caption := rsMenuShowToolbar;
   RememberLbl.Caption := rsRememberLast;
   FilesOpenedLbl.Caption := rsFilesOpened;
   RecentFilesSpinEdit.Left := RememberLbl.Left + RememberLbl.Width + 10;
   FilesOpenedLbl.Left := RecentFilesSpinEdit.Left + RecentFilesSpinEdit.Width + 10;
   DefConfigLabel.Caption := rsDefaultSettings;
   DarLocationLabel.Caption := rsDarExecutabl;
   DefaultConfigEdit.Filter := rsFilterDarGUIFiles + '|*.dargui|' + rsFilterAllFiles + '|*';
   DefaultConfigEdit.FilterIndex := 0;
   OKButton.Caption := rsButtonOK;
   CancelButton.Caption := rsButtonCancel;
 end;

initialization
  {$I userprefs.lrs}

end.

