 unit locatefiledlg;

{$mode objfpc}{$H+}

 interface

 uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  EditBtn, ExtCtrls, StdCtrls, Buttons;

 type

  { TLocateFileForm }

  TLocateFileForm = class(TForm)
    BrowseButton: TButton;
    CancelButton: TBitBtn;
    MessageLabel: TLabel;
    FilenameEdit: TLabeledEdit;
    LoadButton: TSpeedButton;
    OKButton: TBitBtn;
    OKCancelPanel: TPanel;
    OpenDialog1: TOpenDialog;
    SaveButton: TSpeedButton;
    procedure BrowseButtonClick(Sender: TObject);
    procedure FilenameEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

 var
  LocateFileForm: TLocateFileForm;

 implementation

 uses dgStrConst;

 { TLocateFileForm }

 procedure TLocateFileForm.BrowseButtonClick(Sender: TObject);
 begin
   if OpenDialog1.Execute
      then FilenameEdit.Text := OpenDialog1.FileName;
   OKButton.Enabled := (FilenameEdit.Text <> '') and FileExists(FilenameEdit.Text);
 end;

 procedure TLocateFileForm.FilenameEditChange(Sender: TObject);
 begin
   OKButton.Enabled := (FilenameEdit.Text <> '') and FileExists(FilenameEdit.Text);
 end;

 procedure TLocateFileForm.FormCreate(Sender: TObject);
 begin
   OKButton.Caption := rsButtonOK;
   CancelButton.Caption := rsButtonCancel;
 end;

 initialization
  {$I locatefiledlg.lrs}

 end.

