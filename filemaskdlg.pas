unit filemaskdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TFileMaskDialog }

  TFileMaskDialog = class ( TForm )
    FileMask: TComboBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    FileMaskLabel: TLabel;
    procedure FileMaskExit(Sender: TObject);
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormShow ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
    procedure StoreMasks ;
  private
    { private declarations }
    fMasks: string;
    procedure InitialiseInterface;
  public
    { public declarations }
    procedure PopulateFilterList( FilterString: string );
    procedure SetPosition( Sender: TWincontrol);
    property Masks : string read fMasks;
  end; 

var
  FileMaskDialog: TFileMaskDialog;

implementation

uses dgStrConst;

{ TFileMaskDialog }

procedure TFileMaskDialog.FormShow ( Sender: TObject ) ;
begin
  FileMask.SetFocus;
end;

procedure TFileMaskDialog.OKButtonClick ( Sender: TObject ) ;
begin
  ModalResult := mrOk;
  StoreMasks;
end;

procedure TFileMaskDialog.StoreMasks ;
var
  x: Integer;
begin
 fMasks := '';
 if ModalResult = mrOK then
 begin
   if FileMask.Items.Count > 0
      then fMasks := FileMask.Items[0];
   for x := 1 to FileMask.Items.Count-1 do
       fMasks := fMasks + ';' + FileMask.Items[x];
  end;
end;

procedure TFileMaskDialog.InitialiseInterface;
begin
  OKButton.Caption := rsButtonOK;
  CancelButton.Caption := rsButtonCancel;
  FileMaskLabel.Caption := rsFileMask;
end;

procedure TFileMaskDialog.PopulateFilterList(FilterString: string);
var
  a,
  b: integer;
begin
  FileMask.Items.Clear;
  if Length(FilterString) < 1 then exit;
  a := 1;
  b := a;
  while b < Length(FilterString) do
        begin
          if FilterString[b] = ';' then
             begin
               FileMask.Items.Add(Copy(FilterString, a, b-a));
               a := b+1;
             end;
          Inc(b);
        end;
  FileMask.Items.Add(Copy(FilterString, a, b-a+1));
end;

procedure TFileMaskDialog.SetPosition ( Sender: TWincontrol ) ;
// positions dialog in centre of Sender
begin
  FileMask.Text := '';
  with TWinControl(Sender) do
  begin
    FileMaskDialog.Top := Top + ((Height - FileMaskDialog.Height) div 2);
    FileMaskDialog.Left := Left + ((Width - FileMaskDialog.Width) div 2);
  end;
end;

procedure TFileMaskDialog.FileMaskExit(Sender: TObject);
var
  i: Integer;
begin
  if Filemask.Text = '' then exit;
  i := FileMask.Items.IndexOf(FileMask.Text);
  if i > -1
         then FileMask.Items.Delete(i);
  FileMask.Items.Insert(0,FileMask.Text);
  if FileMask.Items.Count > FileMask.DropDownCount
     then FileMask.Items.Delete(FileMask.Items.Count-1);
  StoreMasks;
end;

procedure TFileMaskDialog.FormCreate ( Sender: TObject ) ;
begin
  InitialiseInterface;
end;

initialization
  {$I filemaskdlg.lrs}

end.

