unit password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TPasswordDlg }

  TPasswordDlg = class ( TForm )
    CancelButton: TBitBtn;
    PasswordEdit: TEdit;
    OKButton: TBitBtn;
    PasswordLabel: TLabel;
    procedure FormCreate ( Sender: TObject ) ;
    function GetPassword: string;
    procedure OKButtonClick ( Sender: TObject ) ;
  private
    { private declarations }
    ArchiveName: string;
    function IsValidPassword( pw: string ): Boolean;
  public
    { public declarations }
    function Execute( anArchive: string ): TModalResult;
    procedure Clear;
    property Password: string read GetPassword;
  end; 

var
  PasswordDlg: TPasswordDlg;

implementation

uses dgStrConst;

{ TPasswordDlg }

procedure TPasswordDlg.FormCreate ( Sender: TObject ) ;
begin
  OKButton.Caption := rsButtonOK;
  CancelButton.Caption := rsButtonCancel;
  Caption := 'Enter Password';
  PasswordLabel.Caption := 'This file is encrypted. ' + #10 + 'Enter the correct password for this archive.';
end;

function TPasswordDlg.GetPassword: string;
begin
  Result := PasswordEdit.Text;
end;

procedure TPasswordDlg.OKButtonClick ( Sender: TObject ) ;
begin
  if IsValidPassword(PasswordEdit.Text)
     then ModalResult := mrOK;
end;

function TPasswordDlg.IsValidPassword ( pw: string ) : Boolean;
begin
  Result := false;
end;

function TPasswordDlg.Execute( anArchive: string ): TModalResult;
begin
  Clear;
  ArchiveName := anArchive;
  ActiveControl := PasswordEdit;
  Result := Self.ShowModal;
end;

procedure TPasswordDlg.Clear;
begin
  PasswordEdit.Text := '';
end;

initialization
  {$I password.lrs}

end.

