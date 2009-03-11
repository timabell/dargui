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
    NoMatchLabel: TLabel;
    PasswordEdit: TEdit;
    OKButton: TBitBtn;
    PasswordLabel: TLabel;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormShow ( Sender: TObject ) ;
    function GetPassword: string;
    procedure NoMatchLabelChangeBounds(Sender: TObject);
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

uses dgStrConst, darintf;

{ TPasswordDlg }

procedure TPasswordDlg.FormCreate ( Sender: TObject ) ;
begin
  OKButton.Caption := rsButtonOK;
  CancelButton.Caption := rsButtonCancel;
  Caption := rsEnterPassword;
  NoMatchLabel.Caption := rsIncorrectPasswordEnt;
end;

procedure TPasswordDlg.FormShow ( Sender: TObject ) ;
begin
    PasswordLabel.Caption := Format ( rsTheFileIsEncryptedEn, [ ExtractFileName
      ( ArchiveName ) , #10 ] ) ;
end;

function TPasswordDlg.GetPassword: string;
begin
  Result := PasswordEdit.Text;
end;

procedure TPasswordDlg.NoMatchLabelChangeBounds(Sender: TObject);
begin
    with TLabel(Sender) do
       begin
         if FocusControl <> nil then
            Top := FocusControl.Top - Height;
       end;
end;

procedure TPasswordDlg.OKButtonClick ( Sender: TObject ) ;
begin
  if IsValidPassword(PasswordEdit.Text)
     then ModalResult := mrOK
     else begin
            NoMatchLabel.Visible := false;
            PasswordEdit.Text := '';
            PasswordEdit.SetFocus;
            Application.ProcessMessages;
            Sleep(300);
            NoMatchLabel.Visible := true;
          end;
end;

function TPasswordDlg.IsValidPassword ( pw: string ) : Boolean;
var
  x: Integer;
begin
  if Length(ArchiveName) > 0 then
     Result := GetInodeCount(ArchiveName, ' -K :' + pw) > -1
  else begin
         Result := true;
         for x := 1 to Length(pw) do
             if pw[x] in [#0..#32,':'] then Result := false;
         if Length(pw) = 0 then Result := false;
       end;
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
  NoMatchLabel.Visible := false;
end;

initialization
  {$I password.lrs}

end.

