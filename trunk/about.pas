unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TAboutForm }

  TAboutForm = class ( TForm )
    Bevel1: TBevel;
    OKButton: TBitBtn;
    Image1: TImage;
    VersionLabel: TLabel;
    SVNLabel: TLabel;
    GPLLabel: TLabel;
    AuthorLabel: TLabel;
    DevInfoLabel: TLabel;
    DarVersionLabel: TLabel;
    HotLabel1: TLabel;
    procedure FormCreate ( Sender: TObject ) ;
    procedure HotLabel1Click ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{ TAboutForm }

uses Process, darintf, dgStrConst;



{ TAboutForm }

procedure TAboutForm.HotLabel1Click ( Sender: TObject ) ;
var
  B: string;
  P: string;
  Proc: TProcess;
begin
  GetDefaultBrowser(B, P);
  writeln(B);
  if B <> '' then
     begin
       Proc := TProcess.Create(Application);
       Proc.CommandLine := B + #32 + TLabel(Sender).Caption;
       Proc.Execute;
     end;
end;

procedure TAboutForm.FormCreate ( Sender: TObject ) ;
begin
  Caption := rsMenuHelpAbout;
  VersionLabel.Caption := rsVersionNumber;
  SVNLabel.Caption := rsSVNRevision;
  AuthorLabel.Caption := Format ( rsAuthor, [ 'Malcolm Poole' ] ) ;
  GPLLabel.Caption := rsLicenceGPL;
  DevInfoLabel.Caption := rsDarGUIHasBeenDevelop;
  OKButton.Caption := rsButtonOK;
end;

initialization
  {$I about.lrs}

end.

