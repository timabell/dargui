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
    BitBtn1: TBitBtn;
    Image1: TImage;
    VersionLabel: TLabel;
    SVNLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    DarVersionLabel: TLabel;
    HotLabel1: TLabel;
    procedure HotLabel1Click ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{ TAboutForm }

uses Process, darintf;



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

initialization
  {$I about.lrs}

end.

