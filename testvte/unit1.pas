unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, vteterminal, ExtCtrls, ColorBox, Process, baseunix, gtk2proc, gdk2x,
  gtkterm;

type

  { TVTEDemoForm }

  TVTEDemoForm = class(TForm)
    ForegroundColorBox: TColorBox;
    BackgroundColorBox: TColorBox;
    CreateTerminalButton: TButton;
    CreateSlaveButton: TButton;
    DestroyTerminalButton: TButton;
    ExecuteCommandEdit: TEdit;
    ExecuteButton: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    SendNewlineButton: TButton;
    Label2: TLabel;
    Label3: TLabel;
    SendEscButton: TButton;
    CreateEmbedButton: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    DemoPanel: TPanel;
    procedure Button1Click ( Sender: TObject ) ;
    procedure ColorBoxChange ( Sender: TObject ) ;
    procedure CreateSlaveButtonClick(Sender: TObject);
    procedure CreateTerminalButtonClick ( Sender: TObject ) ;
    procedure DestroyTerminalButtonClick(Sender: TObject);
    procedure ExecuteButtonClick(Sender: TObject);
    procedure SendNewlineButtonClick(Sender: TObject);
    procedure SendEscButtonClick(Sender: TObject);
    procedure CreateEmbedButtonClick(Sender: TObject);
    procedure FormClose ( Sender: TObject; var CloseAction: TCloseAction ) ;
    procedure TerminateCallback(Sender: TObject; exitcode:integer);
    function CancelCallback ( Sender: TObject ) : Boolean;
    function VTETerminalClose(Sender: TObject): boolean;
  private
    { private declarations }
  public
    { public declarations }
    procedure TestOutput(Sender:TObject; output: string);
  end;

var
  VTEDemoForm: TVTEDemoForm;
  VTESlave: TVTETerminal;
  VTE: TVTETerminal;
  //VTE: TBlindTerminal;
  Cancelled: Boolean;
  embedTerm: TGtkTerm;


var
  O, E: TPipeReader;


implementation



{ TVTEDemoForm }

procedure TVTEDemoForm.CreateSlaveButtonClick(Sender: TObject);
var
  S : string;
begin
  S := Edit1.Text;
  VTESlave := TVTEterminal.Create(PChar(s));//('dar -l /home/malcolm/test -v');//('ls -a');
  //VTE := TVTETerminal.Create(nil);
  VTESlave.Caption := 'VTE Slave Terminal: ' + Edit1.Text;
  Cancelled   := false;
  VTESlave.OnClose := @VTETerminalClose;
  VTESlave.OnCancel := @CancelCallback;
  VTESlave.OnProcessExit := @TerminateCallback;
  VTESlave.CancelButtonVisible := true;
  //VTE.Width := 1000;
  //VTE.Height := 150;
  //vte.WindowState := wsMaximized;
  //VTE.OnStdout:=@TestOutput;
  //VTE.OnStderr:=@TestOutput;
  //VTE.StdPipe:='/tmp/stdout';
  //VTE.ErrPipe:='/tmp/stderr';
  //VTE.ShowModal(GDK_WINDOW_XWINDOW(GetControlWindow(PGtkWidget(Self.Handle))));
  //FormID := GDK_WINDOW_XWINDOW(GetControlWindow(PGtkWidget(Self.Handle)));
  //vte.ShowModal(GDK_WINDOW_XWINDOW( pointer( PGtkWidget(VTEDemoForm.Handle)^.Window ) ));
  VTESlave.ShowModal(Self);
  //VTE.Show;
  VTESlave.Execute;

  //BTE := TBlindTerminal.Create;

  CreateSlaveButton.Enabled := false;
end;

procedure TVTEDemoForm.ColorBoxChange ( Sender: TObject ) ;
begin
  if VTE=nil then exit;
  if Sender=ForegroundColorBox
     then VTE.Foreground := TColorBox(Sender).Selected
  else if Sender=BackgroundColorBox
     then VTE.Background := TColorBox(Sender).Selected;
end;

procedure TVTEDemoForm.Button1Click ( Sender: TObject ) ;
begin
  embedTerm.Height := 250;
  EmbedTerm.Width := DemoPanel.Width-20;
end;


procedure TVTEDemoForm.CreateTerminalButtonClick ( Sender: TObject ) ;
begin
  CreateTerminalButton.Enabled := false;
  DestroyTerminalButton.Enabled := true;

  VTE := TVTETerminal.Create(nil);
  VTE.Caption := 'VTE Terminal';
  Cancelled   := false;
  VTE.OnClose := @VTETerminalClose;
  VTE.Foreground := ForegroundColorBox.Selected;
  VTE.Background := BackgroundColorBox.Selected;
  VTE.Show;
end;

procedure TVTEDemoForm.DestroyTerminalButtonClick(Sender: TObject);
begin
  //BTE.Free;
  VTE.Free;
  VTE := nil;
  if O <> nil then O.Terminate;
  if E <> nil then E.Terminate;
  O := nil;
  E := nil;
  DestroyTerminalButton.Enabled := false;
  CreateTerminalButton.Enabled := true;
end;

procedure TVTEDemoForm.ExecuteButtonClick(Sender: TObject);
var
  s: String;
begin
  s := ExecuteCommandEdit.Text;
  VTE.ExecuteCommand(s);
  VTE.BringToFront;
  //VTE.ExecuteCommand(Edit1.Text);
 { if O <> nil then O.Terminate;
  if E <> nil then E.Terminate;
  if VTE.Output <> ''
     then begin
     O := TPipeReader.Create(true, VTE.Output);
     O.OnOutput:=@TestOutput;
     end;
  if VTE.ErrorOutput <> ''
     then E  := TPipeReader.Create(true, VTE.ErrorOutput);
  if O <> nil then O.Resume;
  if E <> nil then E.Resume; }
end;

procedure TVTEDemoForm.SendNewlineButtonClick(Sender: TObject);
begin
  if VTE=nil then exit;
  VTE.SendNewline;
end;

procedure TVTEDemoForm.SendEscButtonClick(Sender: TObject);
begin
 if VTE=nil then exit;
 VTE.SendEscape;
end;

procedure TVTEDemoForm.CreateEmbedButtonClick(Sender: TObject);
begin
  embedTerm := TGtkTerm.Create(nil);
  embedTerm.Parent := DemoPanel;
  embedTerm.Height := 250;
  EmbedTerm.Width := DemoPanel.Width-20;
  embedTerm.Top := 5;
  embedTerm.Left := 10;
  CreateEmbedButton.Enabled := false;
  embedTerm.SetFocus;
end;

procedure TVTEDemoForm.FormClose ( Sender: TObject; var CloseAction: TCloseAction ) ;
begin
  if VTE<>nil then VTE.Free;
  if VTESlave<>nil then VTESlave.free;
  if embedTerm<>nil then embedTerm.Free;
end;

procedure TVTEDemoForm.TerminateCallback(Sender: TObject; exitcode: integer);
begin
  if not Cancelled then
  begin
    VTESlave.Print(#27 + '[31m-------------------------------------------' + #27 + '[m' + #10#13);
    VTESlave.Print(#27 + '[31mProcess terminated' + #27 + '[m'+#10#13);
  end;
end;

function TVTEDemoForm.CancelCallback ( Sender: TObject ) : Boolean;
begin
  with TVTETerminal(Sender) do
       begin
           Print(#27 + '[31m-------------------------------------------' + #27 + '[m' + #10#13);
           Print(#27 + '[31mOperation cancelled' + #27 + '[m'+ #10#13+ #10#13);
           TerminateProcess(SIGKILL);
       end;
  Cancelled := true;
  Result := true;
end;

function TVTEDemoForm.VTETerminalClose ( Sender: TObject ): Boolean ;
begin
  if Sender=VTE then
     begin
       VTE.Free;
       VTE := nil;
       DestroyTerminalButton.Enabled := false;
       CreateTerminalButton.Enabled := true;
     end
  else if Sender=VTESlave then
     begin
       VTESlave.Free;
       VTESlave := nil;
     end;
  Result := true;
end;

procedure TVTEDemoForm.TestOutput(Sender: TObject; output: string);
begin
  writeln(output);
end;



initialization
  {$I unit1.lrs}

end.


