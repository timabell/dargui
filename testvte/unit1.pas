unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, vteterminal, ExtCtrls, Process, baseunix, gtk2proc, gdk2x,  gtk2, gdk2, glib2, gtkterm;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click ( Sender: TObject ) ;
    procedure FormClose ( Sender: TObject; var CloseAction: TCloseAction ) ;
    procedure Panel1Click(Sender: TObject);
    procedure VTECallback(Sender: TObject; exitcode:integer);
    function CancelCallback ( Sender: TObject ) : Boolean;
    function VTETerminalClose(Sender: TObject): boolean;
  private
    { private declarations }
  public
    { public declarations }
    procedure TestOutput(Sender:TObject; output: string);
  end;

 // TGlist = record;
 //   PGlist = ^TGlist;

 { TGList = record
    data: Pointer;
    next: PGlist;
    prev: PGlist;
  end;     }

var
  Form1: TForm1;
  VTE: TVTETerminal;
  //VTE: TBlindTerminal;
  Cancelled: Boolean;
  embedTerm: TGtkTerm;


var
  O, E: TPipeReader;


implementation



{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  S : string;
begin
  S := Edit1.Text;
  VTE := TVTEterminal.Create(PChar(s));//('dar -l /home/malcolm/test -v');//('ls -a');
  //VTE := TVTETerminal.Create(nil);
  VTE.Caption := Edit1.Text;
  Cancelled   := false;
  VTE.OnClose := @VTETerminalClose;
  VTE.OnCancel := @CancelCallback;
  VTE.OnProcessExit := @VTECallback;
  //VTE.Width := 1000;
  //VTE.Height := 150;
  //vte.WindowState := wsMaximized;
  //VTE.OnStdout:=@TestOutput;
  //VTE.OnStderr:=@TestOutput;
  //VTE.StdPipe:='/tmp/stdout';
  //VTE.ErrPipe:='/tmp/stderr';
  //VTE.ShowModal(GDK_WINDOW_XWINDOW(GetControlWindow(PGtkWidget(Self.Handle))));
  //FormID := GDK_WINDOW_XWINDOW(GetControlWindow(PGtkWidget(Self.Handle)));
  //vte.ShowModal(GDK_WINDOW_XWINDOW( pointer( PGtkWidget(Form1.Handle)^.Window ) ));
  //VTE.ShowModal(Self);
  VTE.Show;
  //Vte.Background := clCream;
  //VTE.Foreground := $010000;
  VTE.Execute;
 //  writeln('xwindow: ',  );

  //BTE := TBlindTerminal.Create;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //BTE.Free;
  VTE.Free;
  if O <> nil then O.Terminate;
  if E <> nil then E.Terminate;
  O := nil;
  E := nil;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  buf: string;
  bytes: LongInt;
  s: String;
begin
  //VTE.Command := Edit1.Text;
  s := Edit1.Text;
  VTE.ExecuteCommand(s);
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

procedure TForm1.Button4Click(Sender: TObject);
begin
  VTE.SendNewline;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  VTE.Width := 120;
 // VTE.Transparent := true;
 // ShowMessage(IntToStr(Red(clRed)));
 VTE.Background := Panel1.Color;
 VTE.Foreground := Panel2.Color;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  VTE.SendEscape;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  //FpMkfifo('/tmp/test.fifo', StrToInt(Edit1.text));
  //VTE.Print(#27 + '[31mThis is red' + #27 + '[m');
  VTE.Cols := VTE.Cols-4;
end;

procedure TForm1.Button8Click ( Sender: TObject ) ;
var
  L: PGList;
  t: string;
begin
  t := Edit1.Text;
  embedTerm := TGtkTerm.Create(nil);
  try

  L := gtk_container_get_children(PGtkContainer(ScrollBox1.Handle));
//  gtk_widget_destroy(PGtkWidget(L^.data));
  gtk_container_add(GTK_CONTAINER(L^.data)  , PGtkWidget( embedTerm.Vte));
  gtk_widget_show(PGtkWidget( embedTerm.Vte));
  //embedTerm.Execute;
  ScrollBox1.Width := 500;
  writeln(gtk_widget_get_name(PGtkWidget( Panel3.Handle)));
  embedTerm.SetFocus;
  except
    embedTerm.Free;
  end;
end;

procedure TForm1.FormClose ( Sender: TObject; var CloseAction: TCloseAction ) ;
begin
  if VTE<>nil then VTE.Free;
  if embedTerm<>nil then embedTerm.Free;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then TPanel(Sender).Color := ColorDialog1.Color;
end;

procedure TForm1.VTECallback(Sender: TObject; exitcode: integer);
begin
  if not Cancelled then
  begin
    VTE.Print(#27 + '[31m-------------------------------------------' + #27 + '[m' + #10#13);
    VTE.Print(#27 + '[31mDar has terminated' + #27 + '[m'+#10#13);
  end;
end;

function TForm1.CancelCallback ( Sender: TObject ) : Boolean;
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

function TForm1.VTETerminalClose ( Sender: TObject ): Boolean ;
begin
  VTE.Free;
  VTE := nil;
  Result := true;
end;

procedure TForm1.TestOutput(Sender: TObject; output: string);
begin
  writeln(output);
end;



initialization
  {$I unit1.lrs}

end.

