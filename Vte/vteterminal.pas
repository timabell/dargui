unit VTETerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, gtk2, gdk2, glib2, vte, Graphics, baseunix;

type

 TPipeOutputEvent = procedure(Sender: TObject; Output: string) of object;
 TProcessExitEvent = procedure(Sender: TObject; ExitCode: integer) of object;

  { TPipeReader }

  TPipeReader = class(TThread)
    private
      opipe: TFileStream;
      fOutPut : string;
      fPipename: TFilename;
      fOnOutput: TPipeOutputEvent;
      procedure ShowOutput;
    protected
      procedure Execute; override;
    public
      Constructor Create(CreateSuspended : boolean; pipename: TFilename);
      destructor Destroy; override;
      property OnOutput: TPipeOutputEvent read fOnOutput write fOnOutput;
      //property PipeEOF: Boolean read fEOF;
    end;


 { TCustomVTETerminal }

 {
  To run one program and get callback when it terminates
     - Visible Terminal: use TVTETerminal and call Show before Execute
     - Invisible Terminal: use TBlindTerminal
     - Create with program and arguments as a parameter
     - set other properties
     - call Execute
     - OnProcessExit will be called when the program terminates
     - Output and ErrorOutput are ignored in this mode

  To run a program and send output to pipes
     - Create TVTETerminal with parameter nil
     - Set Output, ErrorOutput and other properties as necessary
     - do _not_ call Show
     - call ExecuteCommand with program commandline as parameter
     - OnProcessExit will be called when the program terminates
     - OnStdOut and OnStdErr events can be used to read output from the pipes

  To create a GtkTerm clone
     - create a TVTETerminal with parameter nil
     - call Show
     - OnClose will be called if the user closes the window
 }

 TCustomVTETerminal = class ( TObject )
    Vte: PGtkWidget;
  private
    fCommand: string;
    fOutput: TFilename;
    fStdErr: TFilename;
    fStdoutReader: TPipeReader;
    fStderrReader: TPipeReader;
    fOnStdout: TPipeOutputEvent;
    fOnStdErr: TPipeOutputEvent;
    procedure SetErrorOutput(AValue: TFilename);
    procedure SetOutput(AValue: TFilename);
    function fGetEOF: Boolean;
    FOnProcessExit: TProcessExitEvent;
  public
    constructor Create ( Commandline: PChar );
    destructor Destroy; override;
    procedure ExecuteCommand( Commandline: string );
    procedure Execute;
    procedure SendNewline;
    procedure SendEscape;
    property Command: string read fCommand;
    property StdPipe: TFilename read fOutput write SetOutput;
    property ErrPipe: TFilename read fStdErr write SetErrorOutput;
    property OnProcessExit: TProcessExitEvent read FOnProcessExit write FOnProcessExit;

    //OnStdout is fired when a line of input is read from the fOutput pipe
    property OnStdout: TPipeOutputEvent read fOnStdout write fOnStdout;

    //OnStderr is fired when a line of input is read from the fStdErr pipe
    property OnStderr: TPipeOutputEvent read fOnStderr write fOnStderr;
    property EOF: Boolean read fGetEOF;
 end;

 { TVTETerminal }

 TVTETerminal = class ( TCustomVTETerminal )
  private
    VteWindow: PGtkWidget;
    vBox: PGtkWidget;
    fCaption: string;
    fCols: integer;
    fRows: integer;
    fAudibleBell: boolean;
    fTransparent: Boolean;
    fBackgroundColor: TColor;
    fForegroundColor: TColor;
    fOnDestroy: TNotifyEvent;
    procedure fSetHeight(Value: integer);
    procedure fSetWidth(Value: integer);
    procedure fSetAudibleBell(Value: Boolean);
    procedure fSetTransparent(Value: Boolean);
    procedure SetBackground(const AValue: TColor);
    procedure SetCaption(const AValue: string);
    procedure SetForeground(const AValue: TColor);
  public
    constructor Create( Commandline: PChar );
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure Print( Msg: string );
    procedure SendNewline;
    procedure SendEscape;
    procedure PasteFromClipboard;
    procedure CopyToClipboard;
    property Width: integer read fCols write fSetWidth;
    property Height: integer read fRows write fSetHeight;
    property AudibleBell: boolean read fAudibleBell write fSetAudibleBell;
    property Transparent: Boolean read fTransparent write fSetTransparent;
    property Foreground: TColor read fForegroundColor write SetForeground;
    property Background: TColor read fBackgroundColor write SetBackground;
    property Caption: string read fCaption write SetCaption;
    property OnClose: TNotifyEvent read FOnDestroy write FOnDestroy;
    end;

  { TBlindTerminal }

  TBlindTerminal = class (TCustomVTETerminal );


  procedure VTE_child_exit_callback( Term: TObject);
  function VTE_window_destroy_callback(Term: TObject): gboolean;

  function GetGdkColor( Color: TColor ): TGdkColor;

implementation

procedure VTE_child_exit_callback( Term: TObject);
begin
  if Assigned(TVTETerminal(Term).OnProcessExit)
     then TVTETerminal(Term).OnProcessExit(TVTETerminal(Term),
           vte_terminal_get_child_exit_status  (PVteTerminal(TVTETerminal( Term).Vte))) ;
end;

function VTE_window_destroy_callback(Term: TObject): gboolean;
begin
  if Assigned(TVTETerminal(Term).OnClose)
     then TVTETerminal(Term).OnClose(Term) ;
  Result := TRUE;
end;

function GetGdkColor(Color: TColor): TGdkColor;
var
  c: TGdkColor;
begin
  c.blue := Blue(Color)*Blue(Color);
  c.red := Red(Color)*Red(Color);
  c.green := Green(Color)*Green(Color);
  c.pixel := 0;
  Result := c;
end;

{ TPipeReader }

procedure TPipeReader.ShowOutput;
begin
    if Assigned(fOnOutput) then fOnOutput(Self, fOutPut);
end;

procedure TPipeReader.Execute;
  var
    newStatus : string;
    stdoutput: string;
    bytesread: LongInt;

  begin
    while not FileExists(fPipename) do Application.ProcessMessages;
    opipe := TFileStream.Create(fPipename, fmOpenRead and fmShareDenyNone);
    SetLength(stdoutput, 4096);
    while (not Terminated) do
      begin
        bytesread := opipe.Read(stdoutput[1], 4096);
        if bytesread > 0 then
           begin
             fOutPut := copy(stdoutput, 1, bytesread);
             Synchronize(@ShowOutput);
           end;
      end;
end;

constructor TPipeReader.Create(CreateSuspended : boolean; pipename: TFilename);
begin
    FreeOnTerminate := True;
    fPipename := pipename;
    inherited Create(CreateSuspended);
end;

destructor TPipeReader.Destroy;
begin
   if Assigned(opipe) then opipe.Free;
   if FileExists(fPipename) then sysutils.DeleteFile(fPipename);
  inherited Destroy;
end;

{ TCustomVTETerminal }

procedure TCustomVTETerminal.SetErrorOutput(AValue: TFilename);
begin
  AValue := Trim(AValue);
  if (AValue='') or (AValue=fStdErr) then exit;
  if (AValue[1] = DirectorySeparator)
     and (DirectoryExists(ExtractFilePath(AValue)))
  then
      try
        if fStdErr<>'' then Sysutils.DeleteFile(fStdErr);
        FpMkfifo(AValue, fmCreate and fmOutput);
        fStdErr := AValue;
      except
        fStdErr := '';
      end
  else Raise Exception.Create('SetErrorOutput: Output directory not found');
end;

procedure TCustomVTETerminal.SetOutput(AValue: TFilename);
begin
  AValue := Trim(AValue);
  if AValue='' then exit;
  if (AValue[1] = DirectorySeparator)
     and (DirectoryExists(ExtractFilePath(AValue)))
  then
      try
        if fOutput<>'' then Sysutils.DeleteFile(fOutput);
        FpMkfifo(AValue, fmCreate and fmOutput);
        fOutput := AValue;
      except
        fOutput := '';
      end
  else Raise Exception.Create('SetOutput: Output directory not found');
end;

 function TCustomVTETerminal.fGetEOF: Boolean;
 begin
   Result := false;
   //if Assigned(fStderrReader) then Result := fStderrReader.PipeEOF;
   //if Assigned(fStdoutReader) then Result := (Result and fStdoutReader.PipeEOF);
 end;

constructor TCustomVTETerminal.Create(CommandLine: PChar);
begin
  inherited Create;
  Vte := vte_terminal_new;
  fCommand := CommandLine;
  fOutput := '';
  fStdErr := '';
end;

destructor TCustomVTETerminal.Destroy;
begin
  if Vte <> nil then gtk_widget_destroy(Vte);
  if fStderrReader <> nil then fStderrReader.Terminate;
  if fStdoutReader <> nil then fStdoutReader.Terminate;
  inherited Destroy;
end;

procedure TCustomVTETerminal.ExecuteCommand( Commandline: string );
var
  C, P: string;
begin
  if Length(Commandline) < 1 then exit;
  fStderrReader := nil;
  fStdoutReader := nil;
  if fOutput <> '' then
     begin
       if not FileExists(fOutput) then
           if FpMkfifo(fOutput, 438)<>0 then writeln('pipe creation failed: ',foutput);
       if FileExists(fOutput) then
          begin
            Commandline := Commandline + ' 1>|' + fOutput;
            fStdoutReader := TPipeReader.Create(true, fOutput);
            if Assigned(fOnStdout) then
               fStdoutReader.OnOutput := fOnStdout;
          end;
     end;
  if fStdErr <> '' then
     begin
       if not FileExists(fStdErr) then
            if FpMkfifo(fStdErr, 438)<>0 then writeln('pipe creation failed: ',fStdErr);
       if FileExists(fStdErr) then
          begin
            Commandline := Commandline + ' 2>|' + fStdErr;
            fStderrReader := TPipeReader.Create(true, fStdErr);
            if Assigned(fOnStdErr) then
                fStderrReader.OnOutput := fOnStdErr;
          end;
     end;
  if Assigned(OnProcessExit) then Commandline := Commandline + '; exit $?' ;
  C := 'unset HISTFILE;' + Commandline + #10;
//  writeln('executing ', C);
  if Length(C) > 1
     then  vte_terminal_feed_child(Vte, PChar(C), Length(C)+1);
  if fStdoutReader <> nil then fStdoutReader.Resume;
  if fStderrReader <> nil then fStderrReader.Resume
end;

{ This is how to output to the pipes
     dar -l /home/malcolm/test -v 1>|/tmp/stdout 2>|/tmp/stderr
}

{ And this is how to output stderr and stdout to two separate apps
     { dar -l /home/malcolm/test -v | app1; } 3>&1 1>&2 2>&3 | app2

}

procedure TCustomVTETerminal.Execute;
var
  A, C: string;
  Args: PPChar;
  x: integer;
begin
  if fCommand='' then
     begin
       C := 'No command defined';
       vte_terminal_feed(Vte, PChar(C), Length(C));
       SendNewline;
       exit;
     end;
  Args := nil;
  x := Pos(#32, fCommand);
  if x > 0 then
     begin
       C := Copy(fCommand, 1, x-1);
       A := Command;
       Args := StringToPPChar(A, 0);
     end;
  vte_terminal_fork_command(Vte, PChar(C), Args, nil, nil, false, false, false);
end;

procedure TCustomVTETerminal.SendNewline;
var
  CR: PChar;
begin
  CR := #10;
  vte_terminal_feed_child_binary(Vte, CR, 1);
end;

procedure TCustomVTETerminal.SendEscape;
var
  ESC: PChar;
begin
  ESC := #27;
  vte_terminal_feed_child_binary(Vte, ESC, 1);
end;


{ TVTETerminal }

procedure TVTETerminal.fSetHeight(Value: integer);
begin
  if Value = fRows then exit;
  vte_terminal_set_size(Vte, fCols, Value);
  fRows := Value;
end;

procedure TVTETerminal.fSetWidth(Value: integer);
begin
  if Value = fCols then exit;
  vte_terminal_set_size(Vte, Value, fRows);
  fCols := Value;
end;

procedure TVTETerminal.fSetAudibleBell(Value: Boolean);
begin
  if Value = fAudibleBell then exit;
  vte_terminal_set_audible_bell(Vte, Value);
  fAudibleBell := Value;
end;

procedure TVTETerminal.fSetTransparent(Value: Boolean);
begin
  vte_terminal_set_background_transparent(Vte, Value);
end;

procedure TVTETerminal.SetBackground(const AValue: TColor);
var
  c: TGdkColor;
begin
  if AValue=fBackgroundColor then exit;
  c := GetGdkColor(AValue);
  vte_terminal_set_color_background(VTE, @c);
end;

procedure TVTETerminal.SetCaption(const AValue: string);
begin
  if AValue=fCaption then exit;
  gtk_window_set_title(PgtkWindow(VteWindow), Pgchar(AValue));
end;

procedure TVTETerminal.SetForeground(const AValue: TColor);
var
  c: TGdkColor;
begin
  if AValue=fForegroundColor then exit;
  c := GetGdkColor(AValue);
  vte_terminal_set_color_foreground(VTE, @c);
end;

constructor TVTETerminal.Create(Commandline: PChar);
begin
  inherited Create( Commandline );
  VteWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect(pGtkObject(VteWindow), 'delete-event',
                                        GTK_SIGNAL_FUNC (@VTE_window_destroy_callback), Self);
  gtk_container_set_border_width(GTK_CONTAINER(VteWindow), 5);
  vBox := gtk_vbox_new(true,5);
  gtk_box_set_homogeneous(PGtkBox( vBox ), false);
  gtk_container_add(GTK_CONTAINER(VteWindow), vBox);
  gtk_signal_connect(pGtkObject(Vte), 'child-exited',
                                        GTK_SIGNAL_FUNC (@VTE_child_exit_callback), Self);
  fCols := vte_terminal_get_column_count(Vte);
  fRows := vte_terminal_get_row_count(Vte);
  gtk_box_pack_start(PGtkBox(vBox),Vte, true, true, 5);
  if CommandLine=nil then
     vte_terminal_fork_command(Vte, nil, nil, nil, nil, false, false, false);

  Caption := 'TVTETerminal';
end;

destructor TVTETerminal.Destroy;
begin
  inherited Destroy;
//  writeln('about to destroy vBox');
  gtk_widget_destroy(vBox);
//  writeln('about to destroy VTEWindow');
  gtk_widget_destroy(VteWindow);
//  writeln('OK: done');
end;

procedure TVTETerminal.Show;
begin
  gtk_widget_show_all(VteWindow);
end;

procedure TVTETerminal.Hide;
begin
  gtk_widget_hide(VteWindow);
end;

//Print output on the terminal which will not be interpreted by bash
procedure TVTETerminal.Print(Msg: string);
begin
  vte_terminal_feed(Vte, PChar(Msg), Length(Msg));
  SendNewline;
end;

procedure TVTETerminal.SendNewline;
var
  CR: PChar;
begin
  CR := #10;
  vte_terminal_feed_child_binary(Vte, CR, 1);
end;

procedure TVTETerminal.SendEscape;
var
  CR: PChar;
begin
  CR := #27;
  vte_terminal_feed_child_binary(Vte, CR, 1);
end;

procedure TVTETerminal.PasteFromClipboard;
begin
  vte_terminal_paste_clipboard(Vte);
end;

procedure TVTETerminal.CopyToClipboard;
begin
  vte_terminal_copy_clipboard(Vte);
end;


end.

