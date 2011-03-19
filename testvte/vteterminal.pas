unit VTETerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, gtk2, gdk2, gdk2x, glib2, vte, x, Graphics, baseunix, LCLType;

type

 TPipeOutputEvent = procedure(Sender: TObject; Output: string) of object;
 TProcessExitEvent = procedure(Sender: TObject; ExitCode: integer) of object;
 TWindowStateEvent = procedure(Sender: TObject; GtkWindowState: integer) of object;
 TCancelEvent = function(Sender: TObject) : Boolean of object;

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
    fCols: integer;
    fRows: integer;
    fWidth: Integer;
    fHeight: Integer;
    fVTECellWidth: integer;
    fVTECellHeight: integer;
    fVTEHeightPixels: Integer;
    fVTEWidthPixels: Integer;
    fButtonBarHeightPixels: Integer;
    fAudibleBell: boolean;
    fTransparent: Boolean;
    fBackgroundColor: TColor;
    fForegroundColor: TColor;
    fOnDestroy: TCancelEvent;
    fChildPid: pid_t;
    fCommand: string;
    fOutput: TFilename;
    fStdErr: TFilename;
    fStdoutReader: TPipeReader;
    fStderrReader: TPipeReader;
    fOnStdout: TPipeOutputEvent;
    fOnStdErr: TPipeOutputEvent;
    fOnProcessExit: TProcessExitEvent;
    fOnResize: TNotifyEvent;
    procedure SetErrorOutput(AValue: TFilename);
    procedure SetOutput(AValue: TFilename);
    function fGetEOF: Boolean;
    procedure fSetCols(Value: integer);
    procedure fSetRows(Value: integer);
    procedure fSetAudibleBell(Value: Boolean);
    procedure fSetTransparent(Value: Boolean);
    procedure SetForeground(const AValue: TColor);
    procedure SetBackground( AValue: TColor);
    procedure ResizeVTEtofit(maxwidth, maxheight: integer);
  protected
    procedure DoSizeAllocate(terminalwidth, terminalheight: integer); virtual;
    procedure DoProcessExit;  virtual;
  public
    constructor Create ( Commandline: PChar ); virtual;
    destructor Destroy; override;
    procedure ExecuteCommand( Commandline: string );
    procedure Execute;
    procedure TerminateProcess( sig: integer );
    procedure Print( Msg: string );
    procedure SendNewline;
    procedure SendEscape;
    procedure PasteFromClipboard;
    procedure CopyToClipboard;
  published
    property Cols: integer read fCols write fSetCols;
    property Rows: integer read fRows write fSetRows;
    property ChildPid : pid_t read fChildPid write fChildPid;
    property Command: string read fCommand;
    property StdPipe: TFilename read fOutput write SetOutput;
    property ErrPipe: TFilename read fStdErr write SetErrorOutput;
    property OnProcessExit: TProcessExitEvent read fOnProcessExit write fOnProcessExit;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;

    //OnStdout is fired when a line of input is read from the fOutput pipe
    property OnStdout: TPipeOutputEvent read fOnStdout write fOnStdout;

    //OnStderr is fired when a line of input is read from the fStdErr pipe
    property OnStderr: TPipeOutputEvent read fOnStderr write fOnStderr;
    property EOF: Boolean read fGetEOF;
    property AudibleBell: boolean read fAudibleBell write fSetAudibleBell;
    property Transparent: Boolean read fTransparent write fSetTransparent;
    property Foreground: TColor read fForegroundColor write SetForeground;
    property Background: TColor read fBackgroundColor write SetBackground;
 end;

 { TVTETerminal }

 TVTETerminal = class ( TCustomVTETerminal )
  private
    fOnCancel: TCancelEvent;
    VteWindow: PGtkWidget;
    fGtkLayout: PGtkWidget;
    vBox: PGtkWidget;
    buttonBox: PGtkWidget;
    cancelButton: PGtkWidget;
    closeButton: PGtkWidget;
    fCaption: string;
    fWindowStateChange: TWindowStateEvent;
    FwindowState: byte;
    function GetWindowState: TWindowState;
    procedure SetCaption( AValue: string);
    procedure setHeight ( const AValue: integer ) ;
    procedure setWidth ( const AValue: integer ) ;
    procedure setWindowState ( const AValue: TWindowState ) ;
  protected
    procedure DoProcessExit; override;
  public
    constructor Create( Commandline: PChar );   override;
    destructor Destroy; override;
    procedure Show;
    procedure ShowModal( ParentWindow: TForm );
    procedure Hide;
    procedure EnableButton(Button: PGtkWidget; Enable: boolean);
    property Caption: string read fCaption write SetCaption;
    property OnClose: TCancelEvent read FOnDestroy write FOnDestroy; // if function returns true then window is allowed to close
    property OnCancel: TCancelEvent read fOnCancel write fOnCancel;
    property OnResize: TNotifyEvent read fOnResize write fOnresize;
    property OnWindowStateChange: TWindowStateEvent read fWindowStateChange write fWindowStateChange;
    property WindowState: TWindowState read GetWindowState write setWindowState;
    property Width: integer read fWidth write setWidth;
    property Height: integer read fHeight write setHeight;
    end;

  { TBlindTerminal }

  TBlindTerminal = class (TCustomVTETerminal );

const
  GTK_WINDOW_STATE_NORMAL        = 0; //The window is shown normally.
  GTK_WINDOW_STATE_WITHDRAWN     = 1; //The window is not shown.
  GTK_WINDOW_STATE_ICONIFIED     = 2;  //The window is minimized.
  GTK_WINDOW_STATE_MAXIMIZED     = 4;  //The window is maximized.
  GTK_WINDOW_STATE_STICKY        = 8;  //The window is sticky.
  GTK_WINDOW_STATE_FULLSCREEN    = 16; //The window is maximized without decorations
  GTK_WINDOW_STATE_ABOVE         = 32; //The window is kept above other windows.
  GTK_WINDOW_STATE_BELOW         = 64; //The window is kept below other windows.

  WINDOW_PADDING                 = 5;
  PAD_COUNT                      = 1;


  { TCustomVTETerminal callbacks }
  procedure CustomVTE_child_exit_callback( Term: TObject);
  procedure CustomVTE_sizerequest_callback( VteWidget: PGtkWidget; requ: PGtkRequisition; Term: TObject); cdecl;

  { TVTETerminal callbacks }
  function VTE_window_destroy_callback( Window: PGtkWidget; Event: PGdkEvent; Term: TObject): gboolean; cdecl;
  function VTE_window_state_change_callback( Window: PGtkWidget; Event: PGdkEvent; Term: TObject): gboolean; cdecl;
  procedure cancelButtonClick( button: PGtkWidget;  Term: TObject ); cdecl;
  procedure closeButtonClick( button: PGtkWidget;  Term: TObject ); cdecl;
  procedure window_sizeallocate_callback( Window: PGtkWidget; allocation: PGdkRectangle; Term: TObject); cdecl;
  procedure buttonbox_sizeallocate_callback( bBox: PGtkWidget; allocation: PGdkRectangle; Term: TObject); cdecl;

  function GetGdkColor( Color: TColor ): TGdkColor;

implementation

{ TCustomVTETerminal callbacks }

{ NOTE: This function does not work with libvte v0.16 [ < 0.20 ? ] }
procedure CustomVTE_child_exit_callback( Term: TObject);
begin
  if Term is TCustomVTETerminal then TCustomVTETerminal(Term).DoProcessExit;
end;

// This is the only way I can find of getting the default dimensions in pixels
// and calculating the dimensions of the cells
// NOTE: changes to the font used may invalidate this data
procedure CustomVTE_sizerequest_callback ( VteWidget: PGtkWidget;
  requ: PGtkRequisition; Term: TObject ) ; cdecl;
begin
  writeln('VTE size request: ', requ^.width,'x',requ^.height);
  if Term is TCustomVTETerminal then TCustomVTETerminal(Term).DoSizeAllocate(requ^.width, requ^.height);
end;


{ TVTETerminal callbacks }

// triggered if user attempts to close window using the titlebar button
function VTE_window_destroy_callback( Window: PGtkWidget; Event: PGdkEvent; Term: TObject): gboolean;  cdecl;
begin
  Result := false;  // allow close by default
  if not (Term is TVTETerminal) then exit;
  writeln('window destroy signal ');
  if Assigned(TVTETerminal(Term).OnClose)
     then Result := not TVTETerminal(Term).OnClose(Term) ; // if function returns true then window is allowed to close
end;

//can be used to react to changes in window state
function VTE_window_state_change_callback ( Window: PGtkWidget;
  Event: PGdkEvent; Term: TObject ) : gboolean; cdecl;
var
  WindowState: TGdkEventWindowState;
begin
  WindowState := Event^.window_state;
writeln('New window state: ',  WindowState.new_window_state);
  if Assigned(TVTETerminal(Term).OnWindowStateChange)
     then TVTETerminal(Term).OnWindowStateChange(Term, WindowState.new_window_state) ;
  result := true;
end;

// provides a hook for code to respond to cancelButton being pressed
procedure cancelButtonClick( button: PGtkWidget;  Term: TObject ); cdecl;
begin
  writeln('cancel', TVTETerminal(Term).ChildPid );
  if Assigned(TVTETerminal(Term).OnCancel) then
     TVTETerminal(Term).EnableButton(button, not TVTETerminal(Term).OnCancel(Term));
end;

// provides a hook for code to respond to closeButton being pressed
procedure closeButtonClick( button: PGtkWidget;  Term: TObject ); cdecl;
begin
  if not (Term is TVTETerminal) then exit;
  if Assigned(TVTETerminal(Term).OnClose)
     then TVTETerminal(Term).OnClose(Term) ;
end;

procedure window_sizeallocate_callback ( Window: PGtkWidget;
  allocation: PGdkRectangle; Term: TObject ) ; cdecl;
var
  w: Integer;
begin
  if Term <> nil then
  begin
    TVTETerminal(Term).fHeight := allocation^.height;
    TVTETerminal(Term).fWidth := allocation^.width;
    if Assigned(TVTETerminal(Term).fOnResize) then
       TVTETerminal(Term).fOnResize(Term);
    TVTETerminal(Term).ResizeVTEtofit(allocation^.width, allocation^.height);
  end;
  w := allocation^.width;
  writeln('window allocation ', w, 'x', allocation^.height);
end;

procedure buttonbox_sizeallocate_callback ( bBox: PGtkWidget;
  allocation: PGdkRectangle; Term: TObject ) ; cdecl;
begin
  if TVTETerminal(Term).fButtonBarHeightPixels <1 then
     begin
      TVTETerminal(Term).fButtonBarHeightPixels := allocation^.height;
      writeln('buttonBox size allocation: ', allocation^.width,'x',allocation^.height);
      gtk_window_resize(PGtkWindow( TVTETerminal(term).VteWindow ), TVTETerminal(term).fVTEWidthPixels, TVTETerminal(term).fVTEHeightPixels + allocation^.height);
     end;
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
    stdoutput: string;
    bytesread: LongInt;

begin
  while not FileExists(fPipename) do Application.ProcessMessages;
  opipe := TFileStream.Create(fPipename, fmOpenRead and fmShareDenyNone);
  writeln('opened pipe ', fPipename);
  SetLength(stdoutput, 2048);
  while (not Terminated) do
    begin
      bytesread := opipe.Read(stdoutput[1],2048);
      if bytesread> 0 then
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
   writeln(fPipename , ' thread closing');
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

  procedure TCustomVTETerminal.fSetCols ( Value: integer ) ;
 begin
   if Value = fCols then exit;
   vte_terminal_set_size(Vte, Value, fRows);
   fCols := Value;
   if Assigned(fOnResize)
      then fOnResize(Self);
 end;

procedure TCustomVTETerminal.fSetRows ( Value: integer ) ;
 begin
  if Value = fRows then exit;
  vte_terminal_set_size(Vte, fCols, Value);
  fRows := Value;
  if Assigned(fOnResize)
      then fOnResize(Self);
end;

procedure TCustomVTETerminal.DoSizeAllocate ( terminalwidth,
  terminalheight: integer ) ;
  begin
   if fVTEHeightPixels < 1 then    // code executed only on first callback
   begin
     //gtk_window_resize(PGtkWindow( TVTETerminal(term).VteWindow ), requ^.width,
     //          allocation^.height + TVTETerminal(term).fButtonBarHeightPixels + (WINDOW_PADDING*PAD_COUNT));
     fVTEHeightPixels := terminalheight;
     fVTEWidthPixels := terminalwidth;
     fVTECellHeight := fVTEHeightPixels div fRows;
   writeln('CellHeight: ', fVTECellHeight);
     fVTECellWidth := fVTEWidthPixels div fCols;
   writeln('CellWidth: ', fVTECellWidth);
   end;

  end;

procedure TCustomVTETerminal.DoProcessExit;
begin
  exitcode := vte_terminal_get_child_exit_status( Vte ) shr 8;
  writeln('exit signal: ', exitcode);
  if Assigned(OnProcessExit)
     then OnProcessExit(Self, exitcode) ;
end;

constructor TCustomVTETerminal.Create(CommandLine: PChar);
begin
  inherited Create;
  fChildPid := 0;
  Vte := vte_terminal_new;
  fCommand := CommandLine;
  fOutput := '';
  fStdErr := '';
  fCols := vte_terminal_get_column_count(Vte);
  writeln('Cols: ', fcols);
  fRows := vte_terminal_get_row_count(Vte);
  writeln('Rows: ', fRows);
  gtk_signal_connect(GTK_OBJECT(Vte), 'size-request',
		                   GTK_SIGNAL_FUNC(@CustomVTE_sizerequest_callback), Self);
  gtk_signal_connect(pGtkObject(Vte), 'child-exited',
                                     GTK_SIGNAL_FUNC (@CustomVTE_child_exit_callback), Self);
 if CommandLine=nil then
    ChildPid := vte_terminal_fork_command(Vte, nil, nil, nil, nil, false, false, false);
end;

destructor TCustomVTETerminal.Destroy;
begin
  if Vte <> nil then begin gtk_widget_destroy(Vte);writeln('Vte destroyed in TCustomVTETerminal.Destroy');end;
  if fStderrReader <> nil then fStderrReader.Terminate;
  if fStdoutReader <> nil then fStdoutReader.Terminate;
  inherited Destroy;
end;

procedure TCustomVTETerminal.ExecuteCommand( Commandline: string );
var
  C: string;
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
  C := Commandline + #10;
  writeln('executing ', C);
  if Length(C) > 1
     then  vte_terminal_feed_child(Vte, PChar(C), Length(C)+1);
  if fStdoutReader <> nil then fStdoutReader.Resume;
  if fStderrReader <> nil then fStderrReader.Resume;
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
       A := fCommand;
       C := Copy(fCommand, 1, x-1);
       Args := StringToPPChar(A, 0);
     end
  else C := fCommand;
  fChildPid := vte_terminal_fork_command(Vte, PChar(C), Args, nil, nil, false, false, false);
  writeln('pid: ', fChildPid);
  Freemem(Args);
end;

procedure TCustomVTETerminal.TerminateProcess ( sig: integer ) ;
begin
  if fChildPid > 0
     then FpKill(fChildPid, sig);
  fChildPid := 0;
end;

procedure TCustomVTETerminal.fSetAudibleBell(Value: Boolean);
begin
  if Value = fAudibleBell then exit;
  vte_terminal_set_audible_bell(Vte, Value);
  fAudibleBell := Value;
end;

procedure TCustomVTETerminal.fSetTransparent(Value: Boolean);
begin
  vte_terminal_set_background_transparent(Vte, Value);
end;

procedure TCustomVTETerminal.SetBackground(AValue: TColor);
var
  c: TGdkColor;
begin
  if AValue=fBackgroundColor then exit;
  writeln('new background: ', AValue);
  c := GetGdkColor(AValue);
  vte_terminal_set_color_background(VTE, PGdkColor( @c ));
  fBackgroundColor := AValue;
end;

procedure TCustomVTETerminal.SetForeground(const AValue: TColor);
var
  c: TGdkColor;
begin
  if AValue=fForegroundColor then exit;
  writeln('new foreground: ', AValue);
  c := GetGdkColor(AValue);
  vte_terminal_set_color_foreground(VTE, @c);
  fForegroundColor := AValue;
end;

procedure TCustomVTETerminal.Print(Msg: string);
begin
  vte_terminal_feed(Vte, PChar(Msg), Length(Msg));
  SendNewline;
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
  CR: PChar;
begin
  CR := #27;
  vte_terminal_feed_child_binary(Vte, CR, 1);
end;

procedure TCustomVTETerminal.PasteFromClipboard;
begin
  vte_terminal_paste_clipboard(Vte);
end;

procedure TCustomVTETerminal.CopyToClipboard;
begin
  vte_terminal_copy_clipboard(Vte);
end;

procedure TCustomVTETerminal.ResizeVTEtofit ( maxwidth, maxheight: integer ) ;
var
  targetCols: LongInt;
  targetRows: LongInt;
begin
  targetCols := maxwidth div fVTECellWidth;
  targetRows := (maxheight-fButtonBarHeightPixels-(WINDOW_PADDING*PAD_COUNT)) div fVTECellHeight;
  Cols := targetCols;
  Rows := targetRows;
//  vte_terminal_set_size(PVteTerminal(Vte);
end;



{ TVTETerminal }

function TVTETerminal.GetWindowState: TWindowState;
begin
  case FwindowState of
       GTK_WINDOW_STATE_NORMAL: Result := wsNormal;
       GTK_WINDOW_STATE_ICONIFIED: Result := wsMinimized;
       GTK_WINDOW_STATE_MAXIMIZED: Result := wsMaximized;
       else
         Result := wsNormal;
  end;
end;

procedure TVTETerminal.SetCaption(AValue: string);
begin
  if AValue=fCaption then exit;
  gtk_window_set_title(PgtkWindow(VteWindow), Pgchar(AValue));
end;

procedure TVTETerminal.setHeight ( const AValue: integer ) ;
begin
  if AValue<>fHeight then
     begin
       gtk_window_resize(PGtkWindow( VteWindow ), fWidth, AValue);
       fHeight := AValue;
     end;
end;

procedure TVTETerminal.setWidth ( const AValue: integer ) ;
begin
  if AValue<>fWidth then
     begin
       gtk_window_resize(PGtkWindow( VteWindow ), AValue, fHeight);
       fWidth := AValue;
     end;
end;

procedure TVTETerminal.setWindowState ( const AValue: TWindowState ) ;
begin
  if AValue=GetWindowState then exit;
  case AValue of
       wsNormal: begin
                   if FwindowState=GTK_WINDOW_STATE_ICONIFIED
                        then gtk_window_deiconify( PGtkWindow( VteWindow) )
                   else if FwindowState=GTK_WINDOW_STATE_MAXIMIZED
                        then gtk_window_unmaximize( PGtkWindow( VteWindow) );
                   FwindowState := GTK_WINDOW_STATE_NORMAL;
                 end;
       wsMinimized: begin FwindowState := GTK_WINDOW_STATE_ICONIFIED; gtk_window_iconify( PGtkWindow( VteWindow) );  end;
       wsMaximized: begin FwindowState := GTK_WINDOW_STATE_MAXIMIZED; gtk_window_maximize( PGtkWindow( VteWindow) );  end;
       else
         exit;
  end;
end;

procedure TVTETerminal.DoProcessExit;
begin
  inherited DoProcessExit;
  EnableButton(closeButton, true);
  EnableButton(cancelButton, False);
end;

constructor TVTETerminal.Create(Commandline: PChar);
begin
  inherited Create( Commandline );
  VteWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size(PGtkWindow( VteWindow ), 400,200);
  g_signal_connect( VteWindow , 'delete-event',
                                        G_CALLBACK (@VTE_window_destroy_callback), Self);
  gtk_signal_connect(GTK_OBJECT(VteWindow), 'window-state-event',
		                   GTK_SIGNAL_FUNC(@VTE_window_state_change_callback), Self);
  gtk_signal_connect(GTK_OBJECT(VteWindow), 'size-allocate',
		                   GTK_SIGNAL_FUNC(@window_sizeallocate_callback), Self);
  vBox := gtk_vbox_new(false, 0);
  fGtkLayout := gtk_layout_new(nil, nil);
  gtk_container_add(GTK_CONTAINER(VteWindow), fGtkLayout);
  vte_terminal_set_scrollback_lines(Vte, 500);
  gtk_box_pack_start(PGtkBox(vBox),Vte, false, false, 0);
  gtk_layout_put(PGtkLayout(fGtkLayout), vBox, 0, 0);
  buttonBox := gtk_hbox_new(false, WINDOW_PADDING);
  cancelButton := gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  closeButton := gtk_button_new_from_stock(GTK_STOCK_CLOSE);
  gtk_box_pack_end(PGtkBox(buttonBox), closeButton, false, false, WINDOW_PADDING);
  gtk_box_pack_end(PGtkBox(buttonBox), cancelButton, false, false, WINDOW_PADDING);
  gtk_signal_connect(GTK_OBJECT(buttonBox), 'size-allocate',
		                   GTK_SIGNAL_FUNC(@buttonbox_sizeallocate_callback), Self);
  // gtk_layout_put(PGtkLayout(vBox), buttonBox, 0,500);
 gtk_box_pack_start(GTK_BOX(vBox), buttonBox, false, false, WINDOW_PADDING);
  gtk_widget_set_sensitive(closeButton, false);
  gtk_signal_connect(GTK_OBJECT(cancelButton), 'clicked',
		                   GTK_SIGNAL_FUNC(@cancelButtonClick), Self);
  gtk_signal_connect(GTK_OBJECT(closeButton), 'clicked',
		                   GTK_SIGNAL_FUNC(@closeButtonClick), Self);

  Caption := 'TVTETerminal';
end;

destructor TVTETerminal.Destroy;
begin
  inherited Destroy;
  gtk_widget_destroy(cancelButton);
  gtk_widget_destroy(closeButton);
  gtk_widget_destroy(buttonBox);
  writeln('about to destroy vBox');
  gtk_widget_destroy(vBox);
  writeln('about to destroy VTEWindow');
  gtk_widget_destroy(VteWindow);
  writeln('OK: done');
end;

procedure TVTETerminal.Show;
begin
  gtk_widget_show_all(VteWindow);
end;

procedure TVTETerminal.ShowModal ( ParentWindow: TForm ) ;
begin
  gtk_window_set_modal( PGtkWindow( VteWindow ), true);
  gtk_window_set_transient_for( PGtkWindow( VteWindow ), PGtkWindow(ParentWindow.Handle));
  gtk_window_set_position( PGtkWindow( VteWindow ), GTK_WIN_POS_CENTER_ON_PARENT);
  gtk_widget_show_all(VteWindow);
end;

procedure TVTETerminal.Hide;
begin
  gtk_widget_hide(VteWindow);
end;

//Print output on the terminal which will not be interpreted by bash
procedure TVTETerminal.EnableButton(Button: PGtkWidget; Enable: boolean);
begin
  gtk_widget_set_sensitive(Button, Enable);
end;


end.

