unit darintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Comctrls, StdCtrls, FileUtil, Forms, controls;
  
type
  TDarInfo = record
    version    : string;
    Longopts   : boolean;
    Libz       : Boolean;
    Libbz2     : Boolean;
    SSL        : Boolean;
    Blowfish   : Boolean;
    Extended   : Boolean;
    Largefiles : Boolean;
    Nodump     : Boolean;
    SpAlloc    : Boolean;
    IntSize    : byte;
    Threadsafe : Boolean;
    end;
    
  TdgSegment = record
    StartChar    : integer;
    EndChar      : integer;
    end;
    
    
const
   READ_BYTES = 2048;
   
   SEGSTATUS      = 0;
   SEGPERMISSIONS = 1;
   SEGUSER        = 2;
   SEGGROUP       = 3;
   SEGSIZE        = 4;
   SEGDATE        = 5;
   SEGFILENAME    = 6;
   SEGFILEPATH    = 7;
   
   DAR_EXECUTABLE = 'dar';
   
   TEMP_DIRECTORY = '/tmp/dargui/';
   LOGFILE_BASE = 'dargui.log.';
   BATCHFILE_BASE = 'dargui.batch.';
   TOOLDIR           = '/usr/share/dargui/';
   RUNSCRIPT         = 'rundar.sh';
   
   DAR_DOCPAGE = 'http://dar.linux.free.fr/doc/index.html';
   
// Config file
   CfgUserPrefs = 'User Preferences';
   CfgRecentFiles = 'Recent Files';
   CfgRecentX = 'Recent';
   CfgShowToolbar = 'ShowToolbar';


type
  TFileData = class
    item : array[SEGSTATUS .. SEGFILEPATH] of string;
    folder : Boolean;
    //Filename      : string;
    //Filepath      : string;
    //Archivestatus : string;
    //Permissions   : string;
    //Filesize      : string;
    //Filedate      : string;
    //Group         : string;
    //User          : string;
    end;


  function GetDarVersion : TDarInfo;
  function CheckSupportingApps : integer;
  function GetDarExit: integer;
  function LogNumber(fn: string): integer;
  function GetNextFileName( FileBase: string): string;
  procedure GetTerminalCommand(var Terminal: string);
  function GetRunscriptPath: string;
  function OpenArchive(var fn: string; TV: TTreeview; pw: string): integer;
  function RunDarCommand ( Cmd, Title: string; x, y :integer ) : integer;
  function PosFrom(const SubStr, Value: String; From: integer): integer;
  function SelectChildren(Node: TTreeNode): integer;
  function isInteger(aString: string): Boolean;
  function DeleteFilesByMask(FileMask: string): integer;
  function GetArchiveInformation (fn: TFilename; Memo: TMemo; pw:string): integer;
  function ArchiveIsEncrypted( fn: TFilename ): Boolean;
  function GetInodeCount( archivename, key: string ):integer;
  function ValidateArchive( var archivename: string; var pw: string ): Boolean;
  
  procedure WriteArchiveScript(fn: TFilename);
  function TrimToBase(fn: string): string;
  
  procedure GetDefaultBrowser(var Browser, Params: string);
  function SetBytes( bytes: Int64 ): string;

var
  TerminalCommand: string;
  RunscriptPath: String;

implementation

uses processline, password, darstrings;

// ************** GetDarVersion ***************** //

function GetDarVersion : TDarInfo;

   function GetVersion(src: string) : string;
   var
     x : integer;
   begin
     x := Pos('dar version', src) + 11;
     Delete(src,1,x);
     x := 1;
     while (src[x] in ['0'..'9']) or (src[x] = '.') do
           Inc(x);
     Delete(src,x,255);
     Result := src;
   end;

   function GetIntSize(src: string) : byte;
   var
     x : integer;
   begin
     Result := 0;
     x := Pos(':', src) + 1;
     Delete(src,1,x);
     x := 1;
     while (src[x] in ['0'..'9']) do
           Inc(x);
     Delete(src,x,255);
     Result := StrToInt(src);
   end;

   function GetBoolean(src: string; startpoint: integer) : Boolean;
   begin
     Result := PosDarString(dsYES, src) >= startpoint;
   end;
   
var
  Proc : TProcess;
  Output: TStringList;
  x : integer;
  info : TDarInfo;
  DarInEnglish: Boolean;
begin
  info.version := '';
  DarInEnglish := false;
  Result := info;
  Proc := TProcess.Create(Application);
  Output := TStringList.Create;
  Proc.CommandLine := 'dar -V';
  Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes];
  try
    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);
      for x := 0 to Output.Count -1 do
          if PosDarString(dsLongoptions, Output.Strings[x]) > 0
             then DarInEnglish := true;
      if DarInEnglish then writeln('DAR using English language');
      if not DarInEnglish
         then begin
               if OpenDarTranslationInterface
                    then SetDarStrings
                    else writeln('Unable to translate DAR output: please report this as a bug');
              end;
      for x := 0 to Output.Count -1 do
          begin
          if Pos('dar version',Output.Strings[x]) > 0
             then info.version := GetVersion(Output.Strings[x])
          else if PosDarString(dsLongoptions, Output.Strings[x]) > 0
                  then info.Longopts := GetBoolean(Output.Strings[x], LengthDarString(dsLongoptions))
          else if PosDarString(dsLibzComp, Output.Strings[x]) > 0
                  then info.Libz := GetBoolean(Output.Strings[x], LengthDarString(dsLibzComp))
          else if PosDarString(dsLibbz2Comp, Output.Strings[x]) > 0
                  then info.Libbz2 := GetBoolean(Output.Strings[x], LengthDarString(dsLibbz2Comp))
          else if PosDarString(dsNewBlowfish, Output.Strings[x]) > 0
                  then info.Blowfish := GetBoolean(Output.Strings[x], LengthDarString(dsNewBlowfish))
          else if Pos('extended attrib', Output.Strings[x]) > 0
                  then info.Extended := GetBoolean(Output.Strings[x], 1 )
          else if Pos('large files', Output.Strings[x]) > 0
                  then info.Largefiles := GetBoolean(Output.Strings[x], 1)
          else if Pos('nodump', Output.Strings[x]) > 0
                  then info.Nodump := GetBoolean(Output.Strings[x], 1)
          else if Pos('thread safe', Output.Strings[x]) > 0
                  then info.Threadsafe := GetBoolean(Output.Strings[x], 1)
          else if Pos('special alloc', Output.Strings[x]) > 0
                  then info.SpAlloc := GetBoolean(Output.Strings[x], 1)
          else if Pos('integer size', Output.Strings[x]) > 0
                  then info.IntSize := GetIntSize(Output.Strings[x]);
          end;
      except
      info.version := '-';
      end;  //try
    finally
      Result := info;
      Proc.Free;
      Output.Free;
    end;
end;


function CheckSupportingApps: integer;
var
  Returnvalue: integer;
begin
  Result := -1;
  Returnvalue := 0;
  if SearchFileInPath('xterm','',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]) = ''
     then Returnvalue := 1;
  if SearchFileInPath('bash','',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]) = ''
     then Returnvalue := Returnvalue + 2;
  Result := Returnvalue;
end;

function GetDarExit: integer;
var
  fout: Text;
  s: string;
begin
  Result := -1;
  if FileExists('/tmp/dar_exit') then
      begin
        Assign(fout, '/tmp/dar_exit');
        Reset(fout);
        ReadLn(fout,s);
        Result := StrToInt(s);
        Close(fout);
        Erase(fout);
      end;
end;

function LogNumber(fn: string): integer;
var
  x: Integer;
  NumAsString: String;
begin
  x := Length(fn);
  while ((fn[x] in ['0'..'9']) and (x > 1)) do
      Dec(x);
  NumAsString := Copy(fn, x+1, 8);
  try
    Result := StrToInt(NumAsString);
    except
    Result := -1;
  end;
end;

function GetNextFileName( FileBase: string): string;
var
 Rec : TSearchRec;
 fn: string;
 HighNum: integer;
 LogfileMask: String;
 ThisNum: LongInt;
begin
  HighNum := 1;
  LogfileMask := FileBase + '*';
  try
    if FindFirst (LogfileMask, faAnyFile - faDirectory, Rec) = 0 then
     repeat
        fn := Rec.Name;
        ThisNum := LogNumber(fn);
        if ThisNum >= HighNum
                then HighNum := ThisNum+1;
     until FindNext(Rec) <> 0;
    finally
    FindClose(Rec) ;
  end;
  Result :=  FileBase + IntToStr(HighNum);
end;

procedure GetTerminalCommand(var Terminal: string);

  function Find(const ShortFilename: string; var Filename: string): boolean;
  begin
    Filename:=SearchFileInPath(ShortFilename,'',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]);
    Result:=Filename<>'';
  end;

begin
  Terminal:='';
  // prefer xterm ;)
  if Find('xterm',Terminal) then exit;
  if Find('konsole',Terminal) then exit;
  if Find('gnome-terminal',Terminal) then exit;
  if Find('rxvt',Terminal) then exit;
end;

function GetRunscriptPath: string;
begin
  Result := TOOLDIR;
  //TODO: replace this by check for runscript.sh in appropriate directory
end;

// ************** OpenArchive ***************** //
function OpenArchive(var fn: string; TV : TTreeview; pw: string): integer;
var
  Proc : TProcessLineTalk;
  rootnode: TTreeNode;
  currentnode : TTreeNode;
  parentnode: TTreeNode;
  DataCoords : array[SEGSTATUS .. SEGFILENAME] of TdgSegment;
  CurrentFile : array[SEGSTATUS .. SEGFILEPATH] of string;
  outputline: String;
  nodecount: LongInt;
  n: Integer;
  nodesloaded: Integer;
  progressinterval: Integer;
  completed: LongInt;
  EncryptedArchive: Boolean;

   procedure SetSegments(colheading: string);
   var
     a, b : integer;
     S : integer;
   begin
     S := SEGSTATUS;
     a := 1;
     b := PosFrom('+', colheading, a);
     DataCoords[S].StartChar := a;
     DataCoords[S].EndChar := b;
     While b <> 0 do
           begin
           Inc(S);
           a := b+1;
           b := PosFrom('+', colheading, a);
           DataCoords[S].StartChar := a;
           DataCoords[S].EndChar := b;
           end;
     DataCoords[S].EndChar := 1000;
   end;
   
   procedure ParseCurrentFile(fileinfo: string);
   var
     x, y, s, e : integer;
   begin
     fileinfo := fileinfo + #13;
     if (Pos('[     REMOVED',fileinfo)=1)
        or (Pos('[     SUPPRIME', fileinfo)=1) then   // file flagged as removed
        begin
          CurrentFile[SEGSTATUS] := Copy(fileinfo,
                                  DataCoords[SEGSTATUS].StartChar,
                                  DataCoords[SEGSTATUS].EndChar - DataCoords[SEGSTATUS].StartChar);
          for x := 1 to SEGDATE do
              CurrentFile[x] := '';
          CurrentFile[SEGFILENAME] := Copy(fileinfo, Length(CurrentFile[SEGSTATUS])+1, MaxInt);
        end
     else
        begin                                   // file still exists
     for x := 0 to SEGPERMISSIONS do
         begin
           CurrentFile[x] := Copy(fileinfo,
                                  DataCoords[x].StartChar,
                                  DataCoords[x].EndChar - DataCoords[x].StartChar);
         end;
     s := DataCoords[SEGUSER].StartChar;
     for x := SEGUSER to SEGFILENAME do
         begin
           while fileinfo[s] < #33 do
                 Inc(s);
           e := s;
           while  fileinfo[e] > #30 do
                  Inc(e);
           CurrentFile[x] := Copy(fileinfo, s, e - s);
           s := e + 1;
         end;
        end;
     y := Length(CurrentFile[SEGFILENAME]);
     if y > 0 then
      while (CurrentFile[SEGFILENAME][y] <> DirectorySeparator) and (y > 1) do
         Dec(y);
      if CurrentFile[SEGFILENAME][y] = DirectorySeparator then
         begin
           CurrentFile[SEGFILEPATH] := Copy(CurrentFile[SEGFILENAME],1,y);
           Delete(CurrentFile[SEGFILENAME],1,y);
         end
         else CurrentFile[SEGFILEPATH] := '';
   end;

   function GetParentDirectoryNode(dir: string) : TTreeNode;
   var
     x,
     y: integer;
     teststr: string;
   begin
     if (parentNode = rootNode) or (dir = '') then
        begin
          Result := rootnode;
          exit;
        end;
     dir := DirectorySeparator + dir;
     y := length(dir) - 1; // allow for final '/'
     x := y;
     while (dir[x] <> DirectorySeparator) do
           Dec(x);
     teststr := Copy( dir, x+1, y-x);
     while (teststr <> parentNode.Text) and (parentNode <> rootNode) do
           parentNode := parentNode.Parent;
     Result := parentNode;
   end;

begin
  TV.Items.Clear;
  Result := -1;
  fn := TrimToBase(fn);
  rootnode := TTreeview(TV).Items.AddFirst(nil, ExtractFileName(fn));
  rootnode.Data := TFileData.Create;
  TFileData(rootnode.Data).item[SEGFILENAME] := rootnode.Text;
  parentnode := rootnode;
  nodesloaded := 0;
  nodecount := GetInodeCount(fn, pw);
  progressinterval := 0;
  outputline := '';
  Proc := TProcessLineTalk.Create(nil);
  Proc.CommandLine := 'dar -l ' + fn + pw + ' -Q';
  Proc.Execute;
  TTreeView(TV).Visible := false;
  Application.ProcessMessages;
  try
    While (nodesloaded < nodecount) do
          begin
          if Proc.ExitStatus <> 0 then raise Exception.Create('Dar exited with error code ' + IntToStr(Proc.ExitStatus));
          outputline := Proc.ReadLine;
          if outputline<>'' then
              begin
              if outputline[1]='[' then
                 if not (Pos('[data', outputline)=1) then
                    begin
                       Inc(nodesloaded);
                       Inc(progressinterval);
                       ParseCurrentFile(outputline);
                       currentnode :=  TTreeView(TV).Items.AddChild(GetParentDirectoryNode(Currentfile[SEGFILEPATH]), CurrentFile[SEGFILENAME]);
                       currentnode.Data := TFileData.Create;
                       if currentnode.Level < 2
                          then currentnode.Parent.Expand(false);
                       with TFileData(currentnode.data) do
                          begin
                            for n := SEGSTATUS to SEGFILEPATH do
                                item[n] := CurrentFile[n];
                            folder := false;
                          end;
                       if (Pos('[-----]', CurrentFile[SEGSTATUS]) > 0)
                          and (CurrentFile[SEGSIZE] = '0') then
                          begin // NB: empty files still get marked as folders in isolated catalogues
                            parentnode := currentnode;
                            TFileData(currentnode.Data).folder := true;
                          end;
                       {completed := (nodesloaded*90) div nodecount;
                       if progressinterval=(nodecount div 20) then    //can be used for calling a progress monitor callback
                            begin
                              write(completed, '% complete', #13);
                              progressinterval := 0;
                            end;   }
                       //Application.ProcessMessages;
                    end;
              if Pos('----', outputline) = 1 then SetSegments(outputline);
              end;
          end;
    TV.AlphaSort;
    TTreeView(TV).Visible := true;
    TTreeView(TV).Items[0].Selected := true;
  finally
    Result := Proc.ExitStatus;
    Proc.Free;
  end;  // try .. finally
end;


// ************** RunDarCommand ***************** //

function RunDarCommand ( Cmd, Title: string; x, y :integer ) : integer;
var
  Proc: TProcess;
  LogFile: string;
begin
  Result := -1;
  LogFile := GetNextFileName(TEMP_DIRECTORY + LOGFILE_BASE);
  Proc := TProcess.Create(Application);
  try
    Proc.CommandLine := TerminalCommand
                             +  ' -geometry 100x15+' + IntToStr(x) + '+' + IntToStr(y)
                             + ' -T "DarGUI: ' + Title
                             + '" '
                             + ' -e ' + RunscriptPath + RUNSCRIPT + #32  + Cmd
                             + ';' + Logfile;
    Cmd := Proc.CommandLine; //for debugging
    Proc.Options := Proc.Options + [poStderrToOutPut];
    Proc.Execute;
    While Proc.Running do
          Application.ProcessMessages;
  finally
    Proc.Free;
    Result := GetDarExit;
  end;
end;

// function PosFrom was taken from synautil.pas [ synapse.ararat.cz ]
function PosFrom(const SubStr, Value: String; From: integer): integer;
var
  ls,lv: integer;
begin
  Result := 0;
  ls := Length(SubStr);
  lv := Length(Value);
  if (ls = 0) or (lv = 0) then
    Exit;
  if From < 1 then
    From := 1;
  while (ls + from - 1) <= (lv) do
  begin
    {$IFNDEF CIL}
    if CompareMem(@SubStr[1],@Value[from],ls) then
    {$ELSE}
    if SubStr = copy(Value, from, ls) then
    {$ENDIF}
    begin
      result := from;
      break;
    end
    else
      inc(from);
  end;
end;

function SelectChildren(Node: TTreeNode): integer;
var
  count: integer;
  ChildNode: TTreeNode;
begin
  Result := 0;
  ChildNode := Node.GetFirstChild;
  ChildNode.MultiSelected := Node.MultiSelected;
  count := 1;
  Result := count;
end;

function isInteger(aString: string): Boolean;
var
  x: integer;
begin
  Result := true;
  for x := 1 to Length(aString) do
      if not (aString[x] in ['0'..'9'])
         then Result := false;
end;

function DeleteFilesByMask(FileMask: string): integer;
Var
   UFile : file;
   SearchRec: TSearchRec;
   aName:string;
   filePath: string;
Begin
 Result := 0;
 filePath:= ExtractFilePath(FileMask);
 if FindFirst(FileMask, faArchive, SearchRec)
        = 0 then
     begin
       repeat
          aName := filePath + DirectorySeparator + SearchRec.Name;
          Assign(UFile, aName);
          Erase (UFile);
          Result := Result + 1;
       until FindNext(SearchRec) <> 0;
       FindClose(SearchRec);
     end;
End;

function GetArchiveInformation(fn: TFilename; Memo: TMemo; pw: string): integer;
var
  Proc: TProcess;
  x: Integer;
begin
  Result := -1;
  Proc := TProcess.Create(nil);
  Proc.CommandLine := 'dar -l' + fn + pw +' -v -Q';
  Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes, poStderrToOutPut];
      try
      Proc.Execute;
      Memo.Lines.LoadFromStream(Proc.Output);
      for x := Memo.Lines.Count -1 downto 0 do
          begin
            if Memo.Lines[x] = ''
               then Memo.Lines[x] := StringOfChar('-',45)
            else if Pos('aborting', AnsiLowerCase(Memo.Lines[x])) = 1
               then Memo.Lines.Delete(x)
            else if Pos('extracting', AnsiLowerCase(Memo.Lines[x])) = 1
               then Memo.Lines.Delete(x)
            else if Pos('has been encrypted', Memo.Lines[x]) > 0
               then Memo.Lines.Delete(x);
          end;
      Result := 0;
      finally
      Proc.Free;
      end;
 end;

function TrimToBase(fn: string): string;
var
  x: LongInt;
begin
  Result := fn;
  fn := Trim(fn);
  x := Pos('.dar',fn);
  if x = Length(fn)-3 then
     begin
     x := x-1;
       while fn[x] <> '.' do
             Dec(x);
       Delete(fn,x,100);
       Result := fn;
     end;
end;

// GetDefaultBrowser taken from LazConf.pp [ Lazarus IDE ]
procedure GetDefaultBrowser(var Browser, Params: string);

  function Find(const ShortFilename: string; var Filename: string): boolean;
  begin
    Filename:=SearchFileInPath(ShortFilename,'',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]);
    Result:=Filename<>'';
  end;

begin
  Params:='%s';
  Browser:='';
  // prefer open source ;)
  if Find('mozilla',Browser) then exit;
  if Find('galeon',Browser) then exit;
  if Find('firefox',Browser) then exit;
  if Find('dillo',Browser) then exit;
  if Find('konqueror',Browser) then exit;
  if Find('safari',Browser) then exit;
  if Find('netscape',Browser) then exit;
  if Find('opera',Browser) then exit;
  if Find('iexplore.exe',Browser) then exit;
end;

// adapted from code found at http://www.freevbcode.com/ShowCode.asp?ID=1971
function SetBytes(bytes: Int64): string;
begin
  if bytes < 0 then
     begin
       Result := '';
       exit;
     end;
  try
    if bytes >= 1073741824
       then Result := FormatFloat('#0.00 GB', (bytes / 1024 / 1024 / 1024))
    else if bytes >= 1048576
         then Result := FormatFloat('#0.00 MB', (bytes / 1024 / 1024))
    else if bytes >= 1024
         then Result := FormatFloat('#0 KB', (bytes / 1024))
    else Result := IntToStr(bytes) + ' B';
  except
    Result := '';
  end;
end;

function ArchiveIsEncrypted ( fn: TFilename ) : Boolean;
var
  Proc : TProcess;
  Output: TStringList;
  p: Integer;
  teststr: String;
begin
  Result := false;
  teststr := 'The archive ' + ExtractFileName(fn) + ' is encrypted';
  Proc := TProcess.Create(Application);
  Output := TStringList.Create;
  Proc.CommandLine :=  DAR_EXECUTABLE + ' -l ' + fn + ' -v -Q';
  Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes, poStderrToOutPut];
  try
    Proc.Execute;
    Output.LoadFromStream(Proc.Output);
    Result := Pos(teststr, Output.Text) > 0;
  finally
    Proc.Free;
    Output.Free;
  end;
end;

function ValidateArchive( var archivename: string; var pw: string ): Boolean;
begin
  result := true;
  pw := '';
  archivename := TrimToBase( archivename );
  if ArchiveIsEncrypted(archivename) then
   if PasswordDlg.Execute( archivename ) = mrOK
      then pw := ' -K :' + PasswordDlg.Password
      else Result := false;
end;

//TODO: implement this procedure WriteArchiveScript(fn: TFilename);
procedure WriteArchiveScript(fn: TFilename);
begin

end;

function GetInodeCount( archivename, key: string ):integer;
var
 Proc : TProcess;
 Output: TStringList;
 p: Integer;
 outputline: string;
 coloncount: Integer;
 
 function FindInodeLine: string;
 var
   ln: integer;
 begin
   ln := 0;
   Result := Output.Strings[ln];
     while (ln < Output.Count) do
         begin
           Result := Output.Strings[ln];
           if PosDarString((dsInodeCount), Result) > 0
              then exit ;
           Inc(ln);
         end;
 end;
 
begin
 Result := -1;
 coloncount := 0;
 Proc := TProcess.Create(Application);
 Output := TStringList.Create;
 Proc.CommandLine :=  'dar -l ' + archivename + ' -v' + key + ' -Q';
 Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes];
 try
    Proc.Execute;
    Output.LoadFromStream(Proc.Output);
    outputline := FindInodeLine;
    p := Pos(':',outputline);
    if p > 0 then  //try to avoid provoking an exception
       try
         Result := StrToInt(Copy(outputline, p+2, MaxInt));
       except
         Result := -1;
       end;
 finally
   if Result=-1
      then writeln('Unable to extract node count - dumping output of dar -l -v:' + #10 + Output.Text);
   Proc.Free;
   Output.Free;
 end;
end;



End.

{EXIT CODES
dar exits with the following code:

0
 Operation successful.
1
 Syntax error on command-line.
2
 Error due to a hardware problem or a lack of memory.
3
 Detection of a condition that should never happen, and which is considered as a bug of the application.
4
 Code issued when the user has aborted the program upon dar question from dar. This also happens when dar is not run from a terminal (for example launched from crontab) and dar has a question to the user. In that case, dar aborts the same way as if the user pressed the escape key at the question prompt.
5
 is returned when an error concerning the treated data has been detected. While saving, this is the case when a file could not be opened or read. While restoring, it is the case when a file could not be created or replaced. While comparing, it is the case when a file in the archive does not match the one in the filesystem. While testing, it is the case when a file is corrupted in the archive.
6
 an error occurred while executing user command (given with -E or -F option). Mainly because the creation of a new process is not possible (process table is full) or the user command returned an error code (exit status different of zero).
7
 an error has occurred when calling a libdar routine. This means the caller (dar program), did not respect the specification of the API (and this can be considered as a particular case of a bug).
8
 the version of dar used is based in finite length integers (it has been compiled with the option --enable-mode=...). This code is returned when an integer overflow occurred. use the full version (based in infinint) to avoid this error.
9
 this code indicates an unknown error. I have probably forgotten to update the exception caching code to take care of new exceptions... this is a minor bug you are welcome to report.
10
 you have tried to use a feature that has been disabled at compilation time.
11
 some saved files have changed while dar was reading them, this may lead the data saved for this file not correspond to a valid state for this file. For example, if the beginning and the end of the file have been modified at the same time (while dar is reading it), only the change at the end will be saved (the beginning has already been read), the resulting state of the file as recorded by dar has never existed and may cause problem to the application using it.
 }
