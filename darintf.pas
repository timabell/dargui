unit darintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Comctrls, StdCtrls;
  
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
   

type
  TFileData = class
    item : array[SEGSTATUS .. SEGFILEPATH] of string;
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
  function OpenArchive(fn: string; TV: TTreeview): integer;
  function CreateArchive(Cmd: string; msg: TMemo): integer;
  function PosFrom(const SubStr, Value: String; From: integer): integer;
  function SelectChildren(Node: TTreeNode): integer;
  function isInteger(aString: string): Boolean;

implementation

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

   function GetBoolean(src: string) : Boolean;
   begin
     Result := Pos(': YES', src) > 0;
   end;
   
var
  Proc : TProcess;
  Output: TStringList;
  x : integer;
  info : TDarInfo;
begin
  info.version := '';
  Result := info;
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  Proc.CommandLine := 'dar -V';
  Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes];
  try
    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);
      for x := 0 to Output.Count -1 do
          begin
          if Pos('dar version',Output.Strings[x]) > 0
             then info.version := GetVersion(Output.Strings[x])
          else if Pos('long options', Output.Strings[x]) > 0
                  then info.Longopts := GetBoolean(Output.Strings[x])
          else if Pos('libz comp', Output.Strings[x]) > 0
                  then info.Libz := GetBoolean(Output.Strings[x])
          else if Pos('libbz2', Output.Strings[x]) > 0
                  then info.Libbz2 := GetBoolean(Output.Strings[x])
          else if Pos('new blowfish', Output.Strings[x]) > 0
                  then info.Blowfish := GetBoolean(Output.Strings[x])
          else if Pos('extended attrib', Output.Strings[x]) > 0
                  then info.Extended := GetBoolean(Output.Strings[x] )
          else if Pos('large files', Output.Strings[x]) > 0
                  then info.Largefiles := GetBoolean(Output.Strings[x])
          else if Pos('nodump', Output.Strings[x]) > 0
                  then info.Nodump := GetBoolean(Output.Strings[x])
          else if Pos('thread safe', Output.Strings[x]) > 0
                  then info.Threadsafe := GetBoolean(Output.Strings[x])
          else if Pos('special alloc', Output.Strings[x]) > 0
                  then info.SpAlloc := GetBoolean(Output.Strings[x])
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


// ************** OpenArchive ***************** //

function OpenArchive(fn: string; TV : TTreeview): integer;
var
  Proc : TProcess;
  Output: TStringList;
  M: TMemoryStream;
  x,
  n: LongInt;
  BytesRead: LongInt;
  rootnode: TTreeNode;
  currentnode : TTreeNode;
  parentnode: TTreeNode;
  DataCoords : array[SEGSTATUS .. SEGFILENAME] of TdgSegment;
  CurrentFile : array[SEGSTATUS .. SEGFILEPATH] of string;
  FileData : TFileData;

   procedure SetSegments(colheading: string);
   var
     a, b : integer;
     S : integer;
   begin
     S := SEGSTATUS;
     a := 1;
     b := PosFrom('+', colheading, a);
   //  writeln(a);
     DataCoords[S].StartChar := a;
     DataCoords[S].EndChar := b;
     While b <> 0 do
           begin
           Inc(S);
           a := b+1;
      //     writeln(a);
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
     y := Length(CurrentFile[SEGFILENAME]);
      while (CurrentFile[SEGFILENAME][y] <> DirectorySeparator) and (y > 0) do
         Dec(y);
      if y > 0 then
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
  x := Length(fn) - 4;
  while fn[x] <> '.' do
        Dec(x);
  Delete(fn,x,100);
  Proc := TProcess.Create(nil);
  Output := TStringList.Create;
  M := TMemoryStream.Create;
  BytesRead := 0;
  writeln(fn);
  Proc.CommandLine := 'dar -l ' + fn;
  Proc.Options := [poUsePipes];
  Proc.Execute;
  While Proc.Running do
        begin
        M.SetSize(BytesRead + READ_BYTES);

        // try reading it
        n := Proc.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
        if n > 0
        then begin
             Inc(BytesRead, n);
             Write('.')
             end
        else begin
             // no data, wait 100 ms
             Sleep(100);
             end;
        end;
  // read last part
  repeat
  // make sure we have room
  M.SetSize(BytesRead + READ_BYTES);
  // try reading it
  n := Proc.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
  if n > 0
  then begin
       Inc(BytesRead, n);
       Write('.');
       end;
  until n <= 0;
  if BytesRead > 0 then WriteLn;
  M.SetSize(BytesRead);
  WriteLn('-- executed --');

  Output.LoadFromStream(M);
  WriteLn('-- linecount = ', Output.Count, ' --');
  
  rootnode := TTreeview(TV).Items.AddFirst(nil, ExtractFileName(fn));
  rootnode.Data := TFileData.Create;
  TFileData(rootnode.Data).item[SEGFILENAME] := rootnode.Text;
  parentnode := rootnode;
  for x := 1 to Output.Count-1 do
      begin
      if x = 1 then
         begin
         SetSegments(Output.Strings[x]);
         end
         else
         begin
         ParseCurrentFile(Output.Strings[x]);
         currentnode :=  TTreeView(TV).Items.AddChild(GetParentDirectoryNode(Currentfile[SEGFILEPATH]), CurrentFile[SEGFILENAME]);
         if Pos('[-----]', CurrentFile[SEGSTATUS]) > 0
            then parentnode := currentnode;
         currentnode.Data := TFileData.Create;
         with TFileData(currentnode.data) do
              begin
              for n := SEGSTATUS to SEGFILEPATH do
                  item[n] := CurrentFile[n];
              end;
         end;
      end;
  Result := Proc.ExitStatus;
  M.Free;
  Proc.Free;
  Output.Free;
  writeln('----- archive opened successfully -----');
end;


function CreateArchive ( Cmd: string; msg: TMemo ) : integer;
var
  Proc: TProcess;
  BytesRead: Integer;
  M: TMemoryStream;
  n: LongInt;
begin
  Proc := TProcess.Create(nil);
  M := TMemoryStream.Create;
  BytesRead := 0;
  Proc.CommandLine := cmd;
  Proc.Options := [poUsePipes];
  Proc.Execute;
  While Proc.Running do
        begin
        M.SetSize(BytesRead + READ_BYTES);

        // try reading it
        n := Proc.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
        if n > 0
        then begin
             Inc(BytesRead, n);
             Write('.')
             end
        else begin
             // no data, wait 100 ms
             Sleep(100);
             end;
        end;
  // read last part
  repeat
  // make sure we have room
  M.SetSize(BytesRead + READ_BYTES);
  // try reading it
  n := Proc.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
  if n > 0
  then begin
       Inc(BytesRead, n);
       Write('.');
       end;
  until n <= 0;
  if BytesRead > 0 then WriteLn;
  M.SetSize(BytesRead);
  WriteLn('-- executed --');

  msg.Lines.LoadFromStream(M);

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
  x: integer;
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


end.

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
