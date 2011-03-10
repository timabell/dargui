unit darintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Comctrls, StdCtrls, FileUtil, Forms, controls, Dialogs,
  vteterminal;
  
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

  TArchiveInfo = record
    name       : string[128];
    password   : string[32];
    nodes      : longint;
    rootdir    : string[64];
  end;
    
const
   READ_BYTES = 2048;

   SEGPERMISSIONS = 0;
   SEGUSER        = 1;
   SEGGROUP       = 2;
   SEGSIZE        = 3;
   SEGDATE        = 4;
   SEGSTATUS      = 5;
   SEGFILENAME    = 6;
   SEGFILEPATH    = 7; //TODO: REMOVE THIS

   HEADERNAME     = 0;
   HEADERDATE     = 1;
   HEADERSIZE     = 2;
   HEADERUSER     = 3;
   HEADERGROUP    = 4;
   HEADERSTATUS   = 5;


   LOGFILE_BASE = 'dargui.log.';
   BATCHFILE_BASE = 'dargui.batch.';
   TOOLDIR           = '/usr/share/dargui/';
   RUNSCRIPT         = 'rundar.sh';
   
   DAR_DOCPAGE = 'http://dar.linux.free.fr/doc/index.html';
   DARGUI_HELP = '/usr/share/doc/dargui/index.html';
   DARGUI_INFO_FILE = 'dargui-info-file';

   
// Config file
   cfgUserPrefs = 'User Preferences';
   cfgRecentFiles = 'Recent Files';
   cfgRecentX = 'Recent';
   cfgShowToolbar = 'ShowToolbar';
   cfgDefaultConfig = 'DefaultArchiveConfig';
   cfgKeepWindowPos = 'KeepWindowPosition';
   cfgKeepWindowSize = 'KeepWindowSize';
   cfgRecentFileCnt = 'RecentFilesCount';

// bash special chars which we can't handle in archive names or paths
// we can already handle spaces so they're commented out
const escapechars = ['|', '&', ';', '(', ')', '<', '>', '"', ''''{, ' '}];

type

  TLoadProgress = (lpIncomplete, lpHistoryLoaded, lpLoaded, lpAborted);
  TProgressCallback = procedure(progress: integer) of object;

  TSettingsHeader = record
    version_major: byte;
    version_minor: byte;
    version_revision: byte;
    rootdir: string[64];
    reserve1: longint;
    reserve2: longint;
    reserve3: longint;
    backuptime: TDateTime;
  end;


type
  TStrArray = array[SEGPERMISSIONS .. SEGFILENAME] of string;

  TFileData = class
    item : TStrArray;
    folder : Boolean;
    end;

type
  TControlType = (ctNone, ctEdit, ctCombobox, ctRadiobutton, ctCheckbox, ctDateEdit, ctListBox);
  TArchiveOpenStatus =(aosOK, aosEncrypted, aosWrongPassword, aosFileNotPresent, aosAborted, aosError);

  function GetDarVersion : TDarInfo;
  function CheckSupportingApps : integer;
  function ProcessRunning( processname: string ): boolean;
  function GetDarExit: integer;
  function LogNumber(fn: string): integer;
  function GetNextFileName( FileBase: string): string;
  function GetRunscriptPath: string;
  function LoadArchiveToTree(fn: string; Tree: TTreeView; var info: TArchiveInfo; pw: string; progresscallback: TProgressCallback): integer;
  function RunDarCommand ( Cmd, Title: string; x, y :integer; log: Boolean ) : integer;
  function ShellCommand ( Cmd: string; var processoutput: string ) : integer;
  function PosFrom(const SubStr, Value: String; From: integer): integer;
  function SelectChildren(Node: TTreeNode): integer;
  function isInteger(aString: string): Boolean;
  function DeleteFilesByMask(FileMask: string): integer;
  function CheckArchiveStatus(fn: TFilename; pw:string): TArchiveOpenStatus;
  function GetArchiveInformation (fn: TFilename; Memo: TMemo; pw:string): integer;
  function ArchiveIsEncrypted( fn: TFilename; pass: PChar ) : Boolean;
  function GetInodeCount( var archinfo: TArchiveInfo ):integer;
  function ValidateArchive( archivename: string; var pw: string ): Boolean;
  function CreateUniqueFileName(sPath: string): string;

  function TrimToBase(fn: string): string;
  
  procedure GetDefaultBrowser(var Browser, Params: string);
  function SetBytes( bytes: Int64 ): string;
  function EscapeString(aString: string): string;
  function ContainsSpecialChars( aString: string ): Boolean;

var
  TerminalCommand: string;
  RunscriptPath: String;
  DAR_EXECUTABLE: string;
  TEMP_DIRECTORY: string;

  DarErrorMessage: string;


implementation

uses processline, password, darstrings, dgStrConst;

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
  Proc.CommandLine := DAR_EXECUTABLE + ' -V';
  Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes];
  try
    try
      Proc.Execute;
      Output.LoadFromStream(Proc.Output);
      if Output.Count > 0 then
        begin
          for x := 0 to Output.Count -1 do
              if Pos(Copy(dsLongoptions,1, 22), Output.Strings[x]) > 0
                 then DarInEnglish := true; // TODO: make this less of a hack
          if DarInEnglish then writeln('DAR using English language');
          if not DarInEnglish then
              begin
                  {if IsLanguageSupported
                        then TranslateDarStringsFromPO   }
                   if OpenDarTranslationInterface
                        then SetDarStrings
                        else writeln('Unable to translate DAR output (', SysUtils.GetEnvironmentVariable('LANG'), '): please report this as a bug');
                  end;
          ExtractColumnTitles;
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
              else if PosDarString(dsExtendedAttributes, Output.Strings[x]) > 0
                      then info.Extended := GetBoolean(Output.Strings[x], LengthDarString(dsExtendedAttributes))
              else if PosDarString(dsLargeFiles, Output.Strings[x]) > 0
                      then info.Largefiles := GetBoolean(Output.Strings[x], LengthDarString(dsLargeFiles))
              else if PosDarString(dsNoDump, Output.Strings[x]) > 0
                      then info.Nodump := GetBoolean(Output.Strings[x], LengthDarString(dsNoDump))
              else if PosDarString(dsThreadSafe, Output.Strings[x]) > 0
                      then info.Threadsafe := GetBoolean(Output.Strings[x], LengthDarString(dsThreadSafe))
              else if PosDarString(dsSpecialAlloc, Output.Strings[x]) > 0
                      then info.SpAlloc := GetBoolean(Output.Strings[x], LengthDarString(dsSpecialAlloc))
              else if Pos('integer size', Output.Strings[x]) > 0
                      then info.IntSize := GetIntSize(Output.Strings[x]);
              end;
        end
      else writeln('Error: no output from Dar in GetDarVersion');
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
  TerminalCommand := SearchFileInPath('xterm','',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]);
  if TerminalCommand = ''
     then Returnvalue := 1;
  if SearchFileInPath('bash','',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]) = ''
     then Returnvalue := Returnvalue + 2;
  Result := Returnvalue;
end;

// checks to see if processname is running. Requires checkprocess.sh
function ProcessRunning ( processname: string ) : boolean;
var
  Proc: TProcess;
  output: string;
  b: LongWord;
begin
  Result := false;
  Proc := TProcess.Create(Application);
  Proc.CommandLine := TOOLDIR + '/checkprocess.sh " ' + processname + '"';
  Proc.Options := Proc.Options + [poWaitOnExit, poUsePipes];
  try
    try
      Proc.Execute;
      b := Proc.Output.NumBytesAvailable;
      SetLength(output, b);
      Proc.Output.Read(output[1], b);
      output := output + #10;
      processname := #32 + processname + #10;
      Result := (Pos(processname, output) > 0);
    except
      writeln('Error when testing if ', Trim(processname), ' is running');
      Result := false;
    end;
  finally
    Proc.Free;
  end;
end;

function GetDarExit: integer;
var
  fout: Text;
  s: string;
  Exitfile: String;
begin
  Result := -1;
  ExitFile := TEMP_DIRECTORY + DirectorySeparator + 'dar_exit';
  if FileExists(Exitfile) then
      begin
        Assign(fout, Exitfile);
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

function GetRunscriptPath: string;
begin
  Result := TOOLDIR;
  //TODO: replace this by check for runscript.sh in appropriate directory
end;

// ************** LoadArchiveToTree **************
function LoadArchiveToTree(fn: string; Tree: TTreeView; var info: TArchiveInfo; pw: string; progresscallback: TProgressCallback): integer;
var
  Proc : TProcessLineTalk;
  rootnode: TTreeNode;
  currentnode : TTreeNode;
  parentnode: TTreeNode;
  CurrentFile : array[SEGPERMISSIONS .. SEGFILENAME] of string;
  outputline: String;
  nodecount: LongInt;
  n: Integer;
  nodesloaded: Integer;
  progressinterval: Integer;
  completed: LongInt;

      function ExplodeString(var A: TStrArray; Border, S: string): Integer;
       var
         S2: string;
         x: Integer;
       begin
         Result  := 0;
         if (PosDarString(dsRemoved, S)>0) then   // file flagged as removed
         begin
              A[SEGSTATUS] := Copy(dsRemoved, Pos('[',dsRemoved), Pos(']', dsRemoved)-Pos('[',dsRemoved)+1);
              for x := 0 to SEGDATE do
                  A[x] := '';
              x := Pos(dsRemoved, S) + Length(dsRemoved);
              while S[x] = #32 do inc(x);
              A[SEGFILENAME] := Trim(Copy(S, x-1, MaxInt));
              Result := 7;
         end
         else
           begin
             S2 := S + Border;
             repeat
               A[Result] := Copy(S2, 0 ,Pos(Border, S2) - 1);
               Delete(S2, 1,Length(A[Result] + Border));
               Inc(Result);
             until S2 = '';
           end;
       end;

       // trims depth markers from permissions  string : returns position in tree
       function GetLevel(var perms: string): integer;
       var
         p: Integer;
       begin
         Result := -1;
         if Length(perms)<1 then
            begin
              perms := ' '; //NB: do this to avoid an error when testing for folder
              exit;
            end;
         Result := 0;
         p := 1;
         while perms[p] in [#32,'|'] do
               begin
                 if perms[p]='|' then Inc(Result);
                 inc(p);
               end;
         Delete(perms, 1, p-1);
       end;

       function GetInfoFile: Boolean;
       var
         DarProc: TProcess;
         SavedSettings: TFileStream;
         header: TSettingsHeader;
         databuffer: string;
         datalength: smallint;
         datatype: TControlType;
         x: Integer;
       begin
        Result := false;
        DarProc := TProcess.Create(nil);
        DarProc.Options := [poWaitOnExit];
         Try
         if FileExistsUTF8(TEMP_DIRECTORY + DARGUI_INFO_FILE)
            then DeleteFileUTF8(TEMP_DIRECTORY + DARGUI_INFO_FILE);
         DarProc.CommandLine := DAR_EXECUTABLE + ' -x "' + fn + '" -g "'
               + DARGUI_INFO_FILE + '" -R "' + TEMP_DIRECTORY + '" -O -Q';
         writeln(DarProc.CommandLine);
         DarProc.Execute;
         if FileExistsUTF8(TEMP_DIRECTORY + DARGUI_INFO_FILE) then
            begin
              SavedSettings := TFileStream.Create(TEMP_DIRECTORY + DARGUI_INFO_FILE, fmOpenRead);
              if SavedSettings<> nil then
                 try
                   SavedSettings.Seek(7, soFromBeginning);
                   SavedSettings.Read(header, SizeOf(header));
                   info.rootdir := header.rootdir;
                   Result := true;
                 finally
                   SavedSettings.Free;
                 end;
            end;
         finally
         DarProc.Free;
         end;
       end;


begin
  Tree.Items.Clear;
  Result := -1;
  fn := TrimToBase(fn);
  rootnode := TTreeview(Tree).Items.AddFirst(nil, ExtractFileName(fn));
  rootnode.Data := TFileData.Create;
  TFileData(rootnode.Data).item[SEGFILENAME] := rootnode.Text;
  parentnode := rootnode;
  nodesloaded := 0;
  nodecount := GetInodeCount(info);
  GetInfoFile;
  progressinterval := 0;
  outputline := '';
  Proc := TProcessLineTalk.Create(nil);
  Proc.CommandLine := DAR_EXECUTABLE + ' -l "' + fn + '"' + pw + ' -Q --list-format=tree';
  Proc.Execute;
  TTreeView(Tree).Visible := false;
  Application.ProcessMessages;
  try
    outputline := Proc.ReadLine;
    while Pos('---+---', outputline)<1 do   // loop over header lines
                    outputline := Proc.ReadLine;
    While (nodesloaded < nodecount) do
      begin
        if Proc.ExitStatus <> 0 then raise Exception.Create('Dar exited with error code ' + IntToStr(Proc.ExitStatus));
        outputline := Proc.ReadLine;
        if ExplodeString(CurrentFile, #9, outputline)=7 then
           begin
             Inc(nodesloaded);
             Inc(progressinterval);
             GetLevel(CurrentFile[SEGPERMISSIONS]);

             if CurrentFile[SEGFILENAME]<>DARGUI_INFO_FILE then
             begin
             currentnode :=  TTreeView(Tree).Items.AddChild(parentnode, CurrentFile[SEGFILENAME]);
             currentnode.Data := TFileData.Create;
             if currentnode.Level < 2
                  then currentnode.Parent.Expand(false);
             with TFileData(currentnode.data) do
                  begin
                    for n := SEGPERMISSIONS to SEGFILENAME do
                        item[n] := CurrentFile[n];
                    folder := false;
                  end;
             if CurrentFile[SEGPERMISSIONS][1]='d' then
                  begin
                    if currentnode.Level > 0 then
                    TFileData(currentnode.Data).item[SEGFILENAME] := TFileData(parentnode.Data).item[SEGFILENAME] + DirectorySeparator + CurrentFile[SEGFILENAME];
                    parentnode := currentnode;
                    TFileData(currentnode.Data).folder := true;
                  end;
             end;
             completed := (nodesloaded*90) div nodecount;
             if progressinterval=(nodecount div 20) then    //can be used for calling a progress monitor callback
                    begin
                      if Assigned(progresscallback)
                           then progresscallback(completed);
                      progressinterval := 0;
                    end;
           end
        else if Pos('+---', outputline)>0 then  //go up a level
           begin
             parentnode := parentnode.Parent;
           end;
      end;
    Tree.AlphaSort;
    Tree.Visible := true;
    Tree.Items[0].Selected := true;
  finally
    Result := Proc.ExitStatus;
    Proc.Free;
  end;  // try .. finally
end;

// ************** RunDarCommand ***************** //
function RunDarCommand ( Cmd, Title: string; x, y :integer ; log: Boolean ) : integer;
var
  Proc: TProcess;
  commandfile: TFileStream;
  LogFile: string;
  buffer: string;
begin
  Result := -1;
  LogFile := GetNextFileName(TEMP_DIRECTORY + LOGFILE_BASE);
  Proc := TProcess.Create(Application);
  buffer := Cmd + #10 + 'DarStatus=$?' + #10 + 'exit $DarStatus';
  commandfile := TFileStream.Create(TEMP_DIRECTORY + DirectorySeparator + 'darcommand.sh', fmCreate);
  commandfile.Write(buffer[1], Length(buffer));
  commandfile.Free;
  try
    Proc.CommandLine := TerminalCommand
                             +  ' -geometry 100x15+' + IntToStr(x) + '+' + IntToStr(y)
                             + ' -T "DarGUI: ' + Title
                             + '" '
                             + ' -e ' + RunscriptPath + RUNSCRIPT + ' -c "' + Cmd;
    if log then Proc.CommandLine := Proc.CommandLine + '" -l "' + Logfile + '"';
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

function ShellCommand ( Cmd: string; var processoutput: string ) : integer;
var
  Shell: TProcess;
  bytes: LongWord;
begin
  Result := -1;
  processoutput := '';
  Shell := TProcess.Create(Application);
  if Shell <> nil then
     try
       Shell.CommandLine := Cmd;
       Shell.Options := Shell.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
       Shell.Execute;
       bytes := Shell.Output.NumBytesAvailable;
       SetLength(processoutput, bytes);
       Shell.Output.Read(processoutput[1], bytes);
       Result := Shell.ExitStatus;
       Shell.Free;
     except
       Shell.Free;
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

function Checkarchivestatus ( Fn: Tfilename; Pw: String ) : Tarchiveopenstatus;
var
  Proc: TProcess;
  x: Integer;
  OutputStrings: TStringList;
begin
  Result := aosError;
  DarErrorMessage := '';
  Proc := TProcess.Create(nil);
  Proc.CommandLine := DAR_EXECUTABLE + ' -l "' + fn + '" ' + pw + ' -v -Q';
  Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes, poStderrToOutPut];
      try
      OutputStrings := TStringList.Create;
      Proc.Execute;
      OutputStrings.LoadFromStream(Proc.Output);
      for x := 0 to OutputStrings.Count -1 do
          begin
          if PosDarString(dsCatalogueContents, OutputStrings[x]) > 0
               then Result := aosOK
            else if PosDarString(dsArchiveEncrypted, OutputStrings[x]) > 0
               then Result := aosEncrypted
            else if PosDarString(dsWrongPassword, OutputStrings[x]) > 0
               then Result := aosWrongPassword
            else if PosDarString(dsLastSliceNotFound, OutputStrings[x]) > 0
               then Result := aosFileNotPresent
            else if PosDarString(dsSliceNotFound, OutputStrings[x]) > 0
               then Result := aosFileNotPresent;
          if Result <> aosOK then DarErrorMessage := OutputStrings.Text;
          end;
      finally
      //TODO: remove following code when debugging complete
      {  writeln('OutputStrings: ', OutputStrings.Text);
        case Result of
             aosOK: writeln('Checkarchivestatus: OK');
             aosEncrypted: writeln('Checkarchivestatus: Encrypted');
             aosError: writeln('Checkarchivestatus: Error');
             aosFileNotPresent: writeln('Checkarchivestatus: Slice missing');
             end;    }
      OutputStrings.Free;
      Proc.Free;
      end;
End;

function GetArchiveInformation(fn: TFilename; Memo: TMemo; pw: string): integer;
var
  Proc: TProcess;
  x: Integer;
  tempMemo: TStringList;
begin
  Result := -1;
  DarErrorMessage := '';
  Proc := TProcess.Create(nil);
  Proc.CommandLine := DAR_EXECUTABLE + ' -l "' + fn + '" ' + pw + ' -v -Q';
  Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes, poStderrToOutPut];
      try
      tempMemo := TStringList.Create;
      Proc.Execute;
      tempMemo.LoadFromStream(Proc.Output);
      for x := tempMemo.Count -1 downto 0 do
          begin
          if tempMemo[x] = ''
               then tempMemo[x] := StringOfChar('-',45)
            else if Pos(dsAbortingNoUserResponse, tempMemo[x]) > 0
               then tempMemo.Delete(x)
            else if Pos(dsExtractingContents, tempMemo[x]) > 0
               then tempMemo.Delete(x)
            else if PosDarString(dsArchiveEncrypted, tempMemo[x]) > 0
               then tempMemo.Delete(x);
          end;
      if Memo<>nil then Memo.Lines.Assign(tempMemo);
      Result := 0;
      finally
      Proc.Free;
      tempMemo.Free;
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

function ArchiveIsEncrypted ( fn: TFilename; pass: PChar ) : Boolean;    //TODO: remove? (replaced by CheckArchiveStatus ? )
var
  Proc : TProcess;
  Output: TStringList;
  teststr: String;
begin
  Result := false;
  teststr := Format(dsArchiveEncrypted, [ExtractFileName(fn)]);
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

// checks to see if archive is encrypted. Requests password if encrypted
// TODO: cannot handle absent final slice or archives created by newer versions of dar
// TODO: this function has a lot of legacy baggage to tidy up
function ValidateArchive( archivename: string; var pw: string ): Boolean;
begin
  result := true;
  {if ContainsSpecialChars(archivename) then
     begin
       Result := false;
       ShowMessage(rsErrInvalidChars);
       exit;
     end; }
  pw := '';
  archivename := TrimToBase( archivename );
  try
   // if ArchiveIsEncrypted(archivename, nil) then
     if PasswordDlg.Execute( archivename ) = mrOK
        then pw := ' -K ":' + PasswordDlg.Password + '"'
        else Result := false;
  except
    ShowMessage('Error in ValidateArchive');
    Result := false;
  end;
end;

// from http://www.delphifaq.net/how-to-get-a-unique-file-name/
function CreateUniqueFileName(sPath: string): string;
var
  chTemp: Char;
begin
  repeat
    Randomize;
    repeat
      chTemp := Chr(Random(43) + 47);
      if Length(Result) = 8 then
        Result := Result + '.'
      else if chTemp in ['0'..'9', 'A'..'Z'] then
        Result := Result + chTemp;
    until Length(Result) = 12;
  until not FileExists(sPath + Result);
end;

function GetInodeCount( var archinfo: TArchiveInfo ):integer;
var
 Proc : TProcess;
 Output: TStringList;

 function ExtractInteger(searchstring: string): integer;
 var
   ln: integer;
 begin
   Result := -1;
   if Output.Count < 1 then exit;
   ln := Output.Count -1;
     while (ln > 0) do
         begin
           Result := ScanForInteger(Output[ln], searchstring);
           if Result > -1 then exit;
           Dec(ln);
         end;
 end;

begin
 Result := -1;
 Proc := TProcess.Create(Application);
 Output := TStringList.Create;
 Proc.CommandLine :=  DAR_EXECUTABLE + ' -l "' + archinfo.name + '" -v' + archinfo.password + ' -Q';
 Proc.Options := Proc.Options  + [poWaitOnExit, poUsePipes];
 try
    Proc.Execute;
    Output.LoadFromStream(Proc.Output);
    if Output.Count > 0 then  //try to avoid provoking an exception :
       try
         archinfo.nodes := ExtractInteger(dsInodeCount) + ExtractInteger(dsDestroyedFiles);
         Result := archinfo.nodes;
       except
         Result := -1;
       end;
 finally
   if Result<-1
      then writeln('Unable to extract node count - dumping output of dar -l ' + archinfo.name + ' -v:' + #10 + Output.Text);
   Proc.Free;
   Output.Free;
 end;
end;

function EscapeString(aString: string): string;
var
  x: Integer;
begin
  for x := length(aString) downto 1 do
      if aString[x] in escapechars
         then aString := Copy(aString, 1, x-1) + '\' + Copy(aString, x, MaxInt);
  Result := aString;
end;

function ContainsSpecialChars(aString: string): Boolean;
var
  x: Integer;
begin
  Result := false;
  for x := 1 to Length( aString ) do
      if aString[x] in escapechars then
         begin
           Result := true;
           exit;
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
