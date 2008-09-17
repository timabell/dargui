unit darstrings;
{ This unit handles dar output in different languages}

{$mode objfpc}{$H+}{$INLINE ON}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, StringHashList
  {$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF}
  {$IFDEF MultiLocale},LConv{$ENDIF}, gettext;
  

var
  DarMOFile: TMOFile;
  
  
resourcestring
  dsYES           = 'YES';
  dsNO            = 'NO';
  dsLongoptions   = 'Long options support       : %s\n';
  dsLibzComp      = '   Libz compression (gzip)    : %s\n';
  dsLibbz2Comp    = '   Libbz2 compression (bzip2) : %s\n';
  dsNewBlowfish   = '   New Blowfish implementation: %s\n';
  
function OpenDarTranslationInterface: Boolean;
procedure CloseDarTranslationInterface;

function TranslateDarString( darstring: string ): string;
function PosDarString( darstring, searchstring :string): integer;
function LengthDarString( darstring: string ): integer;  //returns chars preceding first %s or \n

function SetDarStrings: boolean;



implementation

function OpenDarTranslationInterface: Boolean;
var
  mofilepath: string;
  MainLang: string;
  FallbackLang: string;
  p: LongInt;
  
  function Check_LC_MESSAGES_Path( aPath: string ): string;
  var
    testpath: string;
  begin
    Result := '';
    testpath := aPath + MainLang + '/LC_MESSAGES/dar.mo';
    if FileExists(testpath)
       then Result := testpath
       else begin
              testpath := aPath + FallbackLang + '/LC_MESSAGES/dar.mo';;
              if FileExists(testpath)
                 then Result := testpath;
            end;
  end;
  
  procedure SearchForDarMO;
  begin
    mofilepath := Check_LC_MESSAGES_Path('/usr/share/locale/');
    if Length(mofilepath)=0 then
        mofilepath := Check_LC_MESSAGES_Path('/usr/local/share/locale/');
    if Length(mofilepath)=0 then
        mofilepath := Check_LC_MESSAGES_Path('/usr/share/locale-langpack/');
writeln('dar.mo location: ', mofilepath);
  end;
  
begin
  Result := false;
  mofilepath := '';
  DarMOFile := nil;
  GetLanguageIDs(MainLang, FallbackLang);
  p := Pos('.', MainLang);
  if p > 0 then MainLang := Copy( MainLang, 1, p-1);
  if Pos('en', MainLang)<>1  then
     SearchForDarMO;
  if mofilepath <> '' then
  try
    //DarMOFile:= TMOFile.Create('/home/curator/tmp/Pascal/dargui/locales/fr.gmo');
    DarMOFile:= TMOFile.Create(mofilepath);
  except
    DarMOFile := nil;
    Result := false;
  end;
  Result := DarMOFile <> nil;
end;

procedure CloseDarTranslationInterface;
begin
  if DarMOFile <> nil then
     begin
      DarMOFile.Free;
      DarMOFile := nil;
     end;
end;

function TranslateDarString(darstring: string): string;
var
  p: LongInt;
begin
  if DarMOFile=nil then
     begin
       Result := darstring;
       exit;
     end;
  p := Pos('\n', darstring);
  While p>0 do
        begin
          darstring := Copy(darstring, 1, p-1)
                       + #10
                       + Copy(darstring, p+2, MaxInt);
          p := Pos('\n', darstring);
        end;
  Result := DarMOFile.Translate(darstring, Hash(darstring));
end;

function PosDarString(darstring, searchstring: string): integer;
var
  p: LongInt;
begin
  Result := 0;
  if DarMOFile <> nil
     then darstring := TranslateDarString(darstring);
  p := Pos('%s', darstring);
  if p > 0
     then darstring := Copy(darstring, 1, p-1);
  Result := Pos(darstring, searchstring);
end;

function LengthDarString(darstring: string): integer;
var
  s: LongInt;
  n: LongInt;
begin
  Result := Length(darstring);
  s := Pos('%s', darstring);
  if s > 0
     then if s < Result
        then Result := s;
  n := Pos('\n', darstring);
  if n > 0
     then if n < Result
          then Result := n;
end;

Type
  PResourceStringRecord = ^TResourceStringRecord;
  TResourceStringRecord = Packed Record
     Name,
     CurrentValue,
     DefaultValue : AnsiString;
     HashValue    : LongWord;
{$ifdef cpu64}
     Dummy        : LongWord; // alignment
{$endif cpu64}
   end;

   TResourceStringTableList = Packed Record
     Count : ptrint;
     Tables : Array[Word] of record
       TableStart,
       TableEnd   : PResourceStringRecord;
     end;
   end;


Var
  ResourceStringTable : TResourceStringTableList; External Name 'FPC_RESOURCESTRINGTABLES';


// Custom version of SetResourceStrings from objpas.pp
function SetDarStrings: boolean;
Var
  ResStr : PResourceStringRecord;
  i      : Longint;
  s,
  UpUnitName : AnsiString;
begin
  if DarMOFile=nil
     then exit;
  With ResourceStringTable do
    begin
      UpUnitName:='DARSTRINGS';
      For i:=0 to Count-1 do
        begin
          ResStr:=Tables[I].TableStart;
          { Check name of the Unit }
          if ResStr^.Name<>UpUnitName then
            continue;
          inc(ResStr);
          while ResStr<Tables[I].TableEnd do
            begin
              s:=DarMOFile.Translate(ResStr^.DefaultValue,ResStr^.HashValue);
              if s<>'' then
                ResStr^.CurrentValue:=s;
              inc(ResStr);
            end;
        end;
    end;
   CloseDarTranslationInterface;
end;



end.

