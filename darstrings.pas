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

var
  dsDataColumn,
  dsStatusColumn,
  dsPermissionColumn : string;
  
  
resourcestring
  dsYES           = 'YES';
  dsNO            = 'NO';
  dsLongoptions   = 'Long options support       : %s' + #10;
  dsLibzComp      = '   Libz compression (gzip)    : %s' + #10;
  dsLibbz2Comp    = '   Libbz2 compression (bzip2) : %s' + #10;
  dsNewBlowfish   = '   New Blowfish implementation: %s' + #10;
  dsExtendedAttributes = '   Extended Attributes support: %s' + #10;
  dsLargeFiles    = '   Large files support (> 2GB): %s' + #10;
  dsNoDump        = '   ext2fs NODUMP flag support : %s' + #10;
  dsSpecialAlloc  = '   Special allocation scheme  : %s' + #10;
  dsThreadSafe    = '   Thread safe support        : %s' + #10;

  dsRemoved       = '[     REMOVED       ]';

  dsArchiveEncrypted = 'The archive %S is encrypted and no encryption cipher has been given, cannot '
                       + 'open archive.';
  dsColumnTitles  = '[data ][ EA  ][compr] | permission | user  | group | size  |          '
                    + 'date                 |    filename' + #10;
  
  dsInodeCount    = 'total number of inode : %i' + #10;
  dsDestroyedFiles = '   %i file(s) have been record as destroyed since backup of reference' + #10#10;
  
function OpenDarTranslationInterface: Boolean;
procedure CloseDarTranslationInterface;

function TranslateDarString( darstring: string ): string;
function PosDarString( darstring, searchstring :string): integer;
function LengthDarString( darstring: string ): integer;  //returns chars preceding first %s or #10

function SetDarStrings: boolean;
procedure ExtractColumnTitles;
function UTF8EncodeChars( translatestring: string ): ansistring;



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
  ResetResourceTables;
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
  //if DarMOFile <> nil
  //   then darstring := TranslateDarString(darstring);
  darstring := Trim(darstring);
  p := Pos('%s', darstring);
  if p > 0
     then darstring := Copy(darstring, 1, p-1)
  else
      begin
          p := Pos('%i', darstring);
          if p > 0
             then darstring := Copy(darstring, 1, p-1);
      end;
  Result := Pos(darstring, searchstring);
end;

function LengthDarString(darstring: string): integer;
var
  s: LongInt;
  n: LongInt;
  i: LongInt;
begin
  Result := Length(darstring);
  i := Pos('%i', darstring);
  if i > 0
     then if i < Result
        then Result := i-1;
  s := Pos('%s', darstring);
  if s > 0
     then if s < Result
        then Result := s-1;
  n := Pos(#10, darstring);
  if n > 0
     then if n < Result
          then Result := n-1;
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
  s : Ansistring;
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
              s := UTF8EncodeChars(s);
              if s<>'' then
                ResStr^.CurrentValue:=s;
              inc(ResStr);
            end;
        end;
    end;
   CloseDarTranslationInterface;
end;

procedure ExtractColumnTitles;
var
  p: LongInt;
begin
  p := Pos(']', dsColumnTitles);
  // at present we only use the [data ] column
  dsDataColumn := Copy(dsColumnTitles, 1, p);
end;

function UTF8EncodeChars(translatestring: string): ansistring;
var
  x: Integer;
begin
  Result := '';
  for x := 1 to Length(translatestring) do
      if Ord(translatestring[x]) > 127
         then begin
           //for some reason we do not get the desired result if we UTFEncode Chr(Ord(translatestring[x]))
           //so this is the best we can do at the moment. These are French accented characters
           case Ord(translatestring[x]) of
                224: Result := Result + UTF8Encode(Chr(224));
                226: Result := Result + UTF8Encode(Chr(226));
                232: Result := Result + UTF8Encode(Chr(232));
                233: Result := Result + UTF8Encode(Chr(233));
                234: Result := Result + UTF8Encode(Chr(234));
           otherwise Result := Result + translatestring[x];
           end;
         end
      else Result := Result + translatestring[x];
end;



end.

