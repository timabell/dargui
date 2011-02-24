unit darstrings;
{ This unit handles dar output in different languages}

{$mode objfpc}{$H+}{$INLINE ON}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, StringHashList
  {$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF}
  {$IFDEF MultiLocale},LConv{$ENDIF}, gettext, translations, dialogs;
  

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

  dsColumnTitles  = '[data ][ EA  ][compr] | permission | user  | group | size  |          '
                    + 'date                 |    filename' + #10;
  dsRemoved       = '[     REMOVED       ]';

  dsCatalogueContents = #10'CATALOGUE CONTENTS :'#10#10;
  dsInodeCount    = 'total number of inode : %i' + #10;
  dsDestroyedFiles = '   %i file(s) have been record as destroyed since backup of reference' + #10#10;

  // used by darintf.CheckArchiveStatus and darintf.GetArchiveInformation
  dsArchiveEncrypted = 'The archive %S is encrypted and no encryption cipher has been given, cannot '
                       + 'open archive.';
  dsWrongPassword    = 'Warning, the archive %S has been encrypted. A wrong key is not possible to '
                       + 'detect, it would cause DAR to report the archive as corrupted'#10;
  dsArchiveVersionTooHigh = 'The format version of the archive is too high for that software version, try '
                       + 'reading anyway?';
  dsAbortingNoUserResponse = 'Aborting program. User refused to continue while asking: ';
  dsLastSliceNotFound = 'The last file of the set is not present in ';
  dsSliceNotFound =  '%s is required for further operation, please provide the file.';

  dsReturnCancel = ' [return = OK | Esc = cancel]';

  dsExtractingContents = 'Extracting contents of the archive...';


function OpenDarTranslationInterface: Boolean;
procedure CloseDarTranslationInterface;

function IsLanguageSupported: Boolean;  // Added 2011-02-19 : previous method of
procedure TranslateDarStringsFromPO;    // using dar-provided dar.mo files now unable to handle accented characters

function TranslateDarString( darstring: string ): string;
function PosDarString( darstring, searchstring :string): integer;
function LengthDarString( darstring: string ): integer;  //returns chars preceding first %s or #10

function SetDarStrings: boolean;
procedure ExtractColumnTitles;

function ScanForInteger( searchstring, darstring :string): integer;



implementation

uses darintf;

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
    WriteLn('Error when trying to create ',mofilepath);
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

function IsLanguageSupported: Boolean;
var
  MainLang: string;
  FallbackLang: string;
  p: LongInt;
begin
  Result := false;
  GetLanguageIDs(MainLang, FallbackLang);
  writeln(MainLang, ' : ', FallbackLang);
  if Pos('fr', MainLang)=1 then Result := true;
  if Pos('de', MainLang)=1  then
     begin
       Result := false;
       ShowMessage('Warning: dar output is in German, which DarGUI cannot interpret correctly'+#10
                        + 'Check Help for more information');
     end;
  if Pos('sv', MainLang)=1  then Result := true;
end;

procedure TranslateDarStringsFromPO;
var
  Lang: string;
  FallbackLang: string;
  PODirectory: string;
begin
  PODirectory := TOOLDIR + 'locales/';
  if DirectoryExists(PODirectory) then
     begin
      GetLanguageIDs(Lang, FallbackLang); // in unit gettext
      TranslateUnitResourceStrings('darstrings', PODirectory + 'dargui.%s.po', Lang, FallbackLang);
      writeln('Reading output from dar using language ', Lang);
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

//TODO: review this code - will it work with %S or when %s is at start of string?
function PosDarString(darstring, searchstring: string): integer;
var
  p: integer;
  q: integer;
  x: integer;
  stringparts: TStringList;

  procedure SplitDarString;
  var
    s: integer;
    i: integer;
  begin
    q := -1;
    p := 1;
    while p > 0 do
          begin
            s := Pos('%s', AnsiLowerCase(darstring));
            i := Pos('%i', AnsiLowerCase(darstring));
            p := s;
            if i>p then if p=0 then p := i;
            if p > 0 then
               begin
                 stringparts.Add(Copy(darstring,1,p-1));
                 Delete(darstring, 1, p+1);
               end
               else stringparts.Add(darstring);
          end;
  end;

begin
  Result := 0;
  stringparts := TStringList.Create;
  //if DarMOFile <> nil
  //   then darstring := TranslateDarString(darstring);
  try
    darstring := Trim(darstring);
    SplitDarString;
    x:= 0;
    p := Pos(stringparts[x], searchstring);
    if p > 0 then
       begin
         Result := p;
         while x < stringparts.Count-1 do
               begin
                 q := p + Length(stringparts[x]);
                 Inc(x);
                 p := PosFrom(stringparts[x], searchstring, q);
                 if p=0 then Result := 0;
               end;
       end;
  finally
    stringparts.Free;
  end;
end;

function LengthDarString(darstring: string): integer;
var
  s: LongInt;
  n: LongInt;
  i: LongInt;
begin
  Result := Length(darstring);
  i := Pos('%i', AnsiLowerCase(darstring));
  if i > 0
     then if i < Result
        then Result := i-1;
  s := Pos('%s', AnsiLowerCase(darstring));
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
  ucs : UnicodeString  ;
  s: AnsiString;
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
              ucs:=DarMOFile.Translate(ResStr^.DefaultValue,ResStr^.HashValue);
              s := UTF8Encode(ucs);
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

function ScanForInteger(searchstring, darstring: string): integer;
var
  a1: Integer;
  b1: Integer;
  IntStr: String;
begin
  Result := -1;
  searchstring := Trim( searchstring );
  darstring := Trim( darstring );
  if Length(searchstring) = 0 then exit;
  a1 := 1;
  b1 := 1;
  while b1 <= Length(darstring) do
    begin
      if searchstring[a1] = darstring[b1] then
        begin
          Inc(a1);
          Inc(b1);
        end
      else if (darstring[b1] = '%') and (darstring[b1+1] = 'i') then
          begin
            IntStr := '';
            b1 := b1 + 2;
            while (a1 <= Length(searchstring))
                  and (searchstring[a1] in ['0'..'9']) do
                  begin
                    IntStr := IntStr + searchstring[a1];
                    Inc(a1);
                  end;
          end
      else exit;
  end;
  TryStrToInt(IntStr, Result);
end;



end.

