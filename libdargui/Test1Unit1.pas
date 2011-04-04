unit Test1Unit1;

{$mode objfpc}{$H+}

{$Linklib dargui}
{$Packrecords C}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, EditBtn;

type

  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click ( Sender: TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure Button3Click ( Sender: TObject ) ;
    procedure FileNameEdit1Change ( Sender: TObject ) ;
 private
    { private declarations }
  public
    { public declarations }
    list_state: byte;
  end;

  TDarFeatures = record
   ea: Boolean;
   largefile: Boolean;
   nodump: Boolean;
   special_alloc: Boolean;
   bits: cardinal;
   thread_safe: Boolean;
   libz: Boolean;
   libbz2: Boolean;
   libcrypto: Boolean;
   new_blowfish: Boolean;
  end;

  PDarFeatures = ^TDarFeatures;

  TDarArchive = record
   handle: Pointer;
   name: Pchar;
   directory: Pchar;
   encrypted: Boolean;
   password: Pchar;
   format: integer;
   listcb: Pointer;
   questioncb: Pointer;
   passwordcb: Pointer;
   levellistcb: Pointer;
   verbosity: Boolean;
  end;

  PDarArchive = ^TDarArchive;

  TListingOutput = record
     flag: PChar;
     perm: PChar;
     uid: PChar;
     gid: PChar;
     size: PChar;
     date: PChar;
     filename: PChar;
     is_dir: Boolean;
     has_children: Boolean;
     archive: PDarArchive;
  end;
  PListingOutput = ^TListingOutput;


const
  LIST_NORMAL   = 0;
  LIST_TREE     = 1;
  LIST_XML      = 2;

  function get_dar_version(maj, med, min: PCardinal): cardinal; stdcall; external 'dargui';
  procedure get_dar_features( feat: PDarFeatures ); stdcall; external 'dargui';
  function dg_list_archive(arc: PDarArchive):smallint; stdcall; external 'dargui';
  function dg_open_archive(arc: PDarArchive): smallint; stdcall; external 'dargui';
  function dg_close_archive(arc: PDarArchive): smallint; stdcall; external 'dargui';
  function dg_get_children_of(arc: PDarArchive; directory: PChar): smallint; stdcall; external 'dargui';


  function libdar_get_dar_version(maj, med, min: PCardinal): cardinal; cdecl; external 'dar' name 'get_dar_version';

var
  Form1: TForm1; 

var
  v1, v2, v3 : cardinal;
  astr: string;
  n: integer;
  arch: TDarArchive;
  fn: string;
  f: string;
  d: string;
  list_stage: byte;

  procedure s_callback(mess: Pchar); cdecl;
  function q_callback(mess: Pchar): Boolean; cdecl;
  function pw_callback(mess: Pchar): Pchar; cdecl;
  procedure ll_callback(Output: PListingOutput); cdecl;


implementation

procedure s_callback(mess:Pchar); cdecl;
var
  s: string;
begin
  //ShowMessage(mess);
  s := mess;
  while s[length(s)] in [#10,#13] do
        Delete(s, Length(s), 1);
  if list_stage <2 then WriteLn(s);
  if list_stage>1 then
      begin
        Form1.Memo1.Lines.Add(s);
        Application.ProcessMessages;
      end;
end;

function q_callback(mess: Pchar): Boolean; cdecl;
begin
   if Pos('Continue listing', mess)>0 then
      begin
        result := true;
        list_stage := 2;
      end
   else
   Result := MessageDlg(mess, mtConfirmation, mbYesNo, 0)=mrYes;

end;

function pw_callback ( mess: Pchar ) : Pchar; cdecl;
var
  s: String;
begin
  s := PasswordBox(mess, 'Password required:');
  result := PChar(s);
end;

procedure ll_callback ( Output: PListingOutput ) ; cdecl;
begin
  Form1.Memo1.Lines.Add(Output^.archive^.name + '  ' +  Output^.filename);
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click ( Sender: TObject ) ;
begin
   if get_dar_version(@v1, @v2, @v3)=4
      then WriteLn('libdar version ',v1,'.',v2,'.', v3)
      else writeln('Error found when getting dar version');
end;

procedure TForm1.Button2Click ( Sender: TObject ) ;
var
  A : integer;
  s: String;
begin
   arch.encrypted := false;
   arch.format := LIST_TREE;
   arch.password := nil;
   if get_dar_version(@v1, @v2, @v3)=4
      then WriteLn('libdar version ',v1,'.',v2,'.', v3)
      else writeln('Error found when getting dar version');
   arch.verbosity := true;
   list_stage := 0;
   A := dg_open_archive(@arch);
   writeln('Open archive result: ', A);
   while A=5 do   // opening encrypted archive will fail with code 5
         begin    // also need to be able to abort op
           arch.encrypted := true;
           s := '';
           arch.password := Pchar(s);
           A := dg_open_archive(@arch);
         end;
   if A = 0
      then dg_get_children_of(@arch, PChar(''));
   dg_close_archive(@arch);
end;

procedure TForm1.Button3Click ( Sender: TObject ) ;
var
  A : integer;
  s: String;
begin
   arch.encrypted := false;
   arch.format := LIST_TREE;
   arch.password := nil;
   if get_dar_version(@v1, @v2, @v3)=4
      then WriteLn('libdar version ',v1,'.',v2,'.', v3)
      else writeln('Error found when getting dar version');
   arch.verbosity := true;
   list_stage := 0;
   A := dg_open_archive(@arch);
   writeln('Open archive result: ', A);
   while A=5 do   // opening encrypted archive will fail with code 5
         begin    // also need to be able to abort op
           arch.encrypted := true;
           s := '';
           arch.password := Pchar(s);
           A := dg_open_archive(@arch);
         end;
   if A = 0
      then dg_list_archive(@arch);
   dg_close_archive(@arch);
end;

procedure TForm1.FileNameEdit1Change ( Sender: TObject ) ;
begin
    if FileNameEdit1.FileName<>'' then
    begin
       WriteLn('Filename: ', FileNameEdit1.FileName);
      Memo1.Lines.Clear;
      f := ExtractFileName( FileNameEdit1.FileName );
      Delete(f, Length(f)-5, MaxInt);
      arch.name := Pchar( f );
      writeln(arch.name);
      d :=  ExtractFileDir( FileNameEdit1.FileName ) ;
      arch.directory := PChar(d);
      writeln(arch.directory);
      arch.listcb := @s_callback;
      arch.questioncb := @q_callback;
      arch.passwordcb := @pw_callback;
      arch.levellistcb := @ll_callback;
    end;
end;


end.

