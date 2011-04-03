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
    SpinEdit1: TSpinEdit;
    procedure Button1Click ( Sender: TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure Button3Click ( Sender: TObject ) ;
    procedure FileNameEdit1Change ( Sender: TObject ) ;
 private
    { private declarations }
  public
    { public declarations }
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
   name: Pchar;
   directory: Pchar;
   encrypted: Boolean;
   password: Pchar;
   listcb: Pointer;
   questioncb: Pointer;
   passwordcb: Pointer;
  end;

  PDarArchive = ^TDarArchive;

  function get_dar_version(maj, med, min: PCardinal): cardinal; cdecl; external 'dargui';
  procedure get_dar_features( feat: PDarFeatures ); cdecl; external 'dargui';
  function list_archive(arc: PDarArchive):smallint; cdecl; external 'dargui';
  function open_archive(arc: PDarArchive): smallint; cdecl; external 'dargui';


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

  procedure callback(mess: Pchar); cdecl;
  function qcallback(mess: Pchar): Boolean; cdecl;


implementation

procedure callback(mess:Pchar); cdecl;
var
  s: string;
begin
  //ShowMessage(mess);
  s := mess;
  while s[length(s)] in [#10,#13] do
        Delete(s, Length(s), 1);
  Form1.Memo1.Lines.Add(s);
  Application.ProcessMessages;
  //writeln('callback: ', mess);
end;

function qcallback(mess: Pchar): Boolean; cdecl;
begin
  Result := MessageDlg(mess, mtConfirmation, mbYesNo, 0)=mrYes;
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
begin
   if get_dar_version(@v1, @v2, @v3)=4
      then WriteLn('libdar version ',v1,'.',v2,'.', v3)
      else writeln('Error found when getting dar version');
   //arch.encrypted := true;
   //arch.password := Pchar('test');
   if arch.name <> nil then
    begin
     // if open_archive(@arch) = 0 then
         list_archive(@arch);
    end;
end;

procedure TForm1.Button3Click ( Sender: TObject ) ;
begin
    { if libdar_get_dar_version(@v1, @v2, @v3)=4
      then WriteLn('libdar version ',v1,'.',v2,'.', v3)
      else writeln('Error found when getting libdar version');}

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
      arch.listcb := @callback;
      arch.questioncb := @qcallback;
      arch.passwordcb := nil;
    end;
end;


end.

