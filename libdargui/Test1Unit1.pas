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
   listcallback: Pointer;
   questioncallback: Pointer;
  end;

  PDarArchive = ^TDarArchive;

  function get_dar_version(maj, med, min: PCardinal): cardinal; cdecl; external 'dargui';
  procedure get_dar_features( feat: PDarFeatures ); cdecl; external 'dargui';
  procedure list_archive(arc: PDarArchive); cdecl; external 'dargui';
  function libraryfunction(dir: PChar; callback: Pointer):integer; cdecl; external 'dargui' name 'test_call';
  function libraryfunction2(I: integer):integer; cdecl; external 'dargui' name 'libraryfunction';

var
  Form1: TForm1; 

var
  v1, v2, v3 : cardinal;
  astr: string;
  n: integer;
  arch: TDarArchive;

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
      else writeln('Error found when getting dar version');;
end;

procedure TForm1.Button2Click ( Sender: TObject ) ;
begin
  get_dar_version(@v1, @v2, @v3);
  if FileNameEdit1.FileName<>'' then
    begin
      Memo1.Lines.Clear;
      arch.name := Pchar('DarGUI_releases_to_041');
      arch.directory := PChar('/media/data1/backups');
      arch.listcallback := @callback;
      arch.questioncallback := @qcallback;
      list_archive(@arch);
    end;
end;

procedure TForm1.Button3Click ( Sender: TObject ) ;
begin
  astr := 'Howdy!';
  libraryfunction(Pchar(astr), @callback);
   //n := spinEdit1.Value);
  writeln(  libraryfunction2(spinEdit1.Value) );
end;

end.

