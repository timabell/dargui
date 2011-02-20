unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, headbar;

type

  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    procedure Button1Click ( Sender: TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
 HB: THeaderBar;

implementation

{ TForm1 }

procedure TForm1.FormCreate ( Sender: TObject ) ;
begin
HB := THEaderBar.Create(Self);
HB.Parent := Self;
end;

procedure TForm1.Button1Click ( Sender: TObject ) ;
begin
 HB.Add('new column');
end;

procedure TForm1.Button2Click ( Sender: TObject ) ;
begin
  writeln(HB.ColWidth[2]);
end;

initialization
  {$I unit1.lrs}

end.

