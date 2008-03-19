unit filemaskdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TFileMaskDialog }

  TFileMaskDialog = class ( TForm )
    FileMask: TComboBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    FileMaskLabel: TLabel;
    procedure FileMaskExit(Sender: TObject);
    procedure FormShow ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
    procedure PopulateFilterList( FilterString: string );
  end; 

var
  FileMaskDialog: TFileMaskDialog;

implementation

{ TFileMaskDialog }

procedure TFileMaskDialog.FormShow ( Sender: TObject ) ;
begin
  FileMask.SetFocus;
end;

procedure TFileMaskDialog.PopulateFilterList(FilterString: string);
var
  a,
  b: integer;
begin
  FileMask.Items.Clear;
  if Length(FilterString) < 1 then exit;
  a := 1;
  b := a;
  while b < Length(FilterString) do
        begin
          if FilterString[b] = ';' then
             begin
               FileMask.Items.Add(Copy(FilterString, a, b-a));
               a := b+1;
             end;
          Inc(b);
        end;
  FileMask.Items.Add(Copy(FilterString, a, b-a+1));
end;


procedure TFileMaskDialog.FileMaskExit(Sender: TObject);
var
  i: Integer;
begin
  i := FileMask.Items.IndexOf(FileMask.Text);
  if i > -1
         then FileMask.Items.Delete(i);
  FileMask.Items.Insert(0,FileMask.Text);
  if FileMask.Items.Count > FileMask.DropDownCount
     then FileMask.Items.Delete(FileMask.Items.Count-1);
end;

initialization
  {$I filemaskdlg.lrs}

end.

