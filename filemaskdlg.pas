unit filemaskdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TFileMaskDialog }

  TFileMaskDialog = class ( TForm )
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    FileMask: TEdit;
    FileMaskLabel: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FileMaskDialog: TFileMaskDialog;

implementation

initialization
  {$I filemaskdlg.lrs}

end.

