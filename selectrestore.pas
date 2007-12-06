unit selectrestore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CheckLst, Buttons, StdCtrls, EditBtn;

type

  { TExtractSelectedForm }

  TExtractSelectedForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    FileCheckListBox: TCheckListBox;
    FlatRestoreCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    OverwriteCheckBox: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    RestoreDirectoryEdit: TDirectoryEdit;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ExtractSelectedForm: TExtractSelectedForm;

implementation

{ TExtractSelectedForm }


initialization
  {$I selectrestore.lrs}

end.

