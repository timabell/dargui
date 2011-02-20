unit ArchiveInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TInformationForm }

  TInformationForm = class(TForm)
    OkButton: TBitBtn;
    ButtonPanel: TPanel;
    InformationMemo: TMemo;
    MemoPanel: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  InformationForm: TInformationForm;

implementation

initialization
  {$I archiveinfo.lrs}

end.

