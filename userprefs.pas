unit userprefs;

{
 Set user preferences:
- default compression exceptions
- temporary directory ?
- number of recent files to remember
- show toolbar
- default directory for saving archives
- Size column format
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ListBox1: TListBox;
    MainNotebook: TNotebook;
    ButtonPanel: TPanel;
    DisplayPage: TPage;
    CreateArchivesPage: TPage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  OptionsForm: TOptionsForm;

implementation

initialization
  {$I userprefs.lrs}

end.

