unit diff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TDiffForm }

  TDiffForm = class ( TForm )
    CloseButton: TBitBtn;
    Label1: TLabel;
    ListBox1: TListBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    BrowseArchive: TButton;
    BrowseDirectory: TButton;
    OpenDialog: TOpenDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    VerboseCheck: TCheckBox;
    DialogButtonPanel: TPanel;
    ArchiveBox: TLabeledEdit;
    BaseDirBox: TLabeledEdit;
    ResultButtonPanel: TPanel;
    DialogPanel: TPanel;
    ResultPanel: TPanel;
    procedure BrowseArchiveClick ( Sender: TObject ) ;
    procedure BrowseDirectoryClick ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 



implementation

uses darintf;

{ TDiffForm }

procedure TDiffForm.BrowseArchiveClick ( Sender: TObject ) ;
begin
  if OpenDialog.Execute
     then ArchiveBox.Text := TrimToBase(OpenDialog.FileName);
end;

procedure TDiffForm.BrowseDirectoryClick ( Sender: TObject ) ;
begin
  SelectDirectoryDialog.InitialDir := BaseDirBox.Text;
  if SelectDirectoryDialog.Execute
     then BaseDirBox.Text := SelectDirectoryDialog.FileName;
end;

initialization
  {$I diff.lrs}

end.

