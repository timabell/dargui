unit oplog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Clipbrd, Menus;

type

  { TOpLogForm }

  TOpLogForm = class ( TForm )
    CloseButton: TBitBtn;
    Label1: TLabel;
    miExecuteCommand: TMenuItem;
    miCopyCommand: TMenuItem;
    OpList: TListBox;
    ContentMemo: TMemo;
    OpSelectPanel: TPanel;
    ButtonPanel: TPanel;
    ContentPanel: TPanel;
    OpListPopupMenu: TPopupMenu;
    Splitter1: TSplitter;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure OpListSelectionChange ( Sender: TObject; User: boolean ) ;
    procedure miCopyCommandClick ( Sender: TObject ) ;
    procedure miExecuteCommandClick ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
    procedure RefreshOpList;
    procedure AddCommand(Cmd: string);
  end; 

var
  OpLogForm: TOpLogForm;
  LogList: TStringList;

implementation

uses darintf;

{ TOpLogForm }

procedure TOpLogForm.FormCreate ( Sender: TObject ) ;
begin
  LogList := TStringList.Create;
  RefreshOpList;
end;

procedure TOpLogForm.FormDestroy ( Sender: TObject ) ;
begin
  LogList.Destroy;
end;

procedure TOpLogForm.OpListSelectionChange ( Sender: TObject; User: boolean ) ;
begin
  if LogList.Count > 0 then
     begin
      ContentMemo.Lines.LoadFromFile(LogList.Strings[OpList.ItemIndex]);
      ContentMemo.Lines.Delete(0);
     end;
end;

procedure TOpLogForm.miCopyCommandClick ( Sender: TObject ) ;
begin
  Clipboard.AsText := OpList.Items[Oplist.ItemIndex];
end;

procedure TOpLogForm.miExecuteCommandClick ( Sender: TObject ) ;
begin
  RunDarCommand(OpList.Items[Oplist.ItemIndex], 'Repeating action...', Left+100, Top+150);
end;

procedure TOpLogForm.RefreshOpList;
var
 Rec : TSearchRec;
 fn: string;
 topLine: string;
 fileHandle: TextFile;
 LogfileMask: String;
 begin
  LogfileMask := TEMP_DIRECTORY + LOGFILE_BASE +'*';
  if FindFirst (LogfileMask, faAnyFile - faDirectory, Rec) = 0 then
  try
   repeat
      fn := TEMP_DIRECTORY + Rec.Name;
      if LogList.IndexOf(fn) < 0 then
         begin
          AssignFile(fileHandle, fn);
          Reset(fileHandle);
          ReadLn(fileHandle, topLine);
          if Pos('dar ',topLine) = 1 then
             begin
               OpList.Items.Add(topLine);
               LogList.Add(fn);
             end;
         end;
   until FindNext(Rec) <> 0;
  finally
   FindClose(Rec) ;
  if LogList.Count > 0 then
     begin
//       OpSelector.ItemIndex := LogList.Count-1;
//       OpSelectorSelect(nil);
       OpList.ItemIndex := LogList.Count-1;
       OpListSelectionChange(nil, false);
     end;
  end;
end;

procedure TOpLogForm.AddCommand ( Cmd: string ) ;
begin
  OpList.Items.Add(Cmd);
  OpList.ItemIndex := OpList.Count - 1;
  OpListSelectionChange(nil, false);
end;

initialization
  {$I oplog.lrs}

end.

