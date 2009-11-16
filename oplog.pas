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
    CommandLabel: TLabel;
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
    procedure FormResize(Sender: TObject);
    procedure OpListSelectionChange ( Sender: TObject; User: boolean ) ;
    procedure OpSelectPanelResize(Sender: TObject);
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

uses dgStrConst, darintf;

{ TOpLogForm }

procedure TOpLogForm.FormCreate ( Sender: TObject ) ;
begin
  CommandLabel.Caption := rsCommands;
  CloseButton.Caption := rsButtonClose;
  LogList := TStringList.Create;
  RefreshOpList;
end;

procedure TOpLogForm.FormDestroy ( Sender: TObject ) ;
begin
  LogList.Free;
end;

procedure TOpLogForm.FormResize(Sender: TObject);
begin
  CloseButton.Left := ButtonPanel.Width - CloseButton.Width - 10;
  Splitter1.Refresh;
end;

procedure TOpLogForm.OpListSelectionChange ( Sender: TObject; User: boolean ) ;
begin
  if LogList.Count > Oplist.ItemIndex then
     begin
      ContentMemo.Lines.LoadFromFile(LogList.Strings[OpList.ItemIndex]);
      ContentMemo.Lines.Delete(0);
     end;
end;

procedure TOpLogForm.OpSelectPanelResize(Sender: TObject);
begin
  Oplist.Height := OpSelectPanel.Height - 40;
end;

procedure TOpLogForm.miCopyCommandClick ( Sender: TObject ) ;
begin
  Clipboard.AsText := OpList.Items[Oplist.ItemIndex];
end;

procedure TOpLogForm.miExecuteCommandClick ( Sender: TObject ) ;
begin
  RunDarCommand(OpList.Items[Oplist.ItemIndex], rsCptRepeatingAction, Left+100,
    Top+150);
end;

procedure TOpLogForm.RefreshOpList;
var
 Rec : TSearchRec;
 fn: string;
 topLine: string;
 fileHandle: TextFile;
 LogfileMask: String;
 begin
  LogfileMask := TEMP_DIRECTORY + DirectorySeparator + LOGFILE_BASE + '*';
  try
  if FindFirst (LogfileMask, faAnyFile - faDirectory, Rec) = 0 then
   repeat
      fn := TEMP_DIRECTORY + DirectorySeparator + Rec.Name;
      if LogList.IndexOf(fn) < 0 then
         begin
          AssignFile(fileHandle, fn);
          Reset(fileHandle);
          ReadLn(fileHandle, topLine);
          CloseFile(fileHandle);
          if Pos(DAR_EXECUTABLE,topLine) = 1 then
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
       OpList.ItemIndex := LogList.Count-1;
       OpListSelectionChange(nil, false);
     end;
  end;
end;

procedure TOpLogForm.AddCommand ( Cmd: string ) ;
begin
  OpList.Items.Add(Cmd);
  OpList.ItemIndex := OpList.Count - 1;
  //OpListSelectionChange(nil, false);
end;

initialization
  {$I oplog.lrs}

end.

