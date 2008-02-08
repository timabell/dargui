unit oplog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TOpLogForm }

  TOpLogForm = class ( TForm )
    CloseButton: TBitBtn;
    OpSelector: TComboBox;
    ContentMemo: TMemo;
    OpSelectPanel: TPanel;
    ButtonPanel: TPanel;
    ContentPanel: TPanel;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure OpSelectorSelect ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
    procedure RefreshOpList;
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
  if LogList.Count > 0 then
     begin
       OpSelector.ItemIndex := LogList.Count-1;
       OpSelectorSelect(Sender);
     end;
end;

procedure TOpLogForm.FormDestroy ( Sender: TObject ) ;
begin
  LogList.Destroy;
end;

procedure TOpLogForm.OpSelectorSelect ( Sender: TObject ) ;
begin
  ContentMemo.Lines.LoadFromFile(LogList.Strings[OpSelector.ItemIndex]);
end;

procedure TOpLogForm.RefreshOpList;
var
 Rec : TSearchRec;
 fn: string;
 topLine: string;
 fileHandle: TextFile;
 LogNum: string;
 begin
  if FindFirst (LOGFILE_MASK, faAnyFile - faDirectory, Rec) = 0 then
  try
   repeat
      fn := Rec.Name;
      writeln(fn);
      LogNum := IntToStr(LogNumber(fn));
      if LogList.IndexOf(LogNum) < 0 then
         begin
          AssignFile(fileHandle, '/tmp/' + fn);
          Reset(fileHandle);
          ReadLn(fileHandle, topLine);
          if Pos('dar ',topLine) = 1 then
             begin
               OpSelector.Items.Add(topLine);
               LogList.Add('/tmp/' + fn);
             end;
         end;
   until FindNext(Rec) <> 0;
  finally
   FindClose(Rec) ;
  end;
end;

initialization
  {$I oplog.lrs}

end.

