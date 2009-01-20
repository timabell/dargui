unit schedman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, ExtCtrls;

type

  TScriptInfo = class(TObject)
    Fullname: ansistring;
    ScriptType: byte;
    ScriptID: integer;
  end;

type
  { TScheduleManagerForm }

  TScheduleManagerForm = class ( TForm )
    CloseButton: TBitBtn;
    CancelButton: TButton;
    ResultButtonPanel: TPanel;
    ScheduleList: TStringGrid;
    CronTab: TStringList;
    AtQ: TStringList;
    procedure CancelButtonClick ( Sender: TObject ) ;
    procedure CloseButtonClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure FormResize ( Sender: TObject ) ;
    procedure InitialiseInterface;
  private
    procedure GetAtScripts;
    procedure GetCronScripts;
    procedure DeleteLine( aLine: integer );
    function DeleteCronScript (aScript: TScriptInfo): boolean;
    function DeleteAtScript (aScript: TScriptInfo): boolean;
    { private declarations }
  public
    { public declarations }
  end;

var
  ScheduleManagerForm: TScheduleManagerForm;
  
const
  SCRIPT_TYPE_AT   = 0;
  SCRIPT_TYPE_CRON = 1;
  
  COL_SCRIPT_NAME   = 0;
  COL_SCRIPT_TIME   = 1;

implementation

uses main, dgStrConst, darintf;

{ TScheduleManagerForm }

procedure TScheduleManagerForm.FormResize ( Sender: TObject ) ;
begin
  ScheduleList.Width := Width - 50;
  ScheduleList.Height := Height - 150;
  CancelButton.Top := ScheduleList.Top + ScheduleList.Height + 15;
  CloseButton.Left := (ResultButtonPanel.Width - CloseButton.Width) div 2;
end;

procedure TScheduleManagerForm.GetAtScripts;
var
  AtOutput: string;
  counter: Integer;
  jobnum: String;
  searchResult: TSearchRec;
  outputline: string;
  p: LongInt;
  q: LongInt;
  spacecount: Integer;
  ScriptInfo: TScriptInfo;
begin
  if ShellCommand('atq', AtOutput) = 0 then
     begin
       AtQ.Text := AtOutput;   //get list of at jobs scheduled
       for counter := 0 to AtQ.Count-1 do
           begin
             outputline :=  AtQ.Strings[counter];
             p := Pos(#9, outputline);
             jobnum := Copy(outputline, 1, p-1);
             if FindFirst(AtScriptDir + '*.sh.'+ jobnum, faAnyFile, searchResult) = 0 then  //do any job numbers correspond?
                begin
                  ScheduleList.RowCount := ScheduleList.RowCount + 1;
                  ScheduleList.Cells[COL_SCRIPT_NAME, ScheduleList.RowCount-1] := ExtractFileName(searchResult.Name);
                  p := p + 1;                        //start of date/time info
                  q := Length(outputline);
                  spacecount := 0;
                  while spacecount < 2 do            //find end of date/time info
                        begin
                          if outputline[q] = #32 then Inc(spacecount);
                          Dec(q);
                        end;
                  ScheduleList.Cells[COL_SCRIPT_TIME,ScheduleList.RowCount-1] := Copy(outputline, p, q-p+1);
                  ScriptInfo := TScriptInfo.Create;
                  ScriptInfo.Fullname := AtScriptDir + searchResult.Name;
                  ScriptInfo.ScriptType := SCRIPT_TYPE_AT;
                  ScriptInfo.ScriptID := StrToInt(jobnum);
                  ScheduleList.Objects[0, ScheduleList.RowCount-1] := ScriptInfo;
                end;
             FindClose(searchResult);
           end;
     end;
end;

procedure TScheduleManagerForm.GetCronScripts;
var
  Cronoutput: string;
  x: Integer;
  counter: Integer;
  p: Integer;
  ScriptInfo: TScriptInfo;
begin
  if ShellCommand('crontab -l', Cronoutput) = 0 then
     begin
       CronTab.Text := Cronoutput;
       for x := 0 to CronTab.Count-1 do
           if Pos(CronScriptDir, Crontab.Strings[x]) > 0 then
              begin
                ScheduleList.RowCount := ScheduleList.RowCount + 1;
                counter := 0;
                p := 1;
                while counter < 5 do
                      begin
                        if Crontab.Strings[x][p] = #32 then Inc(counter);
                        Inc(p);
                      end;
                ScheduleList.Cells[COL_SCRIPT_TIME,ScheduleList.RowCount-1] := Copy(Crontab.Strings[x], 1, p-1);
                ScheduleList.Cells[COL_SCRIPT_NAME,ScheduleList.RowCount-1] := ExtractFileName( Copy(Crontab.Strings[x], p , MaxInt) );
                ScriptInfo := TScriptInfo.Create;
                ScriptInfo.Fullname := Copy(Crontab.Strings[x], p , MaxInt);
                ScriptInfo.ScriptType := SCRIPT_TYPE_CRON;
                ScriptInfo.ScriptID := x;
                ScheduleList.Rows[ScheduleList.RowCount-1].Objects[0] := ScriptInfo;
              end;
     end;
end;

procedure TScheduleManagerForm.DeleteLine ( aLine: integer ) ;
begin
  TScriptInfo(ScheduleList.Objects[0, aLine]).Free;
  ScheduleList.Objects[0, aLine] := nil;
  ScheduleList.DeleteColRow(false, aLine);
end;

function TScheduleManagerForm.DeleteCronScript ( aScript: TScriptInfo
  ) : boolean;
var
  shelloutput: string;
begin
  Result := false;
  try
    Crontab.Delete(aScript.ScriptID);
    Crontab.SaveToFile('/tmp/crontab.dargui');
    if ShellCommand('crontab /tmp/crontab.dargui', shelloutput) = 0 then
        begin
          DeleteFile(aScript.Fullname);
          Result := true;
        end
    else ShowMessage(rsErrDeletingCron + #10 + shelloutput);
    DeleteFile('/tmp/crontab.dargui');
  except
    ShowMessage(rsErrDeletingCron);
    Result := false;
  end;
end;

function TScheduleManagerForm.DeleteAtScript ( aScript: TScriptInfo
  ) : boolean;
var
  shelloutput: string;
begin
  Result := false;
  try
    if ShellCommand('atrm ' + IntToStr(aScript.ScriptID), shelloutput) = 0 then
       begin
         DeleteFile(aScript.Fullname);
         Result := true;
       end
    else ShowMessage('Error when executing atrm: '#10 + shelloutput);
  except
    ShowMessage(rsErrDeletingSched);
  end;
end;

procedure TScheduleManagerForm.CloseButtonClick ( Sender: TObject ) ;
begin
  Close;
end;

procedure TScheduleManagerForm.CancelButtonClick ( Sender: TObject ) ;
begin
  //TODO: reload crontab and atq to reduce risk of deleting entries newly created by other apps
  case TScriptInfo(ScheduleList.Objects[0, ScheduleList.Selection.Top]).ScriptType of
       SCRIPT_TYPE_AT    : if DeleteAtScript( TScriptInfo(ScheduleList.Objects[0, ScheduleList.Selection.Top]) )
                              then   DeleteLine(ScheduleList.Selection.Top);
       SCRIPT_TYPE_CRON  : if DeleteCronScript( TScriptInfo(ScheduleList.Objects[0, ScheduleList.Selection.Top]) )
                              then   DeleteLine(ScheduleList.Selection.Top);
       end;
end;

procedure TScheduleManagerForm.FormCreate ( Sender: TObject ) ;
begin
  InitialiseInterface;
  CronTab := TStringList.Create;
  AtQ := TStringList.Create;
  GetAtScripts;
  GetCronScripts;
  CancelButton.Enabled :=  ScheduleList.RowCount > 1 ;
end;

procedure TScheduleManagerForm.FormDestroy ( Sender: TObject ) ;
var
  x: Integer;
begin
  CronTab.Free;
  AtQ.Free;
  for x := ScheduleList.RowCount-1 downto 1 do
      DeleteLine(x);
end;

procedure TScheduleManagerForm.InitialiseInterface;
begin
  Caption := rsCptScheduledBackups;
  CancelButton.Caption := rsButtonRemove;
  CloseButton.Caption := rsButtonClose;
  ScheduleList.Columns.Items[COL_SCRIPT_NAME].Title.Caption := rsScript;
  ScheduleList.Columns.Items[COL_SCRIPT_TIME].Title.Caption := rsTime;
end;

initialization
  {$I schedman.lrs}

end.

