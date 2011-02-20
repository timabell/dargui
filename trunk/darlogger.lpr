program darlogger;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { you can add units after this };
  var input : string;
      st: text;
      log: textfile;
      logging: Boolean;
      
const
    TEMP_DIRECTORY = '/tmp/dargui/';
   LOGFILE_BASE = 'dargui.log.';

      
 function LogNumber(fn: string): integer;
var
  x: Integer;
  NumAsString: String;
begin
  x := Length(fn);
  while ((fn[x] in ['0'..'9']) and (x > 1)) do
      Dec(x);
  NumAsString := Copy(fn, x+1, 8);
  try
   Result := StrToInt(NumAsString);
   except
   Result := -1;
   end
end;

function GetNextFileName( FileBase: string): string;
var
 Rec : TSearchRec;
 fn: string;
 HighNum: integer;
 LogfileMask: String;
 ThisNum: LongInt;
 begin
  HighNum := 1;
  LogfileMask := FileBase + '*';
  if FindFirst (LogfileMask, faAnyFile - faDirectory, Rec) = 0 then
  try
   repeat
      fn := Rec.Name;
      ThisNum := LogNumber(fn);
      if ThisNum >= HighNum
              then HighNum := ThisNum+1;
   until FindNext(Rec) <> 0;
  finally
   FindClose(Rec) ;
  end;
  Result :=  FileBase + IntToStr(HighNum);
end;



begin
 logging := false;
 assign(st,'');
 reset(st);
    try
      AssignFile(log, Paramstr(1));
      if FileExists(ParamStr(1))
         then Append(log)
         else Rewrite(log);
      logging := true;
    except
      writeln('Error when opening log file: logging is disabled');
    end;
try
 while not eof(st) do
 begin // <<<<<<<<--- iterate while not end of file
   readln(st,input); //<<< read only a line
   if logging then
      begin
      writeln(input);
      writeln(log,input);
      end
      else writeln(input);
 end;
 finally
 close(st); // <<<<<---
 CloseFile(log);
 end;

end.




