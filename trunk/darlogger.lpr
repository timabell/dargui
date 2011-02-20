program darlogger;

{$mode objfpc}{$H+}

uses
  SysUtils
  { you can add units after this };
  var input : string;
      st: text;
      log: textfile;
      logging: Boolean;

{$R *.res}

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




