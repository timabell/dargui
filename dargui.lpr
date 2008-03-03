program dargui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, main, darintf, selectrestore, archive, filemaskdlg,
  ProcessLine, ArchiveInfo, FileOverwrite, About, oplog, isolate, diff;

begin
  Application.Title:='DarGUI';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm ( TArchiveForm, ArchiveForm ) ;
  Application.CreateForm ( TFileMaskDialog, FileMaskDialog ) ;
  Application.CreateForm(TInformationForm, InformationForm);
  Application.CreateForm ( TOpLogForm, OpLogForm ) ;
  Application.Run;
end.

