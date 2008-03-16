{ DarGUI v.0.2.0

  Copyright (C) 2008 Malcolm Poole mgpoole@users.sourceforge.net

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
program dargui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, main, darintf, selectrestore, archive, filemaskdlg,
  ProcessLine, ArchiveInfo, FileOverwrite, About, oplog, isolate, diff, prefs,
  historymenu, dgStrConst;

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

