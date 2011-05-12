{
  testvte.lpr

  A project for testing and demonstrating use of VTETerminal.pas and libvte9

  Copyright (C) 2011 Malcolm Poole <mgpoole@users.sourceforge.net>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

program testvte;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, Unit1, LResources, gtkterm;

{$IFDEF WINDOWS}{$R testvte.rc}{$ENDIF}

begin
  {$I testvte.lrs}
  Application.Initialize;
  Application.CreateForm ( TVTEDemoForm, VTEDemoForm ) ;
  Application.Run;
end.

