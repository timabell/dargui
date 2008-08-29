unit prefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles;
  
  
type
  TSettingsFile = TInifile;
  
var
  Preferences : TIniFile;
  
  function UseInfoFile: Boolean;

implementation

function UseInfoFile: Boolean;
begin
  Result := Preferences.ReadBool('Preferences', 'UseInfoFile', false);
end;

end.

