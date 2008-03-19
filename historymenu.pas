unit historymenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, prefs;


type

  { TRecentFiles }

  TRecentFiles = class(TMenuItem)
    private
      fItems: TList;
      fMax: integer;
      fIniFile: TSettingsFile;
      procedure SetMax( Value: integer);
      procedure DeleteOldFiles;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure AddFile(aCaption: string; AddToTop: Boolean = true);
    published
      property Max: integer read fMax write SetMax;
      property IniFile: TSettingsFile read fIniFile write fIniFile;
    end;

implementation

uses dgStrConst, darintf;

{ TRecentFiles }

procedure TRecentFiles.SetMax ( Value: integer ) ;
var
  x: Integer;
begin
  if Value > 0 then
     begin
       fMax := Value;
       DeleteOldFiles;
     end;
end;

procedure TRecentFiles.DeleteOldFiles;
var
  x: integer;
begin
       if fMax < fItems.Count
        then for x := fItems.Count-1 downto fMax do
            begin
             if fItems[x] <> nil then
                begin
                TMenuItem(fItems[x]).Free;
                fItems.Delete(x);
                end;
             if Assigned(fIniFile) then
                begin
                  if fIniFile.ReadString(CfgRecentFiles, CfgRecentX + IntToStr(x),'') <> ''
                     then fIniFile.DeleteKey(CfgRecentFiles, CfgRecentX + IntToStr(x));
                end;
            end;
end;

constructor TRecentFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems := TList.Create;
  fItems.Add(Self);
  Caption := '-';
  fMax := 5;
end;

destructor TRecentFiles.Destroy;
var
  x: integer;
begin
  fItems[0] := nil;
  fItems.Free;
  inherited Destroy;
end;

procedure TRecentFiles.AddFile(aCaption: string; AddToTop: Boolean = true);
var
  mi: TMenuItem;
  x: integer;
  
  function ExistsInMenu(fn: string): integer;
  var
    x: integer;
  begin
    Result := -1;
    for x := 0 to fItems.Count-1 do
        if TMenuItem(fItems[x]).Caption = fn then
           begin
             Result := x;
             exit;
           end;
  end;
begin
  if Caption = '-'
     then Caption := aCaption
     else if ExistsInMenu(aCaption) < 0 then
     begin
      mi := TMenuItem.Create(Owner);
      mi.OnClick := Self.OnClick;
      TMenuItem(Parent).Add(mi);
      fItems.Add(mi);
      DeleteOldFiles;
      if AddToTop then
         begin
          for x := fItems.Count-1 downto 1 do
              begin
                TMenuItem(fItems[x]).Caption := TMenuItem(fItems[x-1]).Caption;
                if Assigned(fIniFile) then
                   fIniFile.WriteString(CfgRecentFiles, CfgRecentX + IntToStr(x),TMenuItem(fItems[x-1]).Caption);
              end;
          Caption := aCaption;
         end
     else
         begin
           mi.Caption := aCaption;
           //This should happen when reading from the IniFile so we don't want to write back again
           //if Assigned(fIniFile) then
                //fIniFile.WriteString('Recent Files', 'Recent'+IntToStr(fItems.Count-1),aCaption);
         end;
     end;
  if Assigned(fIniFile) then
                fIniFile.WriteString(CfgRecentFiles, CfgRecentX + '0',Caption);
end;

end.

