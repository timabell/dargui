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

uses darintf;

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

//TRecentFiles.AddFile should be called from the event assigned to OnClick
//to ensure that most recently used items are moved to the top
procedure TRecentFiles.AddFile(aCaption: string; AddToTop: Boolean = true);
var
  mi: TMenuItem;
  x: integer;
  Itemindex: LongInt;
  
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
  aCaption := TrimToBase(aCaption);
  if Caption = '-' then     // we have an empty menu: use the root entry
     begin
       Caption := aCaption;
     end;
  Itemindex := ExistsInMenu(aCaption);
  if Itemindex < 0 then  // new addition to menu
     begin
        mi := TMenuItem.Create(Owner);
        mi.OnClick := Self.OnClick;
        if AddToTop then
           begin
            mi.Caption := Caption;
            TMenuItem(Parent).Insert( 1, mi );
            fItems.Insert( 1, mi );
            Caption := aCaption;
           end
        else
           begin
             mi.Caption := aCaption;
             TMenuItem(Parent).Add(mi);
             fItems.Add(mi);
           end;
        DeleteOldFiles;
     end
  else
  if ItemIndex > 0 then
    begin  //move the most recently used to the top
          mi := TMenuItem.Create(Owner);
          mi.OnClick := Self.OnClick;
          mi.Caption := Caption;
          Caption := aCaption;
          fItems.Delete(ItemIndex);
          TMenuItem(Parent).Delete(Itemindex);
          TMenuItem(Parent).Insert(1, mi);
          fItems.Insert(1, mi);
         end;
  if AddToTop then
   // we add to bottom when reading from the IniFile so we don't want to write back again
      if Assigned(fIniFile) then
         begin
           fIniFile.WriteString(CfgRecentFiles, CfgRecentX + '0',Caption);
           for x := 1 to fItems.Count-1 do
                begin
                  if Assigned(fIniFile) then
                     fIniFile.WriteString(CfgRecentFiles, CfgRecentX + IntToStr(x),TMenuItem(fItems[x]).Caption);
                end;
         end;
end;

end.

