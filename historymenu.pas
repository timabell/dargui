unit historymenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, LResources, Controls;


type

  { TRecentFiles }

  TRecentFiles = class(TMenuItem)
    private
      fItems: TList;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure AddFile(aCaption: string; AddToTop: Boolean = true);
    end;

implementation

{ TRecentFiles }

constructor TRecentFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems := TList.Create;
  Caption := '-'
end;

destructor TRecentFiles.Destroy;
var
  x: integer;
begin
  fItems.Free;
  inherited Destroy;
end;

procedure TRecentFiles.AddFile(aCaption: string; AddToTop: Boolean = true);
var
  mi: TMenuItem;
  x: integer;
begin
  if Caption = '-'
     then Caption := aCaption
     else
     begin
          mi := TMenuItem.Create(Owner);
          TMenuItem(Parent).Add(mi);
          fItems.Add(mi);
     if AddToTop then
         begin
          for x := fItems.Count -1 downto 1 do
              TMenuItem(fItems[x]).Caption := TMenuItem(fItems[x-1]).Caption;
          TMenuItem(fItems[0]).Caption := Caption;
          Caption := aCaption;
         end
     else
         begin
           mi.Caption := aCaption;
         end;
     end;
end;

end.

