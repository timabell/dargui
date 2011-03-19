unit gtkterm;

{$mode objfpc}

interface

uses
  Classes, SysUtils, VTETerminal, Controls, ExtCtrls, vte, gtk2, gdk2, glib2;

type

  { TGtkTerm }

  TGtkTerm = class(TCustomVTETerminal)
    fPanel: TPanel;
  private

  public
    constructor Create ( Commandline: PChar ); override;
    procedure SetFocus;
  end;


implementation

{ TGtkTerm }


constructor TGtkTerm.Create ( Commandline: PChar ) ;
begin
  inherited Create ( Commandline ) ;
end;

procedure TGtkTerm.SetFocus;
begin
  gtk_widget_grab_focus(Vte);
end;

end.

