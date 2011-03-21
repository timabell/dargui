unit gtkterm;

{$mode objfpc}

interface

uses
  Classes, SysUtils, VTETerminal, Controls, ExtCtrls, gtk2, glib2, Forms;

type

  { TGtkTerm }

  TGtkTerm = class(TCustomVTETerminal)
    fPanel: TPanel;
    fAlign: TAlign;
  private
    function GetParent: TWinControl;
    procedure SetParent( Value: TWinControl );
    procedure DoSizeAllocate ( terminalwidth, terminalheight: integer ) ; override;
    procedure DoResize(Sender: TObject);
    procedure SetAlign(Value: TAlign);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    function fGetLeft: integer;
    function fGetTop: integer;
    procedure fSetLeft(Value: integer);
    procedure fSetTop(Value: integer);
  public
    constructor Create ( Commandline: PChar ); override;
    destructor Destroy; override;
    procedure SetFocus;
  published
    property Parent: TWinControl read GetParent write SetParent;
    property Align: TAlign read fAlign write SetAlign;
    property Width: integer read fWidth write SetWidth;
    property Height: integer read fHeight write SetHeight;
    property Top: integer read fGetTop write fSetTop;
    property Left: integer read fGetLeft write FSetLeft;
  end;


implementation

{ TGtkTerm }

function TGtkTerm.GetParent: TWinControl;
begin
  Result := fPanel.Parent;
end;

procedure TGtkTerm.SetParent ( Value: TWinControl ) ;
var
  L: PGList;
begin
  fpanel.Parent := Value;
  L := gtk_container_get_children(PGtkContainer(fPanel.Handle));
  gtk_container_add(GTK_CONTAINER( L^.data ), PGtkWidget( Vte ));
  writeln('about to show VTE');
  gtk_widget_show(Vte);
  Application.ProcessMessages;
  writeln('VTE realised');
end;

procedure TGtkTerm.DoSizeAllocate ( terminalwidth, terminalheight: integer ) ;
begin
  inherited DoSizeAllocate ( terminalwidth, terminalheight ) ;
  if fHeight < 1 then    // code executed only on first callback
     begin
       fPanel.Width := terminalwidth;
       fWidth := terminalwidth;
       fPanel.Height := terminalheight;
       fHeight := terminalheight;
       fPanel.OnResize := @DoResize;
     end;
end;

procedure TGtkTerm.DoResize ( Sender: TObject ) ;
begin
  writeln('TGtkTerm.DoResize: ', fPanel.Width, 'x', fPanel.Height);
  ResizeVTEtofit(fPanel.Width, fPanel.Height);
  if Assigned(fOnResize)
     then fOnResize(Self);
end;

procedure TGtkTerm.SetAlign ( Value: TAlign ) ;
begin
  if Value = fAlign then exit;
  fPanel.Align := value;
  ResizeVTEtofit(fPanel.ClientWidth, fPanel.ClientHeight);
end;

procedure TGtkTerm.SetWidth ( Value: Integer ) ;
begin
  if Value=fWidth then exit;
  fPanel.Width := Value;
  fWidth := Value;
  DoResize(Self);
  Application.ProcessMessages; // let the screen display catch up
end;

procedure TGtkTerm.SetHeight ( Value: Integer ) ;
begin
  if Value=fHeight then exit;
  fPanel.Height := Value;
  fHeight := Value;
  DoResize(Self);
  Application.ProcessMessages; // let the screen display catch up
end;

function TGtkTerm.fGetLeft: integer;
begin
  Result := fPanel.Left;
end;

function TGtkTerm.fGetTop: integer;
begin
  Result := fPanel.Top;
end;

procedure TGtkTerm.fSetLeft ( Value: integer ) ;
begin
  fPanel.Left := Value;
end;

procedure TGtkTerm.fSetTop ( Value: integer ) ;
begin
  fPanel.Top := Value;
  writeln('set top');
end;

constructor TGtkTerm.Create ( Commandline: PChar ) ;
begin
  inherited Create ( Commandline ) ;
  fPanel := TPanel.Create(nil);
  fPanel.BevelOuter := bvNone;
  fPanel.Color := Background;
end;

destructor TGtkTerm.Destroy;
begin
  inherited Destroy;
  fPanel.Free;
end;

procedure TGtkTerm.SetFocus;
begin
  gtk_widget_grab_focus(Vte);
end;

end.

