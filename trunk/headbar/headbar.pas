unit HeadBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls;


type

  THeaderPanel = TCustomPanel;

  { THeaderContainer }

  THeaderContainer = class ( TCustomPanel )
  private
    fLeftmargin: integer;
    fRightmargin: integer;
    fChildHeader: THeaderPanel;
    fChildBar: THeaderContainer;
    fSplitter: TSplitter;
    function GetColID : integer;
    function GetCaption: string;
    procedure SetCaption( Value:string );
  public
    constructor Create ( TheOwner: TComponent ) ; override;
    function AddChild ( hCaption: string ) : THeaderContainer;
    property ColID : integer read GetColID;
    property Caption : string read GetCaption write SetCaption;
    property Header : THeaderPanel read fChildHeader;
  end;
  
  { TCustomHeaderBar }

  TCustomHeaderBar = class( THeaderContainer )
  private
    fColCount: integer;
    fHeaders: TList;
    fSubBars: TList;
    function  GetCols(index: Integer): TStrings;
    function GetColLeft(index: Integer): integer;
    function GetColRight(index: Integer): integer;
    function GetColWidth(index: Integer): integer;
    procedure SetCols(index: Integer; const AValue: TStrings);
    procedure SetColCount( Value: integer );
  public
    constructor Create ( TheOwner: TComponent ) ; override;
    destructor Destroy; override;
    procedure Add( Column: string );
    property ColCount : integer read fColCount write SetColCount default 1;
    property Height default 16;
    property Cols[index: Integer]: TStrings read GetCols write SetCols;
    property ColLeft[index: Integer]: integer read GetColLeft;
    property ColRight[index: Integer]: integer read GetColRight;
    property ColWidth[index: Integer]: integer read GetColWidth;
  end;
  
  
  THeaderBar = class( TCustomHeaderBar )
  published
    property Align;
    property Caption;
  end;

implementation

{ TCustomHeaderBar }

function TCustomHeaderBar.GetCols ( index: Integer ) : TStrings;
begin

end;

function TCustomHeaderBar.GetColLeft ( index: Integer ) : integer;
var
  x: Integer;
  i: Integer;
begin
  Result := -1;
  i := 0;
  if index > fHeaders.Count-1 then
     begin
       writeln('Error: column out of range (',index,')');
       exit;
     end;
  for x := 0 to index do
      i := i + THeaderContainer(fHeaders[x]).Left;
  Result := i;
end;

function TCustomHeaderBar.GetColRight ( index: Integer ) : integer;
var
  x: Integer;
  i: Integer;
begin
  Result := -1;
  i := 0;
  if index > fHeaders.Count-1 then
     begin
       writeln('Error: column out of range (',index,')');
       exit;
     end;
  for x := 0 to index do
      i := i + THeaderContainer(fHeaders[x]).Left;
  Result := i + THeaderContainer(fHeaders[x]).Header.Width;
end;

function TCustomHeaderBar.GetColWidth ( index: Integer ) : integer;
begin
  Result := -1;
  if index > fHeaders.Count-1 then
     begin
       writeln('Error: column out of range (',index,')');
       exit;
     end;
  Result := THeaderContainer(fHeaders[index]).Header.Width;
end;

procedure TCustomHeaderBar.SetCols ( index: Integer; const AValue: TStrings ) ;
begin

end;

procedure TCustomHeaderBar.SetColCount ( Value: integer ) ;
begin
  if Value < 1 then exit;
end;

constructor TCustomHeaderBar.Create ( TheOwner: TComponent ) ;
begin
  inherited Create ( TheOwner ) ;
  Align := alTop;
  fHeaders := TList.Create;
  fSubBars := TList.Create;
  fSubBars.Add(Self);
  fHeaders.Add(Self);
  fColCount := 1;
end;

destructor TCustomHeaderBar.Destroy;
begin
  fHeaders.Free;
  fSubBars.Free;
  inherited Destroy;
end;

procedure TCustomHeaderBar.Add ( Column: string ) ;
var
  NewHeader: THeaderContainer;
begin
  writeln(THeaderContainer(fHeaders[fHeaders.Count-1]).Caption);
  NewHeader := THeaderContainer(fHeaders[fHeaders.Count-1]).AddChild(Column);
  if NewHeader <> nil then fHeaders.Add(NewHeader)
  else writeln('NewHeader was nil');
end;

{ THeaderContainer }

function THeaderContainer.GetColID: integer;
begin
  If Owner is THeaderContainer
     then Result := THeaderContainer(Owner).ColID + 1
     else Result := 1;
end;

function THeaderContainer.GetCaption: string;
begin
  Result := fChildHeader.Caption;
end;

procedure THeaderContainer.SetCaption ( Value: string ) ;
begin
  fChildHeader.Caption := Value;
end;

constructor THeaderContainer.Create ( TheOwner: TComponent ) ;
var
  Child: THeaderPanel;
begin
  inherited Create ( TheOwner ) ;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  Child := THeaderPanel.Create(Self);
  Child.Parent := Self;
  Child.Align := alClient;
  Child.Caption := 'Column' + IntToStr(ColID);
  fChildHeader := Child;
end;

function THeaderContainer.AddChild ( hCaption: string ) : THeaderContainer;
begin
 Result := nil;
 if fChildBar <> nil then exit;
 fChildHeader.Align := alLeft;
 fChildHeader.Width := Width div 2;
 fSplitter := TSplitter.Create(Self);
 fSplitter.Parent := Self;
 fSplitter.Align := alLeft;
 fSplitter.Left := fChildHeader.Width;
 fChildBar := THeaderContainer.Create(Self);
 if hCaption <> '' then fChildBar.Caption := hCaption;
 fChildBar.Parent := Self;
 fChildBar.Align := alClient;
 Result := fChildBar;
end;

end.

