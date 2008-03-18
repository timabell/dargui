unit selectfilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons;

type

  TTokenType = integer;
  TRegExp = string;

  { TSelectFilterForm }

  TSelectFilterForm = class ( TForm )
    AddFilterButton: TButton;
    CancelButton: TBitBtn;
    FileNamesOnlyCheck: TCheckBox;
    DelFilterButton: TButton;
    ClearFiltersButton: TButton;
    FilterList: TListBox;
    OKButton: TBitBtn;
    OKCancelPanel: TPanel;
    MainPanel: TPanel;
    RegExpList: TStringList;
    procedure AddFilterButtonClick ( Sender: TObject ) ;
    procedure ClearFiltersButtonClick ( Sender: TObject ) ;
    procedure DelFilterButtonClick ( Sender: TObject ) ;
    procedure FormClose ( Sender: TObject; var CloseAction: TCloseAction ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
    FileView: TTreeview;
    procedure SelectByFilter;
    function MatchesAnyFilter(aString: string): Boolean;
    function RegexpMatches(Filter, SearchText: string) : Boolean;
    function MakeRegExp( Filter: string ) : TRegExp;
  end; 


const
  ttAtStart = 0;
  ttWithin  = 1;
  ttAtEnd   = 2;


implementation

uses filemaskdlg, darintf, synregexpr;

{ TSelectFilterForm }

procedure TSelectFilterForm.AddFilterButtonClick ( Sender: TObject ) ;
var
  newfilter: string;
begin
  if FileMaskDialog.ShowModal = mrOk  then
     begin
       newfilter := Trim(FileMaskDialog.FileMask.Text);
       FilterList.Items.Add(newfilter);
       RegExpList.Add(MakeRegExp(newfilter)) ;
       DelFilterButton.Enabled := true;
       ClearFiltersButton.Enabled := true;
       FilterList.ItemIndex := FilterList.Count-1;
     end;
end;

procedure TSelectFilterForm.ClearFiltersButtonClick ( Sender: TObject ) ;
begin
  if MessageDlg('Remove all filters?', mtConfirmation, mbYesNo,0 ) = mrYes then
     begin
       RegExpList.Clear;
       FilterList.Clear;
       DelFilterButton.Enabled := false;
       ClearFiltersButton.Enabled := false;
     end;
end;

procedure TSelectFilterForm.DelFilterButtonClick ( Sender: TObject ) ;
begin
  if FilterList.ItemIndex > -1 then
     begin
      RegExpList.Delete(FilterList.ItemIndex);
      FilterList.Items.Delete(FilterList.ItemIndex);
      DelFilterButton.Enabled := FilterList.Items.Count > 0;
      ClearFiltersButton.Enabled := FilterList.Items.Count > 0;
     end;
end;

procedure TSelectFilterForm.FormClose ( Sender: TObject;
  var CloseAction: TCloseAction ) ;
begin
  RegExpList.Free;
end;

procedure TSelectFilterForm.FormCreate ( Sender: TObject ) ;
begin
  RegExpList := TStringList.Create;
end;

procedure TSelectFilterForm.OKButtonClick ( Sender: TObject ) ;
var
  x: Integer;
begin
  for x := 0 to FileView.Items.Count-1 do
      FileView.Items[x].MultiSelected :=
           MatchesAnyFilter(TFileData(FileView.Items[x].Data).item[SEGFILEPATH]
                             + TFileData(FileView.Items[x].Data).item[SEGFILENAME]);
  ModalResult := mrOk;
end;

procedure TSelectFilterForm.SelectByFilter;
var
  x: Integer;
  fd: TFileData;
  searchstring: string;
begin
  if FilterList.Count < 1 then exit;
  if FileView.Items.Count > 0
     then for x := 0 to FileView.Items.Count do
        if not FileView.Items[x].HasChildren then
          begin
            fd := TFileData(FileView.Items[x].Data);
            searchstring := fd.item[SEGFILENAME];
            if not FileNamesOnlyCheck.Checked
               then searchstring := fd.item[SEGFILEPATH] + searchstring;
            FileView.Items[x].MultiSelected := MatchesAnyFilter( searchstring );
            FileView.Items[x].Selected := FileView.Items[x].MultiSelected;
          end;
end;

function TSelectFilterForm.MatchesAnyFilter ( aString: string ) : Boolean;
var
  x: Integer;
begin
  Result := false;
  for x := 0 to RegExpList.Count-1 do
      if RegexpMatches(RegExpList.Strings[x], aString) then
         begin
           Result := true;
           exit;
         end;
end;

function TSelectFilterForm.RegexpMatches ( Filter, SearchText: string
  ) : Boolean;
var
  RegExpr: TRegExpr;
begin
  RegExpr := TRegExpr.Create;
  Regexpr.Expression := Filter;
  Result := RegExpr.Exec(SearchText);
  RegExpr.Free;
end;

function TSelectFilterForm.MakeRegExp ( Filter: string ) : TRegExp;
var
  reg_exp: string;
  re_x: integer;
  x: Integer;
  procedure re_AddChar( ch: Char );
  begin
    reg_exp := reg_exp + ch;
    Inc(re_x);
  end;
  
  procedure re_Escape( ch: Char );
  begin
    re_AddChar('\');
    re_AddChar( ch );
  end;
begin
  reg_exp := '';
  Result := reg_exp;
  re_x := 1;
  if Filter = '' then exit;
  if Filter[1] <> '*' then re_AddChar('^');
  for x := 1 to Length(Filter) do
      begin
        if Filter[x] = '*' then
           begin
             re_AddChar('.');
             re_AddChar('*');
           end
        else if Filter[x] = '?'
           then re_AddChar('.')
        else if Filter[x] in ['.', '^', '$', '+', '{', '[', ']', '\', '|', '(', ')']
           then re_Escape(Filter[x])
        else re_AddChar(Filter[x]);
      end;
  if Filter[Length(Filter)] <> '*'
     then re_AddChar('$');
  Result := reg_exp;
end;

initialization
  {$I selectfilter.lrs}

end.

