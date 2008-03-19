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
    MatchAllCheck: TCheckBox;
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
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    function MatchesAllFilters ( aString: string ) : Boolean;
  private
    { private declarations }
    function MatchesAnyFilter(aString: string): Boolean;
    function RegexpMatches(Filter, SearchText: string) : Boolean;
    function MakeRegExp( Filter: string ) : TRegExp;
  public
    { public declarations }
    FileView: TTreeview;
    procedure ApplyFilters;
  end;


const
  ttAtStart = 0;
  ttWithin  = 1;
  ttAtEnd   = 2;
  
var
  SelectFilterForm: TSelectFilterForm;



implementation

uses filemaskdlg, darintf, synregexpr, prefs;

{ TSelectFilterForm }

procedure TSelectFilterForm.AddFilterButtonClick ( Sender: TObject ) ;
var
  newfilter: string;
  recentfilters: string;
  x: Integer;
begin
  recentfilters:= Preferences.ReadString('User Preferences','RecentFilters','') ;
  FileMaskDialog.PopulateFilterList(recentfilters);
  FileMaskDialog.FileMask.Text := '';
  if FileMaskDialog.ShowModal = mrOk  then
     begin
       newfilter := Trim(FileMaskDialog.FileMask.Text);
       FilterList.Items.Add(newfilter);
       RegExpList.Add(MakeRegExp(newfilter)) ;
       DelFilterButton.Enabled := true;
       ClearFiltersButton.Enabled := true;
       FilterList.ItemIndex := FilterList.Count-1;
       recentfilters:= '';
       if FileMaskDialog.FileMask.Items.Count > 0
          then recentfilters := FileMaskDialog.FileMask.Items[0];
       for x := 1 to FileMaskDialog.FileMask.Items.Count-1 do
           recentfilters := recentfilters + ';' + FileMaskDialog.FileMask.Items[x];
       Preferences.WriteString('User Preferences','RecentFilters', recentfilters);
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

procedure TSelectFilterForm.FormCreate ( Sender: TObject ) ;
begin
  RegExpList := TStringList.Create;
end;

procedure TSelectFilterForm.FormDestroy ( Sender: TObject ) ;
begin
  RegExpList.Free;
end;

procedure TSelectFilterForm.ApplyFilters ;
var
  x: Integer;
  searchstring: string;
begin
  if RegExpList.Count < 1 then exit;
      for x := 0 to FileView.Items.Count-1 do
         if not FileView.Items[x].HasChildren then  // avoid searching on directory nodes
             begin
               searchstring := TFileData(FileView.Items[x].Data).item[SEGFILENAME];
               if not FileNamesOnlyCheck.Checked
                     then searchstring := TFileData(FileView.Items[x].Data).item[SEGFILEPATH]
                                             + searchstring;
               if MatchAllCheck.Checked
                  then FileView.Items[x].MultiSelected := MatchesAllFilters(searchstring)
               else FileView.Items[x].MultiSelected := MatchesAnyFilter(searchstring);
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

function TSelectFilterForm.MatchesAllFilters ( aString: string ) : Boolean;
var
  x: Integer;
begin
  Result := true;
  for x := 0 to RegExpList.Count-1 do
      if not RegexpMatches(RegExpList.Strings[x], aString) then
         begin
           Result := false;
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

