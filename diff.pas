unit diff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TDiffForm }
  
  TDiffForm = class ( TForm )
    CloseButton: TBitBtn;
    ImageList1: TImageList;
    ResultsLabel: TLabel;
    ReportLabel: TLabel;
    ResultListBox: TListBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    BrowseArchive: TButton;
    BrowseDirectory: TButton;
    OpenDialog: TOpenDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    VerboseCheck: TCheckBox;
    DialogButtonPanel: TPanel;
    ArchiveBox: TLabeledEdit;
    BaseDirBox: TLabeledEdit;
    ResultButtonPanel: TPanel;
    DialogPanel: TPanel;
    ResultPanel: TPanel;
    procedure BrowseArchiveClick ( Sender: TObject ) ;
    procedure BrowseDirectoryClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormResize ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
    procedure ResultListBoxDrawItem ( Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState ) ;
    procedure InitialiseInterface;
  private
    { private declarations }
  public
    { public declarations }
  end; 



implementation

uses dgStrConst, darintf;

{ TDiffForm }

procedure TDiffForm.BrowseArchiveClick ( Sender: TObject ) ;
begin
  if DirectoryExists( ExtractFilePath( ArchiveBox.Text ) )
     then OpenDialog.InitialDir := ExtractFilePath( ArchiveBox.Text )
     else OpenDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  if OpenDialog.Execute
     then ArchiveBox.Text := TrimToBase(OpenDialog.FileName);
end;

procedure TDiffForm.BrowseDirectoryClick ( Sender: TObject ) ;
begin
  if DirectoryExists( BaseDirBox.Text )
     then SelectDirectoryDialog.InitialDir :=  BaseDirBox.Text
     else SelectDirectoryDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');
  if SelectDirectoryDialog.Execute
     then BaseDirBox.Text := SelectDirectoryDialog.FileName;
end;

procedure TDiffForm.FormCreate ( Sender: TObject ) ;
begin
  InitialiseInterface;
  ResultListBox.Style := lbOwnerDrawFixed;
end;

procedure TDiffForm.FormResize ( Sender: TObject ) ;
begin
  ArchiveBox.Width := Width-170;
  BrowseArchive.Left := Width-130;
  BaseDirBox.Width := Width-170;
  BrowseDirectory.Left := Width-130;
  ResultListBox.Height := ResultPanel.Height-70;
  CloseButton.Left := (ResultPanel.Width - CloseButton.Width) div 2;
  OKButton.Left := (DialogButtonPanel.Width div 2) - OKButton.Width - 20;
  CancelButton.Left := (DialogButtonPanel.Width div 2) + 20;
end;

procedure TDiffForm.OKButtonClick ( Sender: TObject ) ;
var
  verboseflag: String;
  Cmd: String;
  LogFile: String;
  x: Integer;
  aLine: string;
  InodeCount: LongInt;
  DiffCount: LongInt;
begin
  if ArchiveBox.Text = '' then
     begin
       ShowMessage(rsMessInvalidArchiveName);
       ArchiveBox.SetFocus;
       exit;
     end;
  if not FileExists(ArchiveBox.Text + '.1.dar')
     then begin
       ShowMessage ( Format ( rsMessUnableToFind1Dar, [ ArchiveBox.Text ] ) ) ;
       ArchiveBox.SetFocus;
       exit;
     end;
  if not FileExists(BaseDirBox.Text)
     then begin
       ShowMessage ( Format ( rsErrErrorNotFound, [ BaseDirBox.Text ] ) ) ;
       BaseDirBox.SetFocus;
       exit;
     end;
  LogFile := GetNextFileName(TEMP_DIRECTORY + LOGFILE_BASE);
  if VerboseCheck.Checked then verboseflag := ' -v';
  Cmd :=  DAR_EXECUTABLE + ' -d "' + ArchiveBox.Text + '" -R "' + BaseDirBox.Text + '"' + verboseflag;
  if ArchiveIsEncrypted(ArchiveBox.Text, nil)
     then Cmd := Cmd + ' -K :';
  if RunDarCommand ( Cmd, rsCptComparingFiles, Left + 100, Top + 150 , true) = 0
     then begin
       ResultListBox.Items.LoadFromFile(LogFile);
       if Pos(DAR_EXECUTABLE, ResultListBox.Items[0]) = 1
          then ResultListBox.Items.Delete(0);
       if Pos('Warning,', ResultListBox.Items[0]) = 1
          then ResultListBox.Items.Delete(0);
       if Pos('Extracting contents ', ResultListBox.Items[0]) = 1
          then ResultListBox.Items.Delete(0);
       if ResultListBox.Count > 6 then
          begin
            x := ResultListBox.Count-1;
            aLine := Trim(ResultListBox.Items[x]);
            while (Pos('inode(s) treated',aline) < 1) and (x > 0) do
                  begin
                    Dec(x);
                    aLine := Trim(ResultListBox.Items[x]);
                  end;
          if x > 0 then
            Try
              writeln(Copy(aLine, 1, Pos(#32, aLine)-1));
              InodeCount := StrToInt(Copy(aLine, 1, Pos(#32, aLine)-1));
              aLine :=  Trim(ResultListBox.Items[x+1]);
              DiffCount := StrToInt(Copy(aLine, 1, Pos(#32, aLine)-1));
              ReportLabel.Caption := Format ( rsInodesCheckedDiffere, [
                IntToStr ( InodeCount ) , IntToStr ( DiffCount ) ] ) ;
              if DiffCount > InodeCount then InodeCount := DiffCount;
              if verboseflag = ' -v' then
                for x := ResultListBox.Count-1 downto InodeCount do
                    ResultListBox.Items.Delete(x)
                else
                for x := ResultListBox.Count-1 downto DiffCount do
                    ResultListBox.Items.Delete(x);
            except
              Showmessage('diff.pas line 103 : Error when converting ' + aLine);
            end
            else ReportLabel.Caption := rsMessNoStatisticsAvailabl;
          end;
       DialogPanel.Visible := false;
       DialogButtonPanel.Visible := false;
       ResultPanel.Visible := true;
       ResultButtonPanel.Visible := true;
       Constraints.MaxHeight := 0;
       Constraints.MinHeight := 0;
       FormResize(nil);
     end;
end;

procedure TDiffForm.ResultListBoxDrawItem ( Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState ) ;
var
  Glyph:TBitmap;
  aFile: string;
  
  function DiffState: Integer;
  begin
    result := -1;
    if Pos('OK',aFile) = 1 then
       begin
         Result := 0;
        end;
    if Pos('DIFF',aFile) = 1 then
       begin
       if Pos(': file not present', aFile) > 0
          then Result := 2
          else Result := 1;
       end;
    aFile := Copy(aFile, 5, 512);
  end;
  
begin
  if (Index < 0)
  or (Index >= TListBox(Control).Count)
     then exit;
  Glyph := TBitmap.Create;
  with TListBox(Control) do
    try
      aFile := Items[Index];
      ImageList1.GetBitmap(DiffState, Glyph);
      Canvas.FillRect(ARect);
      Canvas.Draw(2, ARect.Top+2, Glyph);
      Canvas.TextRect(ARect, ARect.Left+2 + Glyph.Width, ARect.Top+2, aFile);
    finally
      Glyph.Free;
    end;
end;

procedure TDiffForm.InitialiseInterface;
begin
  Self.Caption := rsHintCompareArchiveFile;
  OpenDialog.Title := rsSelectFile;
  ResultsLabel.Caption := rsResults;
  CloseButton.Caption := rsButtonClose;
  OKButton.Caption := rsButtonOK;
  CancelButton.Caption := rsButtonCancel;
  BrowseArchive.Caption := rsButtonBrowse;
  BrowseDirectory.Caption := rsButtonBrowse;
  VerboseCheck.Caption := rsVerboseOutput;
  BaseDirBox.EditLabel.Caption := rsBaseDirectory;
  ArchiveBox.EditLabel.Caption := rsArchive;
end;


initialization
  {$I diff.lrs}

end.

