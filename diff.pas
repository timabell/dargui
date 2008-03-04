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
    Label1: TLabel;
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
    procedure FormResize ( Sender: TObject ) ;
    procedure OKButtonClick ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 



implementation

uses darintf;

{ TDiffForm }

procedure TDiffForm.BrowseArchiveClick ( Sender: TObject ) ;
begin
  if OpenDialog.Execute
     then ArchiveBox.Text := TrimToBase(OpenDialog.FileName);
end;

procedure TDiffForm.BrowseDirectoryClick ( Sender: TObject ) ;
begin
  SelectDirectoryDialog.InitialDir := BaseDirBox.Text;
  if SelectDirectoryDialog.Execute
     then BaseDirBox.Text := SelectDirectoryDialog.FileName;
end;

procedure TDiffForm.FormResize ( Sender: TObject ) ;
begin
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
  if not FileExists(ArchiveBox.Text + '.1.dar')
     then begin
       ShowMessage('Unable to find ' + ArchiveBox.Text + '.1.dar');
       ArchiveBox.SetFocus;
       exit;
     end;
  if not FileExists(BaseDirBox.Text)
     then begin
       ShowMessage('Error: ' + BaseDirBox.Text + ' not found');
       BaseDirBox.SetFocus;
       exit;
     end;
  LogFile := GetNextFileName(TEMP_DIRECTORY + LOGFILE_BASE);
  if VerboseCheck.Checked then verboseflag := ' -v';
  Cmd :=  DAR_EXECUTABLE + ' -d ' + ArchiveBox.Text + ' -R ' + BaseDirBox.Text + verboseflag;
  if RunDarCommand(Cmd, 'comparing files...', Left+100, Top+150) = 0
     then begin
       ResultListBox.Items.LoadFromFile(LogFile);
       if Pos(DAR_EXECUTABLE, ResultListBox.Items[0]) = 1
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
              ReportLabel.Caption := IntToStr(InodeCount) + ' inodes checked : ' + IntToStr(DiffCount) + ' differences found';
              if verboseflag = ' -v' then
                for x := ResultListBox.Count-1 downto InodeCount do
                    ResultListBox.Items.Delete(x)
                else
                for x := ResultListBox.Count-1 downto DiffCount do
                    ResultListBox.Items.Delete(x);
            except
              Showmessage('diff.pas line 103 : Error when converting ' + aLine);
            end
            else ReportLabel.Caption := 'No statistics available - operation aborted?';
          end;

       DialogPanel.Visible := false;
       DialogButtonPanel.Visible := false;
       ResultPanel.Visible := true;
       ResultButtonPanel.Visible := true;
       FormResize(nil);
     end;

  
end;

initialization
  {$I diff.lrs}

end.

