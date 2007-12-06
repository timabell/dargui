unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DarIntf,
  Menus, ComCtrls, ExtCtrls, Buttons, StdCtrls, Process, LCLType;

type

  { TMainForm }

  TMainForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    MenuBreak2: TMenuItem;
    miArchiveInformation: TMenuItem;
    miHideMessages: TMenuItem;
    MenuSettings: TMenuItem;
    MessageHideButton: TBitBtn;
    MessageLabel: TLabel;
    MainMenu: TMainMenu;
    MessageMemo: TMemo;
    MenuFile: TMenuItem;
    MenuBreak1: TMenuItem;
    MenuArchive: TMenuItem;
    miRestoreSelected: TMenuItem;
    miRestoreAll: TMenuItem;
    MessagePanel: TPanel;
    MessageClosePanel: TPanel;
    MessageBoxSplitter: TSplitter;
    tvMenuRestoreSelected: TMenuItem;
    tvMenuRestoreAll: TMenuItem;
    miExit: TMenuItem;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    HeaderPanel: TPanel;
    FilenameHdr: TPanel;
    DateHdr: TPanel;
    TreeViewMenu: TPopupMenu;
    SizeHdr: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    UserHdr: TPanel;
    GroupHdr: TPanel;
    StatusHdr: TPanel;
    StatusBar1: TStatusBar;
    ArchiveTreeView: TTreeView;
    procedure ArchiveTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ArchiveTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure ArchiveTreeViewSelectionChanged(Sender: TObject);
    procedure miExitClick ( Sender: TObject ) ;
    procedure MessageHideButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miFileNewClick ( Sender: TObject ) ;
    procedure miFileOpenClick(Sender: TObject);
    procedure Splitter3ChangeBounds(Sender: TObject);
    procedure miHideMessagesClick(Sender: TObject);
    procedure miRestoreAllClick(Sender: TObject);
    procedure tvMenuRestoreSelectedClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;
  DarInfo: TDarInfo;
  SelectedNodes: integer;
  UpdatingSelection: Boolean;
  
const
  TEMPBATCHFILE = '/tmp/dargui.batch';

implementation

uses selectrestore, archive;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DarInfo := GetDarVersion;
  UpdatingSelection := false;
  miHideMessages.Checked := false;
  
  writeln(DarInfo.version);
       miRestoreAll.Enabled := true;
       miRestoreSelected.Enabled := true;

  OpenDialog.FileName := '/home/malcolm/test_backup.1.dar';
  //OpenDialog.FileName := '/media/hdb2/backups/archive_2007-08-29.1.dar';
  OpenArchive(OpenDialog.FileName,ArchiveTreeView);

end;

procedure TMainForm.miFileNewClick ( Sender: TObject ) ;
begin
  ArchiveForm.ShowModal;
end;

procedure TMainForm.ArchiveTreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  TFileData(Node.Data).Free;
end;

procedure TMainForm.ArchiveTreeViewSelectionChanged(Sender: TObject);
var
  x: integer;
begin
  if not UpdatingSelection then
  begin
  SelectedNodes := 0;
  for x := 0 to TTreeview(Sender).Items.Count-1 do
      begin
      if TTreeview(Sender).Items[x].MultiSelected then
         begin
         Inc(SelectedNodes);
         end;
      if TTreeview(Sender).Items[x].Parent <> nil then
         begin
         if TTreeview(Sender).Items[x].Parent.MultiSelected then
            begin
            writeln(TTreeview(Sender).Items[x].Parent.Text);
            UpdatingSelection := true;
            TTreeview(Sender).Items[x].MultiSelected := true;
            Inc(SelectedNodes);
            //TTreeview(Sender).Items[x].MultiSelected := TTreeview(Sender).Items[x].Parent.MultiSelected;
            //if TTreeview(Sender).Items[x].Parent.MultiSelected then
               //Inc(SelectedNodes);
            UpdatingSelection := false;
            ////if TTreeview(Sender).Items[x].Expanded then writeln('expanded: ' + TTreeview(Sender).Items[x].Text);
            end;
         end;
      end;

     writeln(SelectedNodes);
  end;
end;


procedure TMainForm.miExitClick ( Sender: TObject ) ;
begin
  Close;
end;

procedure TMainForm.MessageHideButtonClick(Sender: TObject);
begin
  MessagePanel.Visible := false;
  MessageBoxSplitter.Visible := false;
end;


procedure TMainForm.ArchiveTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  FullRect, DisplayRect : TRect;
 TextStyle: TTextstyle;
 
     function TrimText(aText: string; aCol, leftMargin: integer): string;
     var
       a,
       maxWidth,
       textWidth,
       dotWidth: integer;
     const
       CELL_RIGHT_MARGIN = 2;
     begin
       Result := aText;
       case aCol of
               SEGSTATUS      : maxWidth := StatusHdr.Width-CELL_RIGHT_MARGIN -leftMargin;
               SEGPERMISSIONS : ;  // not displayed at present
               SEGUSER        : maxWidth := UserHdr.Width-CELL_RIGHT_MARGIN -leftMargin;
               SEGGROUP       : maxWidth := GroupHdr.Width-CELL_RIGHT_MARGIN -leftMargin;
               SEGSIZE        : maxWidth := SizeHdr.Width-CELL_RIGHT_MARGIN -leftMargin;
               SEGDATE        : maxWidth := DateHdr.Width-CELL_RIGHT_MARGIN -leftMargin;
               SEGFILENAME    : maxWidth := FilenameHdr.Width -CELL_RIGHT_MARGIN -leftMargin;
               end;
       a := Length(aText);
       textWidth := Canvas.TextWidth(aText);
       dotWidth := Canvas.TextWidth('...');
       if textWidth > maxWidth then
          begin
             while ((textWidth + dotWidth) > maxWidth) and (a > 0) do
                   begin
                   Dec(a);
                   textWidth := Canvas.TextWidth(Copy(aText, 1, a));
                   end;
          end;
       if a < LEngth(aText)
          then Result :=  Copy(aText, 1, a) + '...';
     end;

begin
      Fullrect := Node.DisplayRect(false);
      Displayrect := Node.DisplayRect(true);
      Displayrect.Right := FullRect.Right;
//      writeln(DisplayRect.Top,' : ', DisplayRect.Bottom);
      with Sender.Canvas do
        Case Node.Level of
        0: begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := clGray;
             Font.Name:='Arial';
             Font.Size:=9;
             Font.Style:=[];
           end;
        1: begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := clRed;
             Font.Name:='Verdana';
             Font.Size:=9;
             Font.Style:=[];
           end;
        2: begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := clGreen;
             Font.Name:='Times New Roman';
             Font.Size:=9;
             Font.Style:=[fsItalic];
           end;
        else begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := clNavy;
             Font.Name:='Verdana';
             Font.Size:=9;
             Font.Style:=[fsBold];
           end;
        end;//Case
  if stage = cdPostPaint then DefaultDraw := false;
  Sender.Canvas.FillRect(Displayrect);
  Sender.Canvas.TextOut(Displayrect.Left,DisplayRect.Top+1,
            TrimText(TFileData(Node.Data).item[SEGFILENAME], SEGFILENAME,Displayrect.Left));
  Sender.Canvas.TextOut(DateHdr.Left,DisplayRect.Top+1,
            TrimText(TFileData(Node.Data).item[SEGDATE], SEGDATE, 0));
  Sender.Canvas.TextOut(SizeHdr.Left,DisplayRect.Top+1,
            TrimText(TFileData(Node.Data).item[SEGSIZE], SEGSIZE, 0));
  Sender.Canvas.TextOut(UserHdr.Left,DisplayRect.Top+1,
            TrimText(TFileData(Node.Data).item[SEGUSER], SEGUSER, 0));
  Sender.Canvas.TextOut(GroupHdr.Left,DisplayRect.Top+1,
            TrimText(TFileData(Node.Data).item[SEGGROUP], SEGGROUP, 0));
  Sender.Canvas.TextOut(StatusHdr.Left,DisplayRect.Top+1,
            TrimText(TFileData(Node.Data).item[SEGSTATUS], SEGSTATUS, 0));
end;

procedure TMainForm.miFileOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
     begin
       OpenArchive(OpenDialog.FileName, ArchiveTreeView);
       miRestoreAll.Enabled := true;
       miRestoreSelected.Enabled := true;
     end;
end;

procedure TMainForm.Splitter3ChangeBounds(Sender: TObject);
begin
  ArchiveTreeView.Paint;
end;

procedure TMainForm.miHideMessagesClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if Sender = miHideMessages
     then MessagePanel.Visible := not miHideMessages.Checked;
  MessageBoxSplitter.Visible := MessagePanel.Visible;
end;

procedure TMainForm.miRestoreAllClick(Sender: TObject);
var
  RestoreForm: TExtractSelectedForm;
begin
  SelectedNodes := 0;
  RestoreForm := TExtractSelectedForm.Create(Self);
  RestoreForm.Caption := 'Restore ' + IntToStr(ArchiveTreeView.Items.Count-1) + ' files...';
  RestoreForm.Panel1.Visible := false;
  RestoreForm.RestoreDirectoryEdit.Text := GetCurrentDir;
  RestoreForm.Height := RestoreForm.Height - RestoreForm.FileCheckListBox.Height;
  //writeln('dar -x ' + ArchiveTreeView.TopItem.Text + #32 + restorefiles + ' -O' );
  //RestoreForm.CommandLine := 'dar -x ' + ArchiveTreeView.TopItem.Text + #32 ;
  RestoreForm.ShowModal
end;

procedure TMainForm.tvMenuRestoreSelectedClick(Sender: TObject);
var
  x: integer;
  fd: TFileData;
  restorefiles: string;
  batch : TStringList;
  RestoreForm: TExtractSelectedForm;
  daroptions: string;
  Proc : TProcess;
  Output: TStringList;
  M: TMemoryStream;
  n: LongInt;
  BytesRead: LongInt;
StdInBuf, StdOutBuf, OutputLine : string;
pid, status, i , Count, Linestart : longint;

begin
  SelectedNodes := 0;
  batch := TStringList.Create;
  RestoreForm := TExtractSelectedForm.Create(Self);
  RestoreForm.RestoreDirectoryEdit.Text := GetCurrentDir;
  for x := 0 to ArchiveTreeView.Items.Count-1 do
      if ArchiveTreeView.Items[x].MultiSelected then
         begin
         Inc(SelectedNodes);
         fd := TFileData(ArchiveTreeView.Items[x].Data);
         RestoreForm.FileCheckListBox.Items.Add(fd.item[SEGFILEPATH] + fd.item[SEGFILENAME]);
         RestoreForm.FileCheckListBox.Checked[RestoreForm.FileCheckListBox.Count-1] := true;
         restorefiles := restorefiles + ' -g "' + fd.item[SEGFILEPATH] + fd.item[SEGFILENAME] + '" ';
         batch.Add('-g "' + fd.item[SEGFILEPATH] + fd.item[SEGFILENAME] + '" ');
         end;
     writeln(SelectedNodes);
  RestoreForm.Caption := 'Restore ' + IntToStr(SelectedNodes) + ' files...';
  fd := TFileData(ArchiveTreeView.Selected.Data);
  //RestoreForm.CommandLine := 'dar -x ' + ArchiveTreeView.TopItem.Text + #32 + restorefiles + ' -O';
  if SelectedNodes > 0
     then
     begin
     if RestoreForm.ShowModal = mrOK then
     begin
          Proc := TProcess.Create(Application);
          M := TMemoryStream.Create;
          BytesRead := 0;
          daroptions := ' -O';
          if not RestoreForm.OverwriteCheckBox.Checked then daroptions := daroptions + ' -w';
          batch.Insert(0,'-R "' + RestoreForm.RestoreDirectoryEdit.Text + '"');
          batch.SaveToFile(TEMPBATCHFILE);
          Proc.CommandLine := ('dar -x ' + ExtractFilePath(OpenDialog.FileName)
                            + ArchiveTreeView.TopItem.Text + ' -B "' + TEMPBATCHFILE + '" ' + daroptions);
          writeln(Proc.CommandLine);
          Proc.Options := [poUsePipes, poStdErrToOutput];
          RestoreForm.Free;
          Proc.Execute;
        while Proc.Running do
          begin
          Application.ProcessMessages;
          SetLength(StdOutBuf,0);
          Count := 0;
          if Proc.Output <> nil then
                begin
                SetLength(StdOutBuf, BytesRead + READ_BYTES);
                BytesRead := Proc.Output.Read(StdOutBuf[Count+1], length(StdOutBuf));
                writeln(BytesRead);
                Count := Count + BytesRead;
                while BytesRead = READ_BYTES do
                      begin
                      SetLength(StdOutBuf, Count + READ_BYTES);
                      BytesRead := Proc.Output.Read(StdOutBuf[Count+1], READ_BYTES);
                      Count := Count + BytesRead;
                      end;
                end;
          i := 1;
          while i <= Count do
                begin
                if (StdOutBuf[i] in [#10,#13]) or (i = Count) then
                   begin
                   OutputLine := copy(StdOutBuf, LineStart, i-LineStart);
                   if Pos('is about to be over', OutputLine) > 0 then
                        case MessageDlg('Confirm overwrite file',
                              'Overwrite ' + Copy(OutputLine, 1, Pos('is about to be over', OutputLine)-2),
                              mtConfirmation,
                              [mbYes, mbNo, mbCancel], 0)
                                of
                                mrYes: begin
                                       SetLength(StdInBuf, 1);
                                       StdInBuf[1] := CHR(10);
                                       writeln('Bytes sent ',Proc.Input.Write(StdInBuf[1], Length(StdInBuf)));
                                       end;
                                mrNo: begin
                                       SetLength(StdInBuf, 1);
                                       StdInBuf[1] := CHR(27);
                                       //writeln('Bytes sent ',Proc.Input.Write(StdInBuf[1], Length(StdInBuf)));
                                       i := Count + 1;
                                       write('.');
                                       end;
                                mrCancel: Proc.Terminate(0);
                                end;
                   if OutputLine <> '' then
                      MessageMemo.Lines.Add(OutputLine);
                   OutputLine := '';
                   if (i<Count) and (StdOutBuf[i+1] in [#10,#13]) and (StdOutBuf[i]<>StdOutBuf[i+1])
                      then Inc(i);
                   LineStart := i+1;
                   Application.ProcessMessages;
                   end;
                Inc(i);
                end;
          BytesRead := 0;
          write('.');
          end;

  WriteLn('-- executed --');

//  MessageMemo.Lines.LoadFromStream(M);
  WriteLn('-- linecount = ', MessageMemo.Lines.Count, ' --');


        end;
     end
     else ShowMessage('Error: no files were selected');
  batch.Free;
  Proc.Free;
  M.Free;
end;

initialization
  {$I main.lrs}

end.

