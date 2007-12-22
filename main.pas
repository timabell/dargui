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
    LevelColors: array[0..4] of TColor;
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
  //set default colors for Treeview levels
  LevelColors[0] := clBlack;
  LevelColors[1] := clBlack;
  LevelColors[2] := clBlack;
  LevelColors[3] := clBlack;
  LevelColors[4] := clBlack;

  DarInfo := GetDarVersion;
  if DarInfo.version='-' then
     begin
     MessageDlg('Unable to locate dar executable: nothing will work!',
                        mtWarning,
                        [mbOk],
                        0);
     end;
     
  UpdatingSelection := false;
  miHideMessages.Checked := false;
  
  writeln(DarInfo.version);
       miRestoreAll.Enabled := true;
       miRestoreSelected.Enabled := true;

  OpenDialog.FileName := '/home/malcolm/test_backup.1.dar';
  //OpenDialog.FileName := '/media/hdb2/backups/archive_2007-08-29.1.dar';
  if DarInfo.version<>'-'
     then OpenArchive(OpenDialog.FileName,ArchiveTreeView);

end;

procedure TMainForm.miFileNewClick ( Sender: TObject ) ;
var
  BatchFile: TStringList;
  DarOptions: string;
  Command: string;
  x: integer;
  
  procedure AddCompressionOptions;
  var
    x: integer;
    ByteSize: string[1];
  begin
  ByteSize := '';
  if ArchiveForm.NoCompressList.Count > 0 then
    begin
    BatchFile.Add('');
    BatchFile.Add('# Do not compress these files');
    for x := 0 to ArchiveForm.NoCompressList.Count-1 do
        BatchFile.Add('-Z ' + ArchiveForm.NoCompressList.Items[x]);
    end;
  BatchFile.Add('');
  BatchFile.Add('# Do not compress files smaller than this');
  if ArchiveForm.CompLwrLimitCombo.ItemIndex > 0
     then ByteSize := ArchiveForm.CompLwrLimitCombo.Text[1];
  BatchFile.Add('-m ' + ArchiveForm.CompressionLwrLimit.Text + ByteSize);
  end;
  
  function RemoveBaseDirectory(aFilePath: string): string;
  begin
    if Pos(ArchiveForm.BaseDirectory.Text, aFilePath) = 1 then
       begin
       Result := Copy(aFilePath, Length(ArchiveForm.BaseDirectory.Text)+1, READ_BYTES);
       end
       else
       begin
       ShowMessage('Error: Base Directory not in ' + aFilePath);
       Result := '';
       end;
  end;
  
begin
  if ArchiveForm.ShowModal = mrOk then
     try
     DarOptions := ' -X "' + ArchiveForm.ArchiveName.Text + '"';
     BatchFile := TStringList.Create;
     BatchFile.Add('# DAR batch file written by DarGUI');
     BatchFile.Add('-R "' + ArchiveForm.BaseDirectory.Text + '"');
     if ArchiveForm.IncludeDirectories.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add('# Directories to include in archive');
        for x := 0 to ArchiveForm.IncludeDirectories.Count-1 do
            BatchFile.Add('-g "' + RemoveBaseDirectory(ArchiveForm.IncludeDirectories.Items[x]) + '"');
        end;
     if ArchiveForm.IncludeFiles.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add('# Files to include in archive');
        for x := 0 to ArchiveForm.IncludeFiles.Count-1 do
            BatchFile.Add('-g "' + RemoveBaseDirectory(ArchiveForm.IncludeFiles.Items[x]) + '"');
        end;
        writeln('excludedirs: ',ArchiveForm.ExcludeDirectories.Count);
     if ArchiveForm.ExcludeDirectories.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add('# Directories to exclude from archive');
        for x := 0 to ArchiveForm.ExcludeDirectories.Count-1 do
            BatchFile.Add('-P "' + RemoveBaseDirectory(ArchiveForm.ExcludeDirectories.Items[x]) + '"');
        end;
     if ArchiveForm.GZipCheck.Checked
        then begin
             BatchFile.Add('');
             BatchFile.Add('# Use gzip compression');
             BatchFile.Add('--gzip=' + ArchiveForm.CompressionLevel.Text);
             AddCompressionOptions;
             end
     else if ArchiveForm.Bzip2Check.Checked
        then begin
             BatchFile.Add('');
             BatchFile.Add('# Use bzip2 compression');
             BatchFile.Add('--bzip2=' + ArchiveForm.CompressionLevel.Text);
             AddCompressionOptions;
             end;

     Command := DAR_EXECUTABLE + ' -c "' + ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveName.Text + '"'
                            + ' -B "' + TEMPBATCHFILE + '"'
                           // + ' -v -e' // for debugging
                            + DarOptions
                            + ' -X ' + ArchiveForm.ArchiveName.Text + '.*.dar';
                            
     BatchFile.Insert(1, '# ' + Command);
     BatchFile.SaveToFile(TEMPBATCHFILE);
     WriteLn(Command);
     CreateArchive(Command,MessageMemo);
     OpenArchive(ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveName.Text, ArchiveTreeView);
     finally
     BatchFile.Free;
     end;
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
            UpdatingSelection := false;
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
  miHideMessages.Checked := true;
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
      with Sender.Canvas do
        Case Node.Level of
        0: begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := LevelColors[Node.Level];
             Font.Size:=9;
           end;
        1: begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := LevelColors[Node.Level];
             Font.Size:=9;
           end;
        2: begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := LevelColors[Node.Level];
             Font.Size:=9;
           end;
        else begin
             if Node.MultiSelected then
                Font.Color := clWhite
             else
                Font.Color := LevelColors[Node.Level];
             Font.Size:=9;
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
  writeln('TMainForm.miRestoreAllClick incomplete');
  //RestoreForm.ShowModal;
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
  MessageMemo.Clear;
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
          daroptions := ' -O -v';
          if RestoreForm.FlatRestoreCheckBox.Checked then daroptions := daroptions + ' -f';
          if not RestoreForm.OverwriteCheckBox.Checked then daroptions := daroptions + ' -w';
          batch.Insert(0,'-R "' + RestoreForm.RestoreDirectoryEdit.Text + '"');
          batch.SaveToFile(TEMPBATCHFILE);
          Proc.CommandLine := (DAR_EXECUTABLE + ' -x ' + ExtractFilePath(OpenDialog.FileName)
                            + ArchiveTreeView.TopItem.Text + ' -B "' + TEMPBATCHFILE + '" ' + daroptions);
          //showMessage(Proc.CommandLine);
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
                   if Pos('(less destructive choice)', OutputLine) <> 0 then OutputLine := '';
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

