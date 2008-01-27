unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DarIntf,
  Menus, ComCtrls, ExtCtrls, Buttons, StdCtrls, Process, LCLType,ProcessLine;

type

  { TMainForm }

  TMainForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    MenuBreak2: TMenuItem;
    MenuHelp: TMenuItem;
    miHelpAbout: TMenuItem;
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
    ToolbarPanel: TPanel;
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
    procedure miArchiveInformationClick(Sender: TObject);
    procedure miExitClick ( Sender: TObject ) ;
    procedure MessageHideButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miFileNewClick ( Sender: TObject ) ;
    procedure miFileOpenClick(Sender: TObject);
    procedure Splitter3ChangeBounds(Sender: TObject);
    procedure miHelpAboutClick ( Sender: TObject ) ;
    procedure miHideMessagesClick(Sender: TObject);
    procedure miRestoreAllClick(Sender: TObject);
    procedure tvMenuRestoreSelectedClick(Sender: TObject);
    procedure EnableArchiveMenus;
  private
    LevelColors: array[0..4] of TColor;
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;
  CurrentArchive: string;
  DarInfo: TDarInfo;
  SelectedNodes: integer;
  UpdatingSelection: Boolean;
  

  
const
  TEMPBATCHFILE = '/tmp/dargui.batch';
  ARCHIVEMENU_TAG = 1; //used for enabling menuitems after loading archive


implementation

uses selectrestore, archive, archiveinfo, About;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //set default colors for Treeview levels
  //at some point it will be possible for the user to change these
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
     
  GetTerminalCommand(TerminalCommand);
  RunscriptPath := GetRunscriptPath;

  UpdatingSelection := false;
  miHideMessages.Checked := false;
  
  writeln(DarInfo.version);
  
  if Paramcount > 0 then
       if FileExists(ParamStr(1)) then
          begin
            OpenDialog.FileName := ParamStr(1);
            if DarInfo.version<>'-'
               then if OpenArchive(OpenDialog.FileName,ArchiveTreeView) = 0
                    then EnableArchiveMenus;
          end;
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
    B: string[1];
  begin
  B := '';
  if ArchiveForm.NoCompressList.Count > 0 then
    begin
    BatchFile.Add('');
    BatchFile.Add('# Do not compress these files');
    for x := 0 to ArchiveForm.NoCompressList.Count-1 do
        BatchFile.Add('-Z ' + ArchiveForm.NoCompressList.Items[x]);
    end;
  if ArchiveForm.CompLwrLimitCombo.ItemIndex > -1
     then B := ArchiveForm.CompLwrLimitCombo.Items[ArchiveForm.CompLwrLimitCombo.ItemIndex][1];
  if not ((ArchiveForm.CompressionLwrLimit.Text = '100') and (B = 'b')) then
     begin
       BatchFile.Add(#10 + '# Do not compress files smaller than this');
       BatchFile.Add('-m ' + ArchiveForm.CompressionLwrLimit.Text + B);
     end;
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
     DarOptions := ' -X "' + ArchiveForm.ArchiveName.Text + '.*.dar"';
     BatchFile := TStringList.Create;
     BatchFile.Add('# DAR batch file written by DarGUI');
     BatchFile.Add('-R "' + ArchiveForm.BaseDirectory.Text + '"' + #10);
     if ArchiveForm.DryRunCheck.Checked then
        begin
          BatchFile.Add('# Dry run: ths archive will not be written to file unless the next option is removed');
          BatchFile.Add('--empty' + #10);
        end;
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
     if ArchiveForm.ExcludeDirectories.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add('# Directories to exclude from archive');
        for x := 0 to ArchiveForm.ExcludeDirectories.Count-1 do
            BatchFile.Add('-P "' + RemoveBaseDirectory(ArchiveForm.ExcludeDirectories.Items[x]) + '"');
        end;
     if ArchiveForm.ExcludeFiles.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add('# Files to exclude from archive');
        for x := 0 to ArchiveForm.ExcludeFiles.Count-1 do
            BatchFile.Add('-X "' + RemoveBaseDirectory(ArchiveForm.ExcludeFiles.Items[x]) + '"');
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
     if not ArchiveForm.ReadConfigCheck.Checked then
        begin
          BatchFile.Add('# Do not read DAR configuration files (~/.darrc or /etc/darrc)' + #10 + '-N' + #10);
        end;
     if ArchiveForm.EmptyDirCheck.Checked then
        begin
          BatchFile.Add('# Preserve ignored directory names' + #10 + '-D' +#10);
        end;

     Command := DAR_EXECUTABLE + ' -c "' + ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveName.Text + '"'
                            + ' -B "' + TEMPBATCHFILE + '"'
                            + ' -v'
                            //+ ' -e' // for debugging
                            + DarOptions
                            + ' -Q';
                            
     BatchFile.Insert(1, '# ' + Command);
     BatchFile.SaveToFile(TEMPBATCHFILE);

     MessageMemo.Lines.Add(#32 + StringOfChar('-',45));
     MessageMemo.Lines.Add('Creating archive: ' + ArchiveForm.ArchiveName.Text);

     if CreateArchive(Command, Left+100, Top+150) = 0 then
        begin
          CurrentArchive := ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveName.Text;
          OpenArchive(CurrentArchive, ArchiveTreeView);
          EnableArchiveMenus;
        end;
     if ArchiveForm.SaveScriptCheckBox.Checked
        then if ArchiveForm.ScriptFilenameBox.Text <> ''
             then WriteArchiveScript(ArchiveForm.ScriptFilenameBox.Text);
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

procedure TMainForm.miArchiveInformationClick(Sender: TObject);
begin
  if GetArchiveInformation(ExtractFilePath(OpenDialog.FileName)
                            + ArchiveTreeView.TopItem.Text,InformationForm.InformationMemo) = 0
      then
      begin
        InformationForm.Caption := 'Information for archive: ' + ArchiveTreeView.TopItem.Text;
        InformationForm.InformationMemo.Lines.Insert(0, 'Location: ' + OpenDialog.FileName);
        InformationForm.InformationMemo.Lines.Insert(1, StringOfChar('-',45));
        InformationForm.ShowModal;
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
var
  fn: String;
  x: Integer;
begin
  if OpenDialog.Execute then
     begin
       fn := OpenDialog.FileName;
       x := Length(fn) - 4;
       while fn[x] <> '.' do
             Dec(x);
       Delete(fn,x,100);
       if OpenArchive(fn, ArchiveTreeView) = 0 then
        begin
          EnableArchiveMenus;
          CurrentArchive := fn;
        end;
     end;

end;

procedure TMainForm.Splitter3ChangeBounds(Sender: TObject);
begin
  ArchiveTreeView.Paint;
end;

procedure TMainForm.miHelpAboutClick ( Sender: TObject ) ;
var
  Aboutform: TAboutForm;
begin
  Aboutform := TAboutForm.Create(Application);
  if DarInfo.version = '-'
     then AboutForm.DarVersionLabel.Caption := '* No DAR executable found! *'
     else AboutForm.DarVersionLabel.Caption := 'Using DAR version ' + DarInfo.version;
  AboutForm.ShowModal;
  Aboutform.Free;
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
  Batch: TStringList;
  x: Integer;
  Proc: TProcessLineTalk;
  daroptions: String;
  ProcCommandlineOption: String;
  aLine: String;
begin
  SelectedNodes := 0;
  Batch := TStringList.Create;
  Proc := TProcessLineTalk.Create(Application);
  RestoreForm := TExtractSelectedForm.Create(Self);
  RestoreForm.FullRestore := true;
  RestoreForm.Caption := 'Restore ' + IntToStr(ArchiveTreeView.Items.Count-1) + ' nodes...';
  RestoreForm.RestoreDirectoryEdit.Text := GetCurrentDir;
  if RestoreForm.ShowModal = mrOK then
     begin
       Batch.Add('-R "' + RestoreForm.RestoreDirectoryEdit.Text + '"' + #10);
       if RestoreForm.ExistingFiles.Count > 0 then
          begin
          Batch.Add('# We chose not to overwrite these files');
          for x := RestoreForm.ExistingFiles.Count-1 downto 0 do
              begin
                Batch.Add('-X "' + RestoreForm.ExistingFiles.Strings[x] + '"');
              end;
          end;
        daroptions := ' -O -v';
        if RestoreForm.FlatRestoreCheckBox.Checked then daroptions := daroptions + ' -f';
        case RestoreForm.OverwriteOptions.ItemIndex of
             0: daroptions := daroptions + '--no-warn';
             // DAR will try to interact - we need to work around this by anticipating overwrites
             1: ProcCommandlineOption := ' --no-warn';
             2: daroptions := daroptions + ' --no-overwrite';
             end;
        batch.SaveToFile(TEMPBATCHFILE);
        Proc.CommandLine := (DAR_EXECUTABLE + ' -x ' + ExtractFilePath(OpenDialog.FileName)
                          + ArchiveTreeView.TopItem.Text + ' -B "' + TEMPBATCHFILE + '" ' + daroptions + ProcCommandlineOption);
writeln('Executing: ', Proc.CommandLine);
        Proc.Options := Proc.Options + [poStdErrToOutput];
        Proc.Execute;
        while Proc.Running do
          begin
            aLine := Proc.ReadLine;
            while aLine <> '' do
                  begin
                    MessageMemo.Lines.Add(aLine);
                    aLine := Proc.ReadLine;
                    Application.ProcessMessages;
                  end;
          end;
        aLine := Proc.ReadLine;
        while aLine <> '' do
                begin
                MessageMemo.Lines.Add(aLine);
                aLine := Proc.ReadLine;
                end;
  WriteLn('-- executed --');
        end
        else MessageMemo.Lines.Add('Operation aborted: Restore selected files' );
  Batch.Free;
  RestoreForm.Free;
  Proc.Free;
end;


procedure TMainForm.tvMenuRestoreSelectedClick(Sender: TObject);
var
  x: integer;
  fd: TFileData;
  fn: string;
  batch : TStringList;
  RestoreForm: TExtractSelectedForm;
  daroptions: string;
  Proc : TProcessLineTalk;
  aLine: String;
  ProcCommandlineOption: String;

begin
  SelectedNodes := 0;
  MessageMemo.Clear;
  batch := TStringList.Create;
  RestoreForm := TExtractSelectedForm.Create(Self);
  RestoreForm.FullRestore := false;
  RestoreForm.RestoreDirectoryEdit.Text := GetCurrentDir;
  for x := 0 to ArchiveTreeView.Items.Count-1 do    {make list of selected files}
      if ArchiveTreeView.Items[x].MultiSelected then
         begin
         fd := TFileData(ArchiveTreeView.Items[x].Data);
         fn := fd.item[SEGFILEPATH] + fd.item[SEGFILENAME];
         RestoreForm.SelectedFiles.Add(fn);
         Inc(SelectedNodes);
         end;
  RestoreForm.Caption := 'Restore selected files';
  fd := TFileData(ArchiveTreeView.Selected.Data);
  if SelectedNodes > 0 then
       begin
       if RestoreForm.ShowModal = mrOK then
         if RestoreForm.SelectedFiles.Count > 0 then
            begin
              Proc := TProcessLineTalk.Create(Application);
              daroptions := ' -O -v';
              if RestoreForm.FlatRestoreCheckBox.Checked
                 then daroptions := daroptions + ' -f';
              case RestoreForm.OverwriteOptions.ItemIndex of
                   0: daroptions := daroptions + ' --no-warn';
                   // DAR will try to interact - we need to work around this by anticipating overwrites
                   1: ProcCommandlineOption := ' --no-warn';
                   2: daroptions := daroptions + ' --no-overwrite';
                   end;
              Batch.Add(#10 + '# Files to restore');
              for x := 0 to RestoreForm.SelectedFiles.Count-1 do
                  Batch.Add('-g "' + RestoreForm.SelectedFiles.Strings[x] + '"');
              batch.Insert(0,'-R "' + RestoreForm.RestoreDirectoryEdit.Text + '"');
              batch.SaveToFile(TEMPBATCHFILE);
              Proc.CommandLine := (DAR_EXECUTABLE + ' -x "' + CurrentArchive + '" -B "' + TEMPBATCHFILE + '" ' + daroptions + ProcCommandlineOption);
              writeln('Executing: ', Proc.CommandLine);
              Proc.Options := Proc.Options + [poStdErrToOutput];
              //TODO: check that some files do need to be restored: ie not all excluded by overwrite rule
              Proc.Execute;
              while Proc.Running do
                begin
                  aLine := Proc.ReadLine;
                  while aLine <> '' do
                        begin
                          MessageMemo.Lines.Add(aLine);
                          aLine := Proc.ReadLine;
                          Application.ProcessMessages;
                        end;
                end;
              aLine := Proc.ReadLine;
              while aLine <> '' do
                      begin
                      MessageMemo.Lines.Add(aLine);
                      aLine := Proc.ReadLine;
                      end;
              WriteLn('-- executed --');
              Proc.Free;
            end
            else MessageMemo.Lines.Add('Operation aborted: Restore selected files' );
       end
       else ShowMessage('Error: no files were selected');
  RestoreForm.Free;
  batch.Free;
end;

procedure TMainForm.EnableArchiveMenus;
var
  x: Integer;
begin
  for x := 0 to ComponentCount-1 do
      if Components[x] is TMenuItem then
         if TMenuItem(Components[x]).Tag = ARCHIVEMENU_TAG
            then TMenuItem(Components[x]).Enabled := true;
end;


initialization
  {$I main.lrs}

end.

