unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DarIntf,
  Menus, ComCtrls, ExtCtrls, Buttons, StdCtrls, Process, LCLType, historymenu, regexpr;

type

  { TMainForm }

  TMainForm = class(TForm)
    IconList: TImageList;
    MenuBreak3: TMenuItem;
    MenuBreak4: TMenuItem;
    PopupBreak2: TMenuItem;
    pmiShowAll: TMenuItem;
    pmiHideAll: TMenuItem;
    pmiShowAllSelected: TMenuItem;
    pmiSelectFilter: TMenuItem;
    miToggleSelect: TMenuItem;
    PopupBreak1: TMenuItem;
    pmiToggleSelect: TMenuItem;
    miShowToolbar: TMenuItem;
    miOpenRecent: TMenuItem;
    miHelpDar: TMenuItem;
    tbDiff: TBitBtn;
    tbOpen: TBitBtn;
    tbCreate: TBitBtn;
    tbIsolate: TBitBtn;
    MenuBreak2: TMenuItem;
    MenuHelp: TMenuItem;
    miIsolate: TMenuItem;
    miOperationlogs: TMenuItem;
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
    pmiRestoreSelected: TMenuItem;
    pmiRestoreAll: TMenuItem;
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
    StatusBar: TStatusBar;
    ArchiveTreeView: TTreeView;
    procedure ArchiveTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ArchiveTreeViewCompare ( Sender: TObject; Node1,
      Node2: TTreeNode; var Compare: Integer ) ;
    procedure ArchiveTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure ArchiveTreeViewSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure RecentMenuClick ( Sender: TObject ) ;
    procedure ToolbarPanelClick ( Sender: TObject ) ;
    procedure miArchiveInformationClick(Sender: TObject);
    procedure miExitClick ( Sender: TObject ) ;
    procedure MessageHideButtonClick(Sender: TObject);
    procedure miFileNewClick ( Sender: TObject ) ;
    procedure miFileOpenClick(Sender: TObject);
    procedure Splitter3ChangeBounds(Sender: TObject);
    procedure miHelpAboutClick ( Sender: TObject ) ;
    procedure miHelpDarClick ( Sender: TObject ) ;
    procedure miHideMessagesClick(Sender: TObject);
    procedure miIsolateClick ( Sender: TObject ) ;
    procedure miOperationlogsClick ( Sender: TObject ) ;
    procedure miRestoreAllClick(Sender: TObject);
    procedure pmiHideAllClick ( Sender: TObject ) ;
    procedure pmiShowAllClick ( Sender: TObject ) ;
    procedure pmiShowAllSelectedClick ( Sender: TObject ) ;
    procedure miShowToolbarClick ( Sender: TObject ) ;
    procedure pmiSelectFilterClick ( Sender: TObject ) ;
    procedure pmiToggleSelectClick ( Sender: TObject ) ;
    procedure tbDiffClick ( Sender: TObject ) ;
    procedure pmiRestoreSelectedClick(Sender: TObject);
    procedure EnableArchiveMenus;
    procedure InitialiseInterface;
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
  AllowRecursiveSelect: Boolean;
  UserHome: string;
  SettingsDir: string;
  RecentList : TRecentFiles;
  

  
const
  APP_VERSION = '0.2.1';
  SVN_REVISION = '';

  ARCHIVEMENU_TAG = 1; //used for enabling menuitems after loading archive
  SELECT_STATUSBAR = 0;   //index of panel which displays number of selected nodes
  FOLDERICON = 0;  //index of folder icon in IconList


implementation

uses dgStrConst, selectrestore, archive, archiveinfo, About, oplog, isolate, diff, prefs,
   selectfilter;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  PrefFileName: string;
  RecentFile: string;
  x: integer;
begin
  //set default colors for Treeview levels
  //at some point it will be possible for the user to change these
  LevelColors[0] := clBlack;
  LevelColors[1] := clBlack;
  LevelColors[2] := clBlack;
  LevelColors[3] := clBlack;
  LevelColors[4] := clBlack;
  InitialiseInterface;
  AllowRecursiveSelect := true;
  UserHome := SysUtils.GetEnvironmentVariable('HOME');
  if FileExists(UserHome + '/.config')
     then SettingsDir := UserHome + '/.config/dargui/'
     else SettingsDir := UserHome + '/.dargui/';
  if not FileExists(SettingsDir)
     then mkdir(SettingsDir);
  PrefFileName := SettingsDir + 'darguirc';
  RecentList := TRecentFiles.Create(Self);
  RecentList.OnClick := @RecentMenuClick;
  miOpenRecent.Add(RecentList);
  Preferences := TSettingsFile.Create(PrefFileName);
  RecentList.IniFile := Preferences;
  x := 0;
  RecentFile := Preferences.ReadString ( rsCfgRecentFiles, rsCfgRecentX + IntToStr(x) , '' ) ;
  While RecentFile <> '' do
        begin
          RecentList.AddFile(RecentFile, false);
          Inc(x);
          RecentFile := Preferences.ReadString(rsCfgRecentFiles,rsCfgRecentX + IntToStr(x),'');
        end;
  miShowToolbar.Checked := Preferences.ReadString ( rsCfgUserPrefs, rsCfgShowToolbar, '1' ) = '1';
  ToolbarPanel.Visible := miShowToolbar.Checked;
  Caption := Caption + #32 + APP_VERSION;
  case CheckSupportingApps of
       1: ShowMessage ( rsErrNoXterm ) ;
       2: ShowMessage ( rsErrNoBash ) ;
       3: ShowMessage ( rsErrNoXtermBash ) ;
       end;
  DarInfo := GetDarVersion;
  if DarInfo.version='-' then
     begin
     MessageDlg ( rsErrDarNotFound,
                        mtWarning,
                        [mbOk],
                        0);
     end;

  if not FileExists(TEMP_DIRECTORY)
     then mkdir(TEMP_DIRECTORY);

  GetTerminalCommand(TerminalCommand);
  RunscriptPath := GetRunscriptPath;

  UpdatingSelection := false;
  miHideMessages.Checked := true;

  writeln(DarInfo.version);

  OpenDialog.InitialDir := SysUtils.GetEnvironmentVariable('HOME');

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
    BatchFile.Add ( rsNotCompressThese ) ;
    for x := 0 to ArchiveForm.NoCompressList.Count-1 do
        BatchFile.Add('-Z ' + ArchiveForm.NoCompressList.Items[x]);
    end;
  if ArchiveForm.CompLwrLimitCombo.ItemIndex > -1
     then B := ArchiveForm.CompLwrLimitCombo.Items[ArchiveForm.CompLwrLimitCombo.ItemIndex][1];
  if not ((ArchiveForm.CompressionLwrLimit.Text = '100') and (B = 'b')) then
     begin
       BatchFile.Add(#10 + rsNotCompressSmaller);
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
       //ShowMessage('Error: Base Directory not in ' + aFilePath);
       Result := aFilePath;
       end;
  end;
  
begin
  ArchiveForm.BatchFile.Text := GetNextFileName(TEMP_DIRECTORY + BATCHFILE_BASE);
  if ArchiveForm.ShowModal = mrOk then
     try
     Enabled := false;
     DarOptions := ' -X "' + ArchiveForm.ArchiveName.Text + '.*.dar"';
     BatchFile := TStringList.Create;
     BatchFile.Add ( rsDARBatchFile ) ;
     BatchFile.Add('-R "' + ArchiveForm.BaseDirectory.Text + '"' + #10);
     if ArchiveForm.DryRunCheck.Checked then
        begin
          BatchFile.Add ( rsDryRun ) ;
          BatchFile.Add('--empty' + #10);
        end;
     if ArchiveForm.IncludeDirectories.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add ( rsIncDirectories ) ;
        for x := 0 to ArchiveForm.IncludeDirectories.Count-1 do
            BatchFile.Add('-g "' + RemoveBaseDirectory(ArchiveForm.IncludeDirectories.Items[x]) + '"');
        end;
     if ArchiveForm.IncludeFiles.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add ( rsIncFiles ) ;
        for x := 0 to ArchiveForm.IncludeFiles.Count-1 do
            BatchFile.Add('-I "' + RemoveBaseDirectory(ArchiveForm.IncludeFiles.Items[x]) + '"');
        end;
     if ArchiveForm.ExcludeDirectories.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add ( rsExclDirectories ) ;
        for x := 0 to ArchiveForm.ExcludeDirectories.Count-1 do
            BatchFile.Add('-P "' + RemoveBaseDirectory(ArchiveForm.ExcludeDirectories.Items[x]) + '"');
        end;
     if ArchiveForm.ExcludeFiles.Count > 0 then
        begin
        BatchFile.Add('');
        BatchFile.Add ( rsExclFiles ) ;
        for x := 0 to ArchiveForm.ExcludeFiles.Count-1 do
            BatchFile.Add('-X "' + RemoveBaseDirectory(ArchiveForm.ExcludeFiles.Items[x]) + '"');
        end;
     if ArchiveForm.GZipCheck.Checked
        then begin
             BatchFile.Add('');
             BatchFile.Add ( rsUseGzipCompr ) ;
             BatchFile.Add('--gzip=' + ArchiveForm.CompressionLevel.Text);
             AddCompressionOptions;
             end
     else if ArchiveForm.Bzip2Check.Checked
        then begin
             BatchFile.Add('');
             BatchFile.Add ( rsUseBzip2Comp ) ;
             BatchFile.Add('--bzip2=' + ArchiveForm.CompressionLevel.Text);
             AddCompressionOptions;
             end;
     if not ArchiveForm.ReadConfigCheck.Checked then
        begin
          BatchFile.Add ( rsNotReadDARcfg + #10 + '-N' + #10 ) ;
        end;
     if ArchiveForm.EmptyDirCheck.Checked then
        begin
          BatchFile.Add ( rsPreserveDirs + #10 + '-D' + #10 ) ;
        end;
     if ArchiveForm.SlicesCheck.Checked then
        begin
          try
            StrToInt(ArchiveForm.SliceSize.Text);
          except
            ArchiveForm.SliceSize.Text := '650';
          end;
          BatchFile.Add ( rsCreateSlices  + #10 + '--slice ' +
            ArchiveForm.SliceSize.Text + 'M' + #10 ) ;
          if ArchiveForm.PauseCheck.Checked then
             BatchFile.Add ( rsPauseBetween + #10 + '--pause ' + #10 ) ;
        end;


     Command := DAR_EXECUTABLE + ' -c "' + ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveName.Text + '"'
                            + ' -B "' + ArchiveForm.BatchFile.Text + '"'
                            + ' -v'
                            //+ ' -e' // for debugging
                            + DarOptions
                            {+ ' -Q'};
                            
     BatchFile.Insert(1, '# ' + Command);
     BatchFile.SaveToFile(ArchiveForm.BatchFile.Text);

     MessageMemo.Lines.Add(#32 + StringOfChar('-',45));
     MessageMemo.Lines.Add('Creating archive: ' + ArchiveForm.ArchiveName.Text);

     if RunDarCommand ( Command, rsCptCreatingArchive, Left + 100, Top + 150 )
       = 0 then
        begin
          CurrentArchive := ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveName.Text;
          OpenArchive(CurrentArchive, ArchiveTreeView);
          EnableArchiveMenus;
          OpenDialog.FileName := CurrentArchive;
          //OpLogForm.AddCommand(Command);
          OpLogForm.RefreshOpList;
        end;
     if ArchiveForm.SaveScriptCheckBox.Checked
        then if ArchiveForm.ScriptFilenameBox.Text <> ''
             then WriteArchiveScript(ArchiveForm.ScriptFilenameBox.Text);
     finally
     BatchFile.Free;
     Enabled := true;
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
  for x := 1 to TTreeview(Sender).Items.Count-1 do
      begin
      if TTreeview(Sender).Items[x].MultiSelected then
         begin
         Inc(SelectedNodes);
         end;
      if AllowRecursiveSelect then
          if TTreeview(Sender).Items[x].Parent <> nil then
             begin
               if TTreeview(Sender).Items[x].Parent.MultiSelected then
                   // only select childnodes if Parent is collapsed
                   if (not TTreeview(Sender).Items[x].Parent.Expanded)
                   // or hidden from view
                     or (not TTreeView(Sender).Items[x].Parent.IsVisible) then
                     // don't count it again if already Multiselected
                        if not TTreeView(Sender).Items[x].MultiSelected then
                          begin
                            UpdatingSelection := true;
                            TTreeview(Sender).Items[x].MultiSelected := true;
                            Inc(SelectedNodes);
                            UpdatingSelection := false;
                          end;
             end;
        StatusBar.Panels[SELECT_STATUSBAR].Text :=
              Format ( rsStatusNodeSelect, [ IntToStr ( SelectedNodes ) ] ) ;
      end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Preferences.Free;
end;

procedure TMainForm.RecentMenuClick ( Sender: TObject ) ;
var
  fn: string;
begin
  fn := TMenuItem(Sender).Caption + '.1.dar';
  if FileExists(fn) then
       if DarInfo.version<>'-' then  //TODO disable open menus if dar is absent
          begin
            StatusBar.Panels[SELECT_STATUSBAR].Text := rsMessBUSY;
            Application.ProcessMessages;
            OpenDialog.FileName := fn;
            if OpenArchive(OpenDialog.FileName,ArchiveTreeView) = 0
                    then EnableArchiveMenus;
            StatusBar.Panels[SELECT_STATUSBAR].Text := '';
          end
          else MessageDlg ( rsErrUnableToFindArchive, mtError, [ mbOk ] , 0 ) ;
end;

procedure TMainForm.ToolbarPanelClick ( Sender: TObject ) ;
begin

end;

procedure TMainForm.miArchiveInformationClick(Sender: TObject);
begin
  if GetArchiveInformation(ExtractFilePath(OpenDialog.FileName)
                            + ArchiveTreeView.TopItem.Text,InformationForm.InformationMemo) = 0
      then
      begin
        InformationForm.Caption := Format ( rsInformationF, [
          ArchiveTreeView.TopItem.Text ] ) ;
        InformationForm.InformationMemo.Lines.Insert ( 0, Format (
          rsInfoFormLocation, [ OpenDialog.FileName ] ) ) ;
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
  FolderGraphic: TBitmap;
 
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
  if Node.Data <> nil then
     begin
      if TFileData(Node.Data).folder then
         begin
           FolderGraphic := TBitmap.Create;
           IconList.GetBitmap(FOLDERICON, FolderGraphic);
           Sender.Canvas.Draw(Displayrect.Left, DisplayRect.Top, FolderGraphic);
           Sender.Canvas.TextOut(Displayrect.Left + FolderGraphic.Width, DisplayRect.Top+1,
                TrimText(TFileData(Node.Data).item[SEGFILENAME], SEGFILENAME, Displayrect.Left + FolderGraphic.Width));
         end
         else Sender.Canvas.TextOut(Displayrect.Left,DisplayRect.Top+1,
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
      if TFileData(Node.Data).folder then FolderGraphic.Free;
     end;
end;


{ Adapted from treeview.inc [ Lazarus Component Library ] }
procedure TMainForm.ArchiveTreeViewCompare ( Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer ) ;
  
 {Inline function returns true if Node is a File}
  function IsAFile(  Node : TTreeNode  ) : boolean;
  begin
     Result := (Pos('-----', TFileData(Node.Data).item[SEGSTATUS]) < 1);
  end;

     {Inline function returns true if Node is a Folder}
  function IsAFolder(  Node : TTreeNode  ) : boolean;
  begin
     Result := (Pos('-----', TFileData(Node.Data).item[SEGSTATUS]) > 0);
  end;


begin
     {Files before folders}
  if(   IsAFile(  Node1  ) and IsAFolder(  Node2  )   ) then
  begin
     Compare := 1;
     Exit;
  end;

     {Folder after file}
  if(   IsAFolder(  Node1  ) and IsAFile(  Node2  )   ) then
  begin
     Compare := -1;
     Exit;
  end;

     {Nodes are of the same type, so do a normal alpha sort}
  Compare := AnsiCompareStr(Node1.Text,Node2.Text);
end;

procedure TMainForm.miFileOpenClick(Sender: TObject);
var
  fn: String;
begin
  OpenDialog.Title := rsOpenExisting;
  OpenDialog.Filter := rsFilterDARArchives + '|*.*.dar';
  if OpenDialog.Execute then
     begin
       StatusBar.Panels[SELECT_STATUSBAR].Text := rsMessBUSY;
       Application.ProcessMessages;
       fn := OpenDialog.FileName;
       if OpenArchive(fn, ArchiveTreeView) = 0 then
        begin
          EnableArchiveMenus;
          CurrentArchive := fn;
          RecentList.AddFile(fn);
        end
        else MessageDlg ( rsErrUnableToOpenArchive, mtError, [ mbOK ] , 0 ) ;
      StatusBar.Panels[SELECT_STATUSBAR].Text := '';
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
     then AboutForm.DarVersionLabel.Caption := rsAboutNoDAR
     else AboutForm.DarVersionLabel.Caption := Format ( rsAboutDARVersion, [
       DarInfo.version ] ) ;
  Aboutform.VersionLabel.Caption := Aboutform.VersionLabel.Caption + APP_VERSION;
  Aboutform.SVNLabel.Caption := Aboutform.SVNLabel.Caption + SVN_REVISION;
  AboutForm.ShowModal;
  Aboutform.Free;
end;

procedure TMainForm.miHelpDarClick ( Sender: TObject ) ;
var
  Browser: string;
  brParams: string;
  Proc: TProcess;
begin
  GetDefaultBrowser(Browser, brParams);
  if Browser <> '' then
     begin
     Proc := TProcess.Create(Application);
     Proc.CommandLine := Browser + #32 + DAR_DOCPAGE;
     Proc.Execute;
     end;
end;

procedure TMainForm.miHideMessagesClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if Sender = miHideMessages
     then MessagePanel.Visible := not miHideMessages.Checked;
  MessageBoxSplitter.Visible := MessagePanel.Visible;
end;

procedure TMainForm.miIsolateClick ( Sender: TObject ) ;
var
  IsolateForm: TIsolateForm;
  Cmd: String;
begin
  IsolateForm := TIsolateForm.Create(Self);
  try
    if CurrentArchive <> ''
       then
       begin
         IsolateForm.ArchiveBox.Text := CurrentArchive;
         IsolateForm.CatalogueBox.Text :=  CurrentArchive + '_cat';
       end;
    if IsolateForm.ShowModal = mrOK then
       begin
         Cmd := DAR_EXECUTABLE + ' -C ' + IsolateForm.CatalogueBox.Text
                       + ' -A ' + IsolateForm.ArchiveBox.Text + ' -v';
         RunDarCommand ( Cmd, rsCptIsolating, Left + 100, Top + 150 ) ;
         //OpLogForm.AddCommand(Cmd);
       end;
  finally
    IsolateForm.Free;
  end;
end;

procedure TMainForm.miOperationlogsClick ( Sender: TObject ) ;
begin
  OpLogForm.ShowOnTop;
end;


procedure TMainForm.miRestoreAllClick(Sender: TObject);
var
  RestoreForm: TExtractSelectedForm;
  daroptions: String;
  CommandLine: String;
begin
  SelectedNodes := 0;
  RestoreForm := TExtractSelectedForm.Create(Self);
  RestoreForm.FullRestore := true;
  RestoreForm.Caption := Format ( rsCptRestoreNodes, [ IntToStr (
    ArchiveTreeView.Items.Count - 1 ) ] ) ;
  if RestoreForm.ShowModal = mrOK then
     begin
        daroptions := ' -O -v';
        if RestoreForm.FlatRestoreCheckBox.Checked then daroptions := daroptions + ' -f';
        case RestoreForm.OverwriteOptions.ItemIndex of
             0: daroptions := daroptions + ' --no-warn';
             1: ;
             2: daroptions := daroptions + ' --no-overwrite';
             end;
        CommandLine := (DAR_EXECUTABLE + ' -R "' + RestoreForm.RestoreDirectoryEdit.Text + '" -x "' + CurrentArchive + '" ' + daroptions);
        RunDarCommand ( CommandLine, rsCptRestoringFiles, Left + 100, Top + 150
          ) ;
        OpLogForm.AddCommand(CommandLine);
        end
        else MessageMemo.Lines.Add ( rsErrRestoreAborted ) ;
  RestoreForm.Free;
end;

procedure TMainForm.pmiHideAllClick ( Sender: TObject ) ;
begin
  ArchiveTreeView.Items[0].Collapse(true);
end;

procedure TMainForm.pmiShowAllClick ( Sender: TObject ) ;
begin
  ArchiveTreeView.Items[0].Expand(true);
end;

procedure TMainForm.pmiShowAllSelectedClick ( Sender: TObject ) ;
var
  x: Integer;
begin
  for x := 1 to ArchiveTreeView.Items.Count-1 do
      if ArchiveTreeView.Items[x].MultiSelected
         then ArchiveTreeView.Items[x].ExpandParents;
end;

procedure TMainForm.miShowToolbarClick ( Sender: TObject ) ;
begin
  miShowToolbar.Checked := not miShowToolbar.Checked;
  ToolbarPanel.Visible := miShowToolbar.Checked;
  if miShowToolbar.Checked
     then Preferences.WriteString(rsCfgUserPrefs,rsCfgShowToolbar,'1')
     else Preferences.WriteString(rsCfgUserPrefs,rsCfgShowToolbar,'0');
end;

procedure TMainForm.pmiSelectFilterClick ( Sender: TObject ) ;
var
  SelectFilterForm: TSelectFilterForm;
begin
  SelectFilterForm := TSelectFilterForm.Create(Self);
  SelectFilterForm.FileView := ArchiveTreeView;
  SelectFilterForm.Caption := rsFCptSelectbyFilter;
  if SelectFilterForm.ShowModal = mrOK then
     begin
       StatusBar.Panels [ SELECT_STATUSBAR ] .Text := rsMessBUSY;
       Application.ProcessMessages;
       AllowRecursiveSelect := false;
       UpdatingSelection := true;
       SelectFilterForm.ApplyFilters;
       UpdatingSelection := false;
       ArchiveTreeViewSelectionChanged(ArchiveTreeView);
       AllowRecursiveSelect := true;
     end;
  SelectFilterForm.Free;
end;

procedure TMainForm.pmiToggleSelectClick ( Sender: TObject ) ;
var
  x: integer;
begin
  ArchiveTreeView.Items[0].MultiSelected := false;
  AllowRecursiveSelect := false;
  UpdatingSelection := true;
  for x := 1 to ArchiveTreeView.Items.Count-1 do
     ArchiveTreeView.Items[x].MultiSelected := not ArchiveTreeView.Items[x].MultiSelected;
  if ArchiveTreeView.Selected <> nil then
  ArchiveTreeView.Selected.Selected := false;
  UpdatingSelection := false;
  ArchiveTreeViewSelectionChanged(ArchiveTreeView);
  AllowRecursiveSelect := true;
end;

procedure TMainForm.tbDiffClick ( Sender: TObject ) ;
var
  DiffForm: TDiffForm;
begin
  DiffForm := TDiffForm.Create(Self);
  try
    if CurrentArchive <> ''
       then DiffForm.ArchiveBox.Text := CurrentArchive;
    DiffForm.BaseDirBox.Text := SysUtils.GetEnvironmentVariable('HOME');
    DiffForm.ShowModal;
    OpLogForm.RefreshOpList;
  finally
    DiffForm.Free;
  end;
end;


procedure TMainForm.pmiRestoreSelectedClick(Sender: TObject);
var
  x: integer;
  fd: TFileData;
  fn: string;
  batchfile: string;
  batch : TStringList;
  RestoreForm: TExtractSelectedForm;
  daroptions: string;
  CommandLine: String;

begin
  SelectedNodes := 0;
  MessageMemo.Clear;
  batch := TStringList.Create;
  RestoreForm := TExtractSelectedForm.Create(Self);
  RestoreForm.FullRestore := false;
  for x := 0 to ArchiveTreeView.Items.Count-1 do    {make list of selected files}
      if ArchiveTreeView.Items[x].MultiSelected then
         begin
         fd := TFileData(ArchiveTreeView.Items[x].Data);
         fn := fd.item[SEGFILEPATH] + fd.item[SEGFILENAME];
         RestoreForm.SelectedFiles.Add(fn);
         Inc(SelectedNodes);
         end;
  RestoreForm.Caption := rsCptRestoreSelected;
  fd := TFileData(ArchiveTreeView.Selected.Data);
  if SelectedNodes > 0 then
       begin
       if RestoreForm.ShowModal = mrOK then
         if RestoreForm.SelectedFiles.Count > 0 then
            begin
              Enabled := false;
              Application.ProcessMessages;
              daroptions := ' -O -v';
              if RestoreForm.FlatRestoreCheckBox.Checked
                 then daroptions := daroptions + ' -f';
              case RestoreForm.OverwriteOptions.ItemIndex of
                   0: daroptions := daroptions + ' --no-warn';
                   1: ;
                   2: daroptions := daroptions + ' --no-overwrite';
                   end;
              Batch.Add(#10 + rsFilesToRestore);
              for x := 0 to RestoreForm.SelectedFiles.Count-1 do
                  Batch.Add('-g "' + RestoreForm.SelectedFiles.Strings[x] + '"');
              batch.Insert(0,'-R "' + RestoreForm.RestoreDirectoryEdit.Text + '"');
              batchfile := GetNextFileName(TEMP_DIRECTORY + BATCHFILE_BASE);
              batch.SaveToFile(batchfile);
              CommandLine := (DAR_EXECUTABLE + ' -x "' + CurrentArchive + '" -B "' + batchfile + '" ' + daroptions);
              RunDarCommand(CommandLine, rsCptRestoringFiles, Left+100, Top+150);
              //OpLogForm.AddCommand(CommandLine);
              OpLogForm.RefreshOpList;
              //TODO: check that some files do need to be restored: ie not all excluded by overwrite rule
            end
            else MessageMemo.Lines.Add(rsErrRestoreAborted );
       end
       else ShowMessage ( rsErrNoFilesSelected ) ;
  RestoreForm.Free;
  batch.Free;
  Enabled := True;
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

procedure TMainForm.InitialiseInterface;
begin
  pmiSelectFilter.Caption := rsMenuSelectByFilter;
  miToggleSelect.Caption := rsMenuToggleSelect;
  pmiToggleSelect.Caption := rsMenuToggleSelect;
  miShowToolbar.Caption := rsMenuShowToolbar;
  miOpenRecent.Caption := rsMenuOpenRecent;
  miHelpDar.Caption := rsMenuDarDocumentation;
  tbDiff.Hint := rsHintCompareArchiveFile;
  tbOpen.Hint := rsHintOpenAnArchive;
  tbCreate.Hint := rsHintCreateANewArchive;
  tbIsolate.Hint := rsHintIsolateCatalogue;
  MenuHelp.Caption := rsMenuHelp;
  miIsolate.Caption := rsMenuIsolateCatalogue;
  miOperationlogs.Caption := rsMenuOperationLogs;
  miHelpAbout.Caption := rsMenuHelp;
  miArchiveInformation.Caption := rsMenuInformation;
  //miHideMessages: TMenuItem;
  MenuSettings.Caption := rsMenuOptions;
  //MessageHideButton: TBitBtn;
  //MessageLabel: TLabel;
  MenuFile.Caption := rsMenuFile;
  MenuArchive.Caption := rsMenuArchive;
  miRestoreSelected.Caption := rsMenuRestoreSelected;
  miRestoreAll.Caption := rsMenuRestoreAll;
  pmiRestoreSelected.Caption := rsMenuRestoreSelected;
  pmiRestoreAll.Caption := rsMenuRestoreAll;
  pmiShowAllSelected.Caption := rsMenuShowSelected;
  pmiShowAll.Caption := rsMenuShowAll;
  pmiHideAll.Caption := rsMenuHideAll;
  miExit.Caption := rsMenuExit;
  miFileNew.Caption := rsMenuNew;
  miFileOpen.Caption := rsMenuOpen;
  FilenameHdr.Caption := rsColFileName;
  DateHdr.Caption := rsColDate;
  SizeHdr.Caption := rsColSize;
  UserHdr.Caption := rsColUser;
  GroupHdr.Caption := rsColGroup;
  StatusHdr.Caption := rsColStatus;
end;


initialization
  {$I main.lrs}

end.

