unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DarIntf,
  Menus, ComCtrls, ExtCtrls, Buttons, StdCtrls, Process, LCLType, historymenu,
  selectfilter;

type

  { TMainForm }

  TMainForm = class(TForm)
    ArchiveTreeView: TTreeView;
    FileHeaderBar: THeaderControl;
    miHelpContents: TMenuItem;
    miArchiveDiff: TMenuItem;
    MenuScheduling: TMenuItem;
    miSchedManager: TMenuItem;
    miSelectFilter: TMenuItem;
    miShowSelect: TMenuItem;
    miTestArchive: TMenuItem;
    FileViewScrollBox: TScrollBox;
    tbSchedMan: TBitBtn;
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
    tbTest: TBitBtn;
    ToolbarPanel: TPanel;
    TreeViewMenu: TPopupMenu;
    StatusBar: TStatusBar;
    procedure ArchiveTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ArchiveTreeViewCompare ( Sender: TObject; Node1,
      Node2: TTreeNode; var Compare: Integer ) ;
    procedure ArchiveTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure ArchiveTreeViewSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure HeaderBarSectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure RecentMenuClick ( Sender: TObject ) ;
    procedure miArchiveInformationClick(Sender: TObject);
    procedure miExitClick ( Sender: TObject ) ;
    procedure MessageHideButtonClick(Sender: TObject);
    procedure miFileNewClick ( Sender: TObject ) ;
    procedure miFileOpenClick(Sender: TObject);
    procedure miHelpAboutClick ( Sender: TObject ) ;
    procedure miHelpDarClick ( Sender: TObject ) ;
    procedure miHideMessagesClick(Sender: TObject);
    procedure miIsolateClick ( Sender: TObject ) ;
    procedure miOperationlogsClick ( Sender: TObject ) ;
    procedure miRestoreAllClick(Sender: TObject);
    procedure miSchedManagerClick ( Sender: TObject ) ;
    procedure pmiHideAllClick ( Sender: TObject ) ;
    procedure pmiShowAllClick ( Sender: TObject ) ;
    procedure pmiShowAllSelectedClick ( Sender: TObject ) ;
    procedure miShowToolbarClick ( Sender: TObject ) ;
    procedure pmiSelectFilterClick ( Sender: TObject ) ;
    procedure pmiToggleSelectClick ( Sender: TObject ) ;
    procedure tbDiffClick ( Sender: TObject ) ;
    procedure pmiRestoreSelectedClick(Sender: TObject);
    procedure EnableArchiveMenus( flag: Boolean );
    procedure InitialiseInterface;
    procedure tbTestClick ( Sender: TObject ) ;
  private
    LevelColors: array[0..4] of TColor;
    CurrentPass: string;
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;
  SelectFilterForm: TSelectFilterForm;
  CurrentArchive: string;
  DarInfo: TDarInfo;
  SelectedNodes: integer;
  UpdatingSelection: Boolean;
  AllowRecursiveSelect: Boolean;
  UserHome: string;
  SettingsDir: string;
  AtScriptDir: string;
  CronScriptDir: string;
  RecentList : TRecentFiles;

  HasATD: Boolean;
  HasCron: Boolean;
  

  
const
  APP_VERSION = '0.5.0';
  //{$I revision.inc}
  {revision.inc is a dynamically produced file containing the number of the most recent SVN revision
   the include directive can be commented out and the following line uncommented, replacing the number 0 with the
   appropriate revision number}
  SVN_REVISION = '261';

  ARCHIVEMENU_TAG = 1; //used for enabling menuitems after loading archive
  SELECT_STATUSBAR = 0;   //index of panel which displays number of selected nodes
  FOLDERICON = 0;  //index of folder icon in IconList
  
  DARGUI_INFO_FILE = '.dargui-info-file';


implementation

uses baseunix, dgStrConst, selectrestore, archive, archiveinfo, About, oplog, isolate, diff, prefs, schedman, password;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  PrefFileName: string;
  RecentFile: string;
  x: integer;
  test: string;
begin
  //set default colors for Treeview levels
  //at some point it will be possible for the user to change these
  CurrentPass := '';
  test := '';
  LevelColors[0] := clBlack;
  LevelColors[1] := clBlack;
  LevelColors[2] := clBlack;
  LevelColors[3] := clBlack;
  LevelColors[4] := clBlack;
  InitialiseInterface;
  SelectFilterForm := TSelectFilterForm.Create(Self);
  SelectFilterForm.FileView := ArchiveTreeView;
  SelectFilterForm.Caption := rsFCptSelectbyFilter;
  AllowRecursiveSelect := true;
  UserHome := SysUtils.GetEnvironmentVariable('HOME');
  if FileExists(UserHome + '/.config')
     then SettingsDir := UserHome + '/.config/dargui/'
     else SettingsDir := UserHome + '/.dargui/';
  if not FileExists(SettingsDir)
     then mkdir(SettingsDir);
  AtScriptDir := SettingsDir + 'atscripts/';
  if not FileExists(AtScriptDir)
     then mkdir(AtScriptDir);
  CronScriptDir := SettingsDir + 'cronscripts/';
  if not FileExists(CronScriptDir)
     then mkdir(CronScriptDir);
  PrefFileName := SettingsDir + 'darguirc';
  RecentList := TRecentFiles.Create(Self);
  RecentList.OnClick := @RecentMenuClick;
  miOpenRecent.Add(RecentList);
  Preferences := TSettingsFile.Create(PrefFileName);
  RecentList.IniFile := Preferences;
  x := 0;
  RecentFile := Preferences.ReadString ( CfgRecentFiles, CfgRecentX + IntToStr(x) , '' ) ;
  While RecentFile <> '' do
        begin
          RecentList.AddFile(RecentFile, false);
          Inc(x);
          RecentFile := Preferences.ReadString(CfgRecentFiles,CfgRecentX + IntToStr(x),'');
        end;
  miShowToolbar.Checked := Preferences.ReadString ( CfgUserPrefs, CfgShowToolbar, '1' ) = '1';
  ToolbarPanel.Visible := miShowToolbar.Checked;
  Caption := Caption + #32 + APP_VERSION;
  case CheckSupportingApps of
       1: ShowMessage ( rsErrNoXterm ) ;
       2: ShowMessage ( rsErrNoBash ) ;
       3: ShowMessage ( rsErrNoXtermBash ) ;
       end;
  HasATD := (ProcessRunning('atd'))
     and (ShellCommand('atq -V', test) = 0);
  HasCron :=  ( ( (ProcessRunning('cron')  // Ubuntu
              or (ProcessRunning('crond') ) ) ) // CentOS
     and (ShellCommand('crontab -l', test) = 0) ) ;
  miSchedManager.Enabled := HasATD or HasCron;
  tbSchedMan.Enabled := HasATD or HasCron;
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
               then if OpenArchive(OpenDialog.FileName,ArchiveTreeView, '') = 0  //need to check for encryption
                    then EnableArchiveMenus(true);
          end;
end;

procedure TMainForm.miFileNewClick ( Sender: TObject ) ;
var
  DarOptions: string;
  Command: string;
  referencearchive: String;
  
  procedure CreateNewArchive;
  var
    InfoFile: TSettingsFile;
    infofilename: String;
  begin
     try
     Enabled := false;
     ArchiveForm.CreateBatchfile;
     referencearchive := '';
     if ArchiveForm.DiffFileCheck.Checked
        then referencearchive := ' -A ' + TrimToBase( ArchiveForm.DiffReference.Text );
     DarOptions := ' -X ' + ArchiveForm.ArchiveBaseName + '.*.dar';
     Command := DAR_EXECUTABLE + ' -c "' + ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveBaseName + '"'
                            + referencearchive
                            + ' -B "' + ArchiveForm.BatchFileBox.Text + '"'
                            + ' -v'
                            + DarOptions;
     if ArchiveForm.EncryptArchiveCheck.Checked
        then Command := Command + ' -K :';
     ArchiveForm.BatchFile.Insert(1, '# ' + Command);
     if UseInfoFile then
        begin
          infofilename := ArchiveForm.BaseDirectory.Text + DirectorySeparator + DARGUI_INFO_FILE;
          InfoFile := TSettingsFile.Create(infofilename);
          InfoFile.WriteString('Archive information', 'Basedirectory', ArchiveForm.BaseDirectory.Text );
          InfoFile.Free;
          ArchiveForm.BatchFile.Insert(2, '-I ' + DARGUI_INFO_FILE);
        end;
     ArchiveForm.BatchFile.SaveToFile(ArchiveForm.BatchFileBox.Text);

     MessageMemo.Lines.Add(#32 + StringOfChar('-',45));
     MessageMemo.Lines.Add('Creating archive: ' + ArchiveForm.ArchiveName.Text);
     writeln('Creating archive: ' + ArchiveForm.ArchiveName.Text);

     if (RunDarCommand ( Command, rsCptCreatingArchive, Left + 100, Top + 150 )
       = 0 ) and (ArchiveForm.OpenCreatedArchiveCheck.Checked) then
        begin
          CurrentArchive := ArchiveForm.ArchiveDirectory.Text
                            + ArchiveForm.ArchiveBaseName;
          if ValidateArchive(CurrentArchive, CurrentPass) then
             begin
                if OpenArchive(CurrentArchive, ArchiveTreeView, CurrentPass) = 0 then
                   begin
                     EnableArchiveMenus(true);
                     OpenDialog.FileName := CurrentArchive;
                     RecentList.AddFile(CurrentArchive);
                   end
                else
                   begin
                     EnableArchiveMenus(false);
                     CurrentArchive := '';
                   end;
             end;
          OpLogForm.RefreshOpList;
        end;
      if ArchiveForm.SaveScriptCheckBox.Checked
        then if ArchiveForm.ScriptFilenameBox.Text <> ''
             then WriteArchiveScript(ArchiveForm.ScriptFilenameBox.Text);
     finally
     Enabled := true;
     if FileExists(infofilename)
        then DeleteFile(infofilename);
     end;
  end;
  
  function WriteScript( scriptfilename: string ): Boolean;
  var
    Scriptfile: TStringlist;
    c: Integer;
    archivename: String;
  begin
     Result := false;
     Scriptfile := TStringList.Create;
     try
     Enabled := false;
     ArchiveForm.CreateBatchfile;
     archivename := ArchiveForm.ArchiveBaseName;
     if ( ArchiveForm.RepeatRadioButton.Checked ) or ( ArchiveForm.TimestampCheck.Checked )
        then archivename := archivename + '`date +_%Y%m%d%H%M`';  // All cron jobs are timestamped to avoid duplicate filenames
     referencearchive := '';
     if ArchiveForm.DiffFileCheck.Checked
        then referencearchive := ' -A "' + TrimToBase( ArchiveForm.DiffReference.Text ) + '" ';
     DarOptions := ' -X ' + ArchiveForm.ArchiveBaseName + '.*.dar';
     Command := DAR_EXECUTABLE + ' -c "' + ArchiveForm.ArchiveDirectory.Text
                            + archivename + '"'
                            + referencearchive
                            + ' -B "/tmp/dargui.temp"'
                            + ' -v'
                            + DarOptions;
     if ArchiveForm.EncryptArchiveCheck.Checked
        then
        if PasswordDlg.Execute('') = mrOk
           then Command := Command + ' -K :' + PasswordDlg.Password
        else ShowMessage('Archive will not be encrypted');
//     if UseInfoFile then
//        begin
//          infofilename := ArchiveForm.BaseDirectory.Text + DirectorySeparator + DARGUI_INFO_FILE;
//          InfoFile := TSettingsFile.Create(infofilename);
//          InfoFile.WriteString('Archive information', 'Basedirectory', ArchiveForm.BaseDirectory.Text );
//          InfoFile.Free;
//          ArchiveForm.BatchFile.Insert(2, '-I ' + DARGUI_INFO_FILE);
//        end;
//     ArchiveForm.BatchFile.SaveToFile(ArchiveForm.BatchFileBox.Text);
      Scriptfile.Add('#!/bin/bash');
      Scriptfile.Add('touch /tmp/dargui.temp');
      for c := 0 to ArchiveForm.BatchFile.Count-1 do
          Scriptfile.Add('echo ''' + ArchiveForm.BatchFile[c] + ''' >> /tmp/dargui.temp');
      Scriptfile.Add('echo ------------- >> ' + SettingsDir + 'dargui.log');
      Scriptfile.Add('echo `date` ' + Command + ' >> ' + SettingsDir + 'dargui.log');
      Scriptfile.Add(Command + ' -Q >> ' + SettingsDir + 'dargui.log');
      Scriptfile.Add('rm /tmp/dargui.temp');
      Scriptfile.SaveToFile(scriptfilename);
      FpChmod(scriptfilename, S_IRWXU);
      Result := true;
     finally
       Scriptfile.Free;
     end;
  end;
  
  procedure CreateScript;
  var
    Proc: TProcess;
  begin
      if ArchiveForm.ScriptFilenameBox.Text <> ''
           then if WriteScript(ArchiveForm.ScriptFilenameBox.Text)
              then
              begin
                if MessageDlg(Format(rsQueryExecuteScript, [#10,
                  ArchiveForm.ScriptFilenameBox.Text, #10#10]), mtInformation, [
                  mbYes, mbNo], 0) = mrYes
                 then  begin
                         Proc := TProcess.Create(Application);
                         Proc.CommandLine := TerminalCommand + ' -e '  + RunscriptPath + 'runscript.sh "' + ArchiveForm.ScriptFilenameBox.Text + '"';
                         Proc.Execute;
                         Proc.Free;
                       end;
               end
                else
                ShowMessage(rsErrWritingScript);
  end;
  
  Procedure CreateAtScript;
  var
    script: string;
    atcommand: string;
    atresponse:string;
    p: LongInt;
    q: Integer;
  begin
    atresponse := '';
    script := AtScriptDir + ArchiveForm.ArchiveBaseName + '.sh';
    if WriteScript(script) then
       begin
         if Length(ArchiveForm.RunOnceMinuteBox.Text) < 2 then
            ArchiveForm.RunOnceMinuteBox.Text := '0' + ArchiveForm.RunOnceMinuteBox.Text;
         atcommand := 'at -f '
                      + script + #32
                      + ArchiveForm.RunOnceHourBox.Text + ':' + ArchiveForm.RunOnceMinuteBox.Text + #32
                      + FormatDateTime('mmddyy',ArchiveForm.RunOnceDateEdit.Date);
         if ShellCommand(atcommand, atresponse)=0 then
            begin
              p := Pos('job ', atresponse);
              if p > 0 then
                 begin
                   q := p+5;
                   while atresponse[q]<>#32 do
                         inc(q);
                   ShellCommand('mv ' + script + #32 + script + '.' + Copy(atresponse, p+4, q-p-4), atcommand);
                   ShowMessage(Format(rsScriptExecuteTime, [
                     ArchiveForm.RunOnceHourBox.Text,
                     ArchiveForm.RunOnceMinuteBox.Text, DateToStr(
                     ArchiveForm.RunOnceDateEdit.Date)]));
                 end
              else
                 ShowMessage(Format(rsErrScriptNotSetUp, [#10, #10, atresponse])
                   );
            end;
       end;
  end;
  
  procedure CreateCronScript;
  var
    script: String;
    processresponse: string;
    CronList: TStringList;
  begin
    processresponse := '';
    script := ArchiveForm.GetUniqueScriptName(CronScriptDir);
    if WriteScript(script) then
       try
         CronList := TStringList.Create;
         if ShellCommand('crontab -l', processresponse) = 0 then
            begin
              CronList.Text := processresponse;
              CronList.Add(ArchiveForm.RepeatMinuteBox.Text + #32
                            + ArchiveForm.RepeatHourBox.Text + #32
                            + ArchiveForm.RepeatMonthDayBox.Text + #32
                            + ArchiveForm.RepeatMonthBox.Text + #32
                            + ArchiveForm.RepeatWeekDayBox.Text + #32
                            + script);
              script := '/tmp/' + CreateUniqueFileName('/tmp/');
              CronList.SaveToFile(script);
              if ShellCommand('crontab ' + script, processresponse) = 0 then
                 ShowMessage(rsScriptSetUpOK)
              else ShowMessage(processresponse);
              ShellCommand('rm -f ' + script, processresponse);
            end;
       finally
         if CronList <> nil
            then CronList.Free;
       end;
  end;
  
begin
  ArchiveForm.BatchFileBox.Text := GetNextFileName(TEMP_DIRECTORY + BATCHFILE_BASE);
  try
    if ArchiveForm.ShowModal = mrOk then
       if ArchiveForm.RunOnceRadioButton.Checked
          then CreateAtScript
       else
       if ArchiveForm.RepeatRadioButton.Checked
          then CreateCronScript
       else
       if ArchiveForm.SaveScriptCheckBox.Checked
          then CreateScript
       else CreateNewArchive;
  finally
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

procedure TMainForm.HeaderBarSectionResize(HeaderControl: TCustomHeaderControl;
  Section: THeaderSection);
begin
  ArchiveTreeView.Update;
end;

procedure TMainForm.RecentMenuClick ( Sender: TObject ) ;
var
  fn: string;
begin
  fn := TMenuItem(Sender).Caption + '.1.dar';
  if FileExists(fn) then
     begin
       if ValidateArchive(fn, CurrentPass) then
         begin
           if DarInfo.version<>'-' then  //TODO: disable open menus if dar is absent
              begin
                StatusBar.Panels[SELECT_STATUSBAR].Text := rsMessBUSY;
                Application.ProcessMessages;
                OpenDialog.FileName := fn;
                if OpenArchive(OpenDialog.FileName,ArchiveTreeView, CurrentPass) = 0 then
                    begin
                      EnableArchiveMenus(true);
                      CurrentArchive := TrimToBase(OpenDialog.FileName);
                      RecentList.AddFile(fn);
                    end
                    else
                    begin
                      EnableArchiveMenus(false);
                      CurrentArchive := '';
                    end;
                StatusBar.Panels[SELECT_STATUSBAR].Text := '';
              end;
         end;
       end
       else MessageDlg ( rsErrUnableToFindArchive, mtError, [ mbOk ] , 0 ) ;
       //TODO: if archive not found it should be removed from menu
end;

procedure TMainForm.miArchiveInformationClick(Sender: TObject);
begin
  if GetArchiveInformation(ExtractFilePath(OpenDialog.FileName) + ExtractFileName( CurrentArchive ),
                            InformationForm.InformationMemo,
                            CurrentPass ) = 0
      then
      begin
        InformationForm.Caption := Format ( rsInformationF, [
          ExtractFileName( CurrentArchive ) ] ) ;
        InformationForm.InformationMemo.Lines.Insert ( 0, Format (
          rsInfoFormLocation, [ CurrentArchive ] ) ) ;
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
const
  CELL_RIGHT_MARGIN = 5;
  STATUSRIGHTMARGIN = 20;
var
  FullRect, DisplayRect : TRect;
  FolderGraphic: TBitmap;
  x: Integer;
  DisplayStr: string;
 
     function TrimText(var aText: string; aCol, leftMargin: integer): integer;
     var
       a,
       maxWidth,
       textWidth,
       dotWidth: integer;
       RightAlign: Boolean;
       b: Integer;
     begin
       Result := 0;
       RightAlign := false;
       case aCol of
               SEGSTATUS      : maxwidth :=
                                  FileHeaderBar.SectionFromOriginalIndex[HEADERSTATUS].Width;
               SEGPERMISSIONS : ;  // not displayed at present
               SEGUSER        : maxwidth :=
                                 FileHeaderBar.SectionFromOriginalIndex[HEADERUSER].Width;
               SEGGROUP       : maxwidth :=
                                 FileHeaderBar.SectionFromOriginalIndex[HEADERGROUP].Width;
               SEGSIZE        : maxwidth :=
                                 FileHeaderBar.SectionFromOriginalIndex[HEADERSIZE].Width;
               SEGDATE        : maxwidth :=
                                 FileHeaderBar.SectionFromOriginalIndex[HEADERDATE].Width;
               SEGFILENAME    : maxwidth :=
                                 FileHeaderBar.SectionFromOriginalIndex[HEADERNAME].Width;
               end;
       if aCol < SEGFILENAME then RightAlign := true;
       a := 1;
       b := Length(aText);
       textWidth := Canvas.TextWidth(aText);
       dotWidth := Canvas.TextWidth('...');
       if textWidth > maxWidth then
          begin
             while ((textWidth + dotWidth) > maxWidth) and (b > a) do
                   begin
                   if RightAlign then Inc(a) else Dec(b);
                   textWidth := Canvas.TextWidth(Copy(aText, a, b));
                   end;
          if RightAlign then aText := '...' + Copy(aText, a, b)
             else aText := Copy(aText, a, b) + '...';
          Result := 1;
          end;
     end;
     
     procedure WriteColumn( tvCol: integer; tvRect: TRect; aString: string );
     var
       Section: THeaderSection;
       ColX: Integer;
     begin
       tvRect.Top := tvRect.Top + 1;
       case tvCol of
            SEGSTATUS      : Section := FileHeaderBar.SectionFromOriginalIndex[HEADERSTATUS];
            SEGPERMISSIONS : exit; // not implemented
            SEGUSER        : Section := FileHeaderBar.SectionFromOriginalIndex[HEADERUSER];
            SEGGROUP       : Section := FileHeaderBar.SectionFromOriginalIndex[HEADERGROUP];
            SEGSIZE        : Section := FileHeaderBar.SectionFromOriginalIndex[HEADERSIZE];
            SEGDATE        : Section := FileHeaderBar.SectionFromOriginalIndex[HEADERDATE];
       end;
       ColX := Section.Left;
       TrimText(aString, tvCol, tvRect.Left);
       if tvCol < SEGFILENAME
              then ColX := Section.Left + Section.Width - Sender.Canvas.TextWidth(aString) -CELL_RIGHT_MARGIN;                                                                                             ;
       if tvCol = SEGSTATUS then ColX := ColX-STATUSRIGHTMARGIN; //this is a hack to allow for the scrollbar
       Sender.Canvas.TextOut( ColX, tvRect.Top, aString );
     end;

begin
  Fullrect := Node.DisplayRect(false);
  Displayrect := Node.DisplayRect(true);
  Displayrect.Right := FullRect.Right;
  with Sender.Canvas do
       begin
         if Node.MultiSelected then
            Font.Color := clWhite
         else
            Font.Color := clBlack;
         Font.Size:=9;
        end;
  Sender.Canvas.FillRect(Displayrect);
  if Node.Data <> nil then
     begin
      if TFileData(Node.Data).folder then
         begin
           FolderGraphic := TBitmap.Create;
           IconList.GetBitmap(FOLDERICON, FolderGraphic);
           Sender.Canvas.Draw(Displayrect.Left, DisplayRect.Top, FolderGraphic);
           DisplayStr := TFileData(Node.Data).item[SEGFILENAME];
           TrimText(DisplayStr, SEGFILENAME, Displayrect.Left + FolderGraphic.Width);
           Sender.Canvas.TextOut(Displayrect.Left + FolderGraphic.Width, DisplayRect.Top+1,
                DisplayStr);
           FolderGraphic.Free;
         end
      else
         begin
           DisplayStr := TFileData(Node.Data).item[SEGFILENAME];
           TrimText(DisplayStr, SEGFILENAME, Displayrect.Left);
           Sender.Canvas.TextOut(Displayrect.Left,DisplayRect.Top+1,
                DisplayStr);
         end;
      for x := SEGSTATUS to SEGDATE do
            WriteColumn( x, DisplayRect, TFileData(Node.Data).item[x]);
     end;
end;


{ Adapted from treeview.inc [ Lazarus Component Library ] }
procedure TMainForm.ArchiveTreeViewCompare ( Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer ) ;
  
 {Inline function returns true if Node is a File}
  function IsAFile(  Node : TTreeNode  ) : boolean;
  begin
     Result := not TFileData(Node.Data).folder;
  end;

     {Inline function returns true if Node is a Folder}
  function IsAFolder(  Node : TTreeNode  ) : boolean;
  begin
     Result := TFileData(Node.Data).folder;
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
  OpenDialog.Filter := rsFilterDARArchives + '|*.1.dar';
  if OpenDialog.Execute then
     begin
       StatusBar.Panels[SELECT_STATUSBAR].Text := #32 + rsMessBUSY;
       Application.ProcessMessages;
       fn := TrimToBase( OpenDialog.FileName );
       if not ValidateArchive(fn, CurrentPass)
          then exit else
       if OpenArchive(fn, ArchiveTreeView, CurrentPass) = 0 then
          begin
            EnableArchiveMenus(true);
            CurrentArchive := fn;
            RecentList.AddFile(fn);
          end
       else
          begin
            MessageDlg ( rsErrUnableToOpenArchive, mtError, [ mbOK ] , 0 ) ;
            EnableArchiveMenus(false);
            CurrentArchive := '';
          end;
       StatusBar.Panels[SELECT_STATUSBAR].Text := '';
     end;

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
  Aboutform.VersionLabel.Caption := Format (rsVersionNumber, [ APP_VERSION ] );
  Aboutform.SVNLabel.Caption := Format ( rsSVNRevision, [ SVN_REVISION ] );
  AboutForm.ShowModal;
  Aboutform.Free;
end;

procedure TMainForm.miHelpDarClick ( Sender: TObject ) ;
var
  Browser: string;
  brParams: string;
  Proc: TProcess;
  HelpURL: String;
begin
  brParams := '';
  Browser := '';
  if Sender=miHelpDar then HelpURL := DAR_DOCPAGE
  else if Sender= miHelpContents then HelpURL := DARGUI_HELP;
  GetDefaultBrowser(Browser, brParams);
  if Browser <> '' then
     begin
     Proc := TProcess.Create(Application);
     Proc.CommandLine := Browser + #32 + HelpURL;
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
         Cmd := DAR_EXECUTABLE + ' -C "' + IsolateForm.CatalogueBox.Text
                       + '" -A "' + IsolateForm.ArchiveBox.Text + '" -v';
         if ArchiveIsEncrypted(IsolateForm.ArchiveBox.Text)
            then Cmd := Cmd + ' -J :';
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
        if RestoreForm.DryRunCheckBox.Checked then daroptions := daroptions + ' -e';
        case RestoreForm.OverwriteOptions.ItemIndex of
             0: daroptions := daroptions + ' --no-warn';
             1: ;
             2: daroptions := daroptions + ' --no-overwrite';
             end;
        CommandLine := (DAR_EXECUTABLE + ' -x "' + CurrentArchive + '"  -R "' + RestoreForm.RestoreDirectoryEdit.Text + '"' + daroptions);
        RunDarCommand ( CommandLine, rsCptRestoringFiles, Left + 100, Top + 150
          ) ;
        OpLogForm.AddCommand(CommandLine);
        end
        else MessageMemo.Lines.Add ( rsErrRestoreAborted ) ;
  RestoreForm.Free;
end;

procedure TMainForm.miSchedManagerClick ( Sender: TObject ) ;
var
  SchedForm: TScheduleManagerForm;
begin
  SchedForm := TScheduleManagerForm.Create(Application);
  if SchedForm <> nil then
     try
       SchedForm.ShowModal;
     finally
       SchedForm.Free;
     end;
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
  for x := ArchiveTreeView.Items.Count-1 downto 1 do
      begin
        if ArchiveTreeView.Items[x].MultiSelected
           then if not ArchiveTreeView.Items[x].IsVisible
                then ArchiveTreeView.Items[x].MakeVisible;
      end;
end;

procedure TMainForm.miShowToolbarClick ( Sender: TObject ) ;
begin
  miShowToolbar.Checked := not miShowToolbar.Checked;
  ToolbarPanel.Visible := miShowToolbar.Checked;
  if miShowToolbar.Checked
     then Preferences.WriteString(CfgUserPrefs,CfgShowToolbar,'1')
     else Preferences.WriteString(CfgUserPrefs,CfgShowToolbar,'0');
end;

procedure TMainForm.pmiSelectFilterClick ( Sender: TObject ) ;
begin
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

procedure TMainForm.tbTestClick ( Sender: TObject ) ;
var
  Cmd: String;
begin
  Cmd := DAR_EXECUTABLE + ' -t "' + CurrentArchive + '" -v';
  if FileExists(CurrentArchive + '.1.dar') then
     begin
       if ArchiveIsEncrypted(CurrentArchive)
          then Cmd := Cmd + ' -K :';
       RunDarCommand(Cmd,
                        rsCptDarGUICheckingArchive,
                        Left+100, Top+150);
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
  if SelectedNodes = 0 then exit;
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

procedure TMainForm.EnableArchiveMenus( flag: Boolean );
var
  x: Integer;
begin
  tbTest.Enabled := flag;
  for x := 0 to ComponentCount-1 do
      if Components[x] is TMenuItem then
         if TMenuItem(Components[x]).Tag = ARCHIVEMENU_TAG
            then TMenuItem(Components[x]).Enabled := flag;
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
  tbTest.Hint := rsCheckArchiveForErrors;
  tbSchedMan.Hint := rsSeeSchedBackups;
  MenuHelp.Caption := rsMenuHelp;
  miHelpContents.Caption := rsMenuDarGUIHelp;
  miIsolate.Caption := rsMenuIsolateCatalogue;
  MenuScheduling.Caption := rsScheduling;
  miSchedManager.Caption := rsMenuScheduleManager;
  miTestArchive.Caption := rsMenuCheckIntegrity;
  miOperationlogs.Caption := rsMenuOperationLogs;
  miHelpAbout.Caption := rsMenuHelpAbout;
  miArchiveInformation.Caption := rsMenuInformation;
  miArchiveDiff.Caption := rsMenuCompareFiles;
  miShowSelect.Caption := rsMenuShowSelected;
  miSelectFilter.Caption := rsMenuSelectByFilter;
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
  FileHeaderBar.SectionFromOriginalIndex[HEADERNAME].Text := rsColFileName;
  FileHeaderBar.SectionFromOriginalIndex[HEADERDATE].Text := rsColDate;
  FileHeaderBar.SectionFromOriginalIndex[HEADERSIZE].Text := rsColSize;
  FileHeaderBar.SectionFromOriginalIndex[HEADERUSER].Text := rsColUser;
  FileHeaderBar.SectionFromOriginalIndex[HEADERGROUP].Text := rsColGroup;
  FileHeaderBar.SectionFromOriginalIndex[HEADERSTATUS].Text := rsColStatus;
end;


initialization
  {$I main.lrs}

end.

