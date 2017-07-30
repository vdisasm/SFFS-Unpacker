unit uFormMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  MD5,

  SFFS.Helpers,
  SFFS.Unpacker.Thread,
  SFFS.Unpacker.Project;

type
  TFormMain = class(TForm)
    ListViewFiles: TListView;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuSaveProject: TMenuItem;
    Addfilenames1: TMenuItem;
    MenuCloseProject: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    MenuExportListAsText: TMenuItem;
    N2: TMenuItem;
    AbortExtraction1: TMenuItem;
    MenuExtractAll: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure MenuSaveProjectClick(Sender: TObject);
    procedure ListViewFilesData(Sender: TObject; Item: TListItem);
    procedure FormCreate(Sender: TObject);
    procedure Addfilenames1Click(Sender: TObject);
    procedure MenuCloseProjectClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ListViewFilesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure MenuExportListAsTextClick(Sender: TObject);
    procedure AbortExtraction1Click(Sender: TObject);
    procedure MenuExtractAllClick(Sender: TObject);
  private
    FActivationDone: Boolean;
  protected
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure WMSffsProgress(var Message: TMessage); message WM_SFFS_PROGRESS;
  public
    prj: TSFFSUnpackerProject;
    procedure DoOpenFileOrProject(const FileName: string);
    procedure DoCloseProject;
    procedure DoRefresh;
    procedure DoRefreshStatusBar;
    procedure DoTryAddFilesFromOpenedList;
  end;

var
  FormMain: TFormMain;

implementation


{$R *.dfm}


uses
  uFormAddFileNames;

function open_prj(const FileName: string): TSFFSUnpackerProject;
var
  fn, tmp, AppKey: string;
begin
  result := nil;

  if FileName = '' then
    exit;

  if not FileExists(FileName) then
    raise Exception.CreateFmt('File not found: %s', [FileName]);

  // If it's not sffs project.
  if not FileName.ToLower.EndsWith(TSFFSUnpackerProject.PROJECT_EXT) then
  begin
    tmp := FileName + TSFFSUnpackerProject.PROJECT_EXT;

    // If there no project for this dat, then create new project from dat.
    if not FileExists(tmp) then
    begin
      if InputQuery('Enter AppKey for ' + ExtractFileName(FileName), 'AppKey', AppKey) then
        exit(TSFFSUnpackerProject.CreateFromDat(FileName, AppKey))
      else
        exit(nil);
    end;

    fn := tmp;
  end
  else
  begin
    fn := FileName;
  end;

  exit(TSFFSUnpackerProject.CreateFromXml(fn));
end;

procedure TFormMain.AbortExtraction1Click(Sender: TObject);
begin
  if Assigned(prj) then
    prj.ExtractionAbort;
end;

procedure TFormMain.Addfilenames1Click(Sender: TObject);
begin
  if Assigned(prj) then
    FormAddFiles.Show;
end;

procedure TFormMain.DoCloseProject;
begin
  if Assigned(prj) then
  begin
    if prj.Modified then
      if MessageDlg('Do you want to save project changes?',
        TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        prj.SaveProject;
      end;
    FreeAndNil(prj);
  end;
  DoRefresh;
  FormAddFiles.Close;
end;

procedure TFormMain.DoOpenFileOrProject(const FileName: string);
begin
  if FileExists(FileName) then
  begin
    DoCloseProject;
    prj := open_prj(FileName);
    DoRefresh;
  end;
end;

procedure TFormMain.DoRefresh;
begin
  if Assigned(prj) then
  begin
    ListViewFiles.Items.Count := prj.FileInfoList.Count;
  end
  else
  begin
    ListViewFiles.Items.Count := 0;
  end;
  ListViewFiles.Invalidate;
  DoRefreshStatusBar;
end;

procedure TFormMain.DoRefreshStatusBar;
begin
  if Assigned(prj) then
  begin
    if Assigned(ListViewFiles.Selected) then
      StatusBar1.Panels[0].Text := format('%d/%d',
        [ListViewFiles.Selected.Index, ListViewFiles.Items.Count - 1]);

    StatusBar1.Panels[1].Text := format('%d/%d found',
      [prj.FileNamesFound, prj.FileCount]);
  end
  else
  begin
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := '';
  end;
end;

procedure TFormMain.WMSffsProgress(var Message: TMessage);
begin
  StatusBar1.Panels[2].Text := format('%d%%', [Message.WParam]);
end;

procedure TFormMain.DoTryAddFilesFromOpenedList;
var
  i: Integer;
begin
  if not Assigned(prj) then
    exit;

  for i := FormAddFiles.Memo1.Lines.Count - 1 downto 0 do
  begin
    if prj.AddFileName(FormAddFiles.Memo1.Lines[i].Trim) <> -1 then
    begin
      FormAddFiles.Memo1.Lines.Delete(i);
    end;
  end;

  DoRefresh;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  if not FActivationDone then
  begin
    FActivationDone := True;
    DoOpenFileOrProject(ParamStr(1));
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  DoCloseProject;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(handle, True);
end;

procedure TFormMain.ListViewFilesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  DoRefreshStatusBar;
end;

procedure TFormMain.ListViewFilesData(Sender: TObject; Item: TListItem);
var
  Info: TSFFSUnpackerProject.TSFFSFileBaseInfoEx;
begin
  Info := prj.FileInfoList[Item.Index];

  // md5
  Item.Caption := Info.Base.FileNameMD5.ToString;

  // index
  if Info.FileName <> '' then
    Item.SubItems.Add(format('%d', [Info.Base.FileIndex]))
  else
    Item.SubItems.Add('');

  // filename
  if Info.FileName <> '' then
    Item.SubItems.Add(Info.FileName)
  else
    Item.SubItems.Add('');
end;

procedure TFormMain.ListViewFilesDblClick(Sender: TObject);
begin
  if Assigned(prj) and Assigned(ListViewFiles.Selected) then
  begin
    if prj.ExtractionPrepare(handle, True) = TSFFSStatus.OK then
    begin
      prj.ExtractionAdd(ListViewFiles.Selected.Index);
      prj.ExtractionStart;
    end;
  end;
end;

procedure TFormMain.MenuCloseProjectClick(Sender: TObject);
begin
  DoCloseProject;
end;

procedure TFormMain.MenuExportListAsTextClick(Sender: TObject);
const
  S_EXPORT_TXT = 'export.txt';
var
  i: Integer;
  sl: TStringList;
  Info: TSFFSUnpackerProject.TSFFSFileBaseInfoEx;
  str_index: string;
  str_filename: string;
begin
  if Assigned(prj) then
    if prj.FileInfoList.Count <> 0 then
    begin
      sl := TStringList.Create;
      try
        for i := 0 to prj.FileInfoList.Count - 1 do
        begin
          Info := prj.FileInfoList[i];

          str_filename := Info.FileName;

          if str_filename <> '' then
            str_index := IntToStr(Info.Base.FileIndex)
          else
            str_index := '';

          sl.Add(format('%s %-10s %s', [
            Info.Base.FileNameMD5.ToString,
            str_index,
            str_filename
            ]));
        end;

        sl.SaveToFile(S_EXPORT_TXT);
        OpenInExplorer(S_EXPORT_TXT);
      finally
        sl.Free;
      end;
    end;
end;

procedure TFormMain.MenuExtractAllClick(Sender: TObject);
begin
  if Assigned(prj) then
    prj.ExtractAll(handle, True);
end;

procedure TFormMain.MenuSaveProjectClick(Sender: TObject);
begin
  if Assigned(prj) then
    prj.SaveProject();
end;

procedure TFormMain.WMDropFiles(var Message: TWMDropFiles);
var
  buf: array [0 .. 1024] of char;
begin
  DragQueryFile(Message.Drop, 0, buf, 1024);
  DoOpenFileOrProject(buf);
end;

end.
