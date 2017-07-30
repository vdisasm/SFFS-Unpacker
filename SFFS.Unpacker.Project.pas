unit SFFS.Unpacker.Project;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Generics.Collections,

  WinApi.Windows,

  NativeXML,
  MD5,
  SFFS.AppKey,
  SFFS.Helpers,
  SFFS.Types,
  SFFS.Unpacker.Thread;

type
{$SCOPEDENUMS ON}
  TSFFSStatus = (
    OK,
    ERROR,
    NOT_INITIALIZED,
    EXTRACTION_STARTED, // extraction thread started
    EXTRACTION_BUSY,    // there is currenly active extraction
    DAT_FILE_NOT_FOUND,
    FILE_INDEX_FAILED,
    READ_ERROR
    );
{$SCOPEDENUMS OFF}

  TSFFSUnpackerProject = class
  private
    // Dat is needed for:
    // - creating initial project
    // - unpacking files
    FSrcDir: string;
    FDatFileName: string;
  public type
    TSFFSFileBaseInfoEx = class
    private
      FBase: TSFFSFileBaseInfo;
      FFileName: string;
    public
      constructor Create(const Base: TSFFSFileBaseInfo);
      property Base: TSFFSFileBaseInfo read FBase;
      property FileName: string read FFileName;
    end;

    // Flat list of base file info.
    TFileInfoList = TObjectList<TSFFSFileBaseInfoEx>;

    // Filename md5 -> index of info in flat list.
    TFileNameMap = TDictionary<TMD5Digest, integer>;
  private const
    SFailedToLoadSFFSHeader = 'Failed to load SFFS header';
    DEFAULT_UNPACK_DIR      = 'sffs_out';
  public const
    PROJECT_EXT = '.sffs.xml';
  private
    FAppKey: TAppKey;
    FFileCount: uint64;
    FFileNamesFound: integer;
    FFileInfoList: TFileInfoList;
    FFileInfoMap: TFileNameMap;
    FUnpackThread: TUnpackerThread; // nil if no pending task otherwise tasks are running
    procedure LoadHeader;
    procedure FilesClear;
    procedure FilesAdd(const Info: TSFFSFileBaseInfo; const FileName: string = '');
    function LoadHeaderFromStream(Stream: TStream): boolean;
    function GetDatPath: string;
    function GetPrjPath: string;
  private
    FModified: boolean;
    constructor Create;
  public
    destructor Destroy; override;

    constructor CreateFromXml(const FileName: string);
    constructor CreateFromDat(const FileName: string; const AppKey: TAppKey);

    procedure SaveProject(const FileName: string = '');

    // Search for file by hash. Return index in FileInfoList or -1 if failed.
    function FindFileName(const FileName: string): integer;

    // Search for hash according to file name. If it's found name is stored and
    // index in FileInfoList returned. If nothing found -1 returned.
    function AddFileName(const FileName: string): integer;
  private
    // Calculate file info offset by index.
    // Return False if offset is out of file.
    function GetFileInfoOffset(
      FileIndex: integer;
      out Offset: TFileOffset;
      DatFileSize: uint64): boolean;
  public
    // Can return:
    // OK: if extraction thread initialized.
    // EXTRACTION_BUSY is there is some extraction now. You need to abort it first.
    // DAT_FILE_NOT_FOUND: no source DAT file found
    function ExtractionPrepare(
      Wnd: HWND;
      OpenFolder: boolean;
      const OutDir: string = ''): TSFFSStatus;

    // Add file to extraction by index from flat list.
    // Can return:
    // OK: if task added.
    // EXTRACTION_BUSY
    // FILE_INDEX_FAILED
    // READ_ERROR
    function ExtractionAdd(Index: integer): TSFFSStatus;

    // Start async extraction (run thread).
    // Extraction information will be freed when extraction done or when it is
    // aborted.
    // Can return:
    // OK
    // NOT_INITIALIZED
    // EXTRACTION_BUSY
    function ExtractionStart: TSFFSStatus;

    // Blocking function.
    // Request unpacker thread to abort and wait for termination.
    procedure ExtractionAbort;

    function ExtractAll(
      Wnd: HWND;
      OpenFolder: boolean;
      const OutDir: string = ''): TSFFSStatus;

    property SrcDir: string read FSrcDir;
    property DatFileName: string read FDatFileName;
    property AppKey: TAppKey read FAppKey;
    property FileCount: uint64 read FFileCount;
    property FileNamesFound: integer read FFileNamesFound;
    property FileInfoList: TFileInfoList read FFileInfoList;
    property Modified: boolean read FModified;
  end;

implementation

{ TSFFSUnpackerProject }

function TSFFSUnpackerProject.FindFileName(const FileName: string): integer;
var
  fn: string;
  hash: TMD5Digest;
  Index: integer;
begin
  fn := FileName.ToUpper;
  hash := MD5String(fn);
  if not FFileInfoMap.TryGetValue(hash, Index) then
    result := -1
  else
    result := Index;
end;

function TSFFSUnpackerProject.AddFileName(const FileName: string): integer;
var
  Info: TSFFSFileBaseInfoEx;
begin
  result := FindFileName(FileName);
  if result <> -1 then
  begin
    Info := FFileInfoList[result];
    if Info.FFileName = '' then
    begin
      Info.FFileName := FileName;
      BlowfishDecryptWithFileName(FileName, Info.FBase.FileIndex, SizeOf(Info.FBase.FileIndex));
      inc(FFileNamesFound);
      FModified := True;
    end;
  end;
end;

function TSFFSUnpackerProject.ExtractionPrepare(
  Wnd: HWND;
  OpenFolder: boolean;
  const OutDir: string): TSFFSStatus;
var
  Dir: string;
  Src: TFileStream;
begin
  if Assigned(FUnpackThread) then
  begin
    if FUnpackThread.Finished then
      FreeAndNil(FUnpackThread)
    else
      Exit(TSFFSStatus.EXTRACTION_BUSY);
  end;

  if not FileExists(GetDatPath) then
    Exit(TSFFSStatus.DAT_FILE_NOT_FOUND);

  if OutDir <> '' then
    Dir := OutDir
  else
    Dir := DEFAULT_UNPACK_DIR;

  Src := TFileStream.Create(GetDatPath, fmOpenRead or fmShareDenyWrite);
  FUnpackThread := TUnpackerThread.Create(Wnd, Src, Dir);
  FUnpackThread.OpenFolderAfterExtraction := OpenFolder;

  Exit(TSFFSStatus.OK);
end;

function TSFFSUnpackerProject.ExtractionAdd(Index: integer): TSFFSStatus;
var
  Info: TSFFSFileBaseInfoEx;
  FileInfoOffset: TFileOffset;
  FileHdr: TSFFSFileHeader;
begin
  if not Assigned(FUnpackThread) then
    Exit(TSFFSStatus.NOT_INITIALIZED);

  if FUnpackThread.Started then
    Exit(TSFFSStatus.EXTRACTION_BUSY);

  if Index = -1 then
    Exit(TSFFSStatus.FILE_INDEX_FAILED);

  // Get file header.

  Info := FFileInfoList[Index];

  if not GetFileInfoOffset(Info.FBase.FileIndex, FileInfoOffset, FUnpackThread.Source.Size) then
    Exit(TSFFSStatus.FILE_INDEX_FAILED);

  FUnpackThread.Source.Position := FileInfoOffset;

  if FUnpackThread.Source.Read(FileHdr, SizeOf(TSFFSFileHeader)) <> SizeOf(TSFFSFileHeader) then
    Exit(TSFFSStatus.READ_ERROR);

  // Decrypt #1
  BlowfishDecryptWithFileName(Info.FileName, FileHdr, SizeOf(FileHdr));

  // Xor with AppKey
  FAppKey.&Xor(FileHdr, SizeOf(FileHdr));

  // Decrypt #2
  BlowfishDecryptWithFileName(Info.FileName, FileHdr, SizeOf(FileHdr));

  FUnpackThread.List.Add(TUnpackTask.Create(FileHdr, Info.FileName));

  Exit(TSFFSStatus.OK);
end;

function TSFFSUnpackerProject.ExtractionStart: TSFFSStatus;
begin
  if not Assigned(FUnpackThread) then
    Exit(TSFFSStatus.NOT_INITIALIZED);

  if (FUnpackThread.Started) then
    Exit(TSFFSStatus.EXTRACTION_BUSY);

  FUnpackThread.Start;
  Exit(TSFFSStatus.OK);
end;

procedure TSFFSUnpackerProject.ExtractionAbort;
begin
  if Assigned(FUnpackThread) then
  begin
    FUnpackThread.Terminate;
    FUnpackThread.WaitFor;
    FreeAndNil(FUnpackThread);
  end;
end;

function TSFFSUnpackerProject.ExtractAll(
  Wnd: HWND;
  OpenFolder: boolean;
  const OutDir: string): TSFFSStatus;
var
  i: integer;
begin
  if FFileInfoList.Count = 0 then
    Exit(TSFFSStatus.OK);

  result := ExtractionPrepare(Wnd, OpenFolder, OutDir);
  if result <> TSFFSStatus.OK then
    Exit;

  for i := 0 to FFileInfoList.Count - 1 do
  begin
    // Add if it has file name.
    if FFileInfoList[i].FileName <> '' then
      ExtractionAdd(i);
  end;

  result := ExtractionStart();
end;

constructor TSFFSUnpackerProject.Create;
begin
  inherited;
  FFileInfoList := TFileInfoList.Create;
  FFileInfoMap := TFileNameMap.Create;
end;

destructor TSFFSUnpackerProject.Destroy;
begin
  ExtractionAbort; // make sure unpack thread is stopped
  FFileInfoMap.Free;
  FFileInfoList.Free;
  inherited;
end;

procedure TSFFSUnpackerProject.FilesClear;
begin
  FFileInfoList.Clear;
  FFileInfoMap.Clear;
  FFileNamesFound := 0;
end;

procedure TSFFSUnpackerProject.FilesAdd(const Info: TSFFSFileBaseInfo; const FileName: string);
var
  Item: TSFFSFileBaseInfoEx;
begin
  Item := TSFFSFileBaseInfoEx.Create(Info);
  Item.FFileName := FileName;

  if FileName <> '' then
    inc(FFileNamesFound);

  FFileInfoMap.Add(Info.FileNameMD5, FFileInfoList.Count);
  FFileInfoList.Add(Item);

  FModified := True;
end;

constructor TSFFSUnpackerProject.CreateFromDat(const FileName: string; const AppKey: TAppKey);
begin
  Self.Create;
  FAppKey := AppKey;
  FSrcDir := ExtractFileDir(FileName);
  FDatFileName := ExtractFileName(FileName);
  LoadHeader;
  FModified := True; // project must be saved
end;

const
  S_PRJ_ROOT         = 'sffsproj';
  S_PRJ_DAT_FILENAME = 'datfile';
  S_PRJ_APPKEY       = 'appkey';
  S_PRJ_FILES        = 'files';
  S_PRJ_FILE         = 'file';
  S_PRJ_FILECOUNT    = 'count';
  S_PRJ_FILEMD5      = 'md5';
  S_PRJ_FILEINDEX    = 'index';
  S_PRJ_FILENAME     = 'name';

constructor TSFFSUnpackerProject.CreateFromXml(const FileName: string);
var
  xml: TNativeXml;
  nFiles, nFile: TXmlNode;
  i: integer;
  Info: TSFFSFileBaseInfo;
  s: string;
begin
  Self.Create;

  xml := TNativeXml.Create(nil);
  try
    xml.LoadFromFile(FileName);

    FSrcDir := ExtractFileDir(FileName);

    FDatFileName := string(xml.Root.ReadString(S_PRJ_DAT_FILENAME));
    FAppKey := string(xml.Root.ReadString(S_PRJ_APPKEY));

    nFiles := xml.Root.FindNode(S_PRJ_FILES);
    FFileCount := nFiles.ReadInteger(S_PRJ_FILECOUNT);

    if FFileCount <> 0 then
    begin
      for i := 0 to nFiles.NodeCount - 1 do
      begin
        nFile := nFiles.Nodes[i];
        s := string(nFile.FullPath);
        if s.EndsWith(S_PRJ_FILE) then
        begin
          Info.FileNameMD5 := MD5DigestFromString(string(nFile.ReadString(S_PRJ_FILEMD5)));
          Info.FileIndex := nFile.ReadInt64(S_PRJ_FILEINDEX);
          s := string(nFile.ReadString(S_PRJ_FILENAME));
          FilesAdd(Info, s);
        end;
      end;
    end;
    FModified := False;
  finally
    xml.Free;
  end;
end;

procedure TSFFSUnpackerProject.SaveProject(const FileName: string);
var
  xml: TNativeXml;
  nFiles, nFile: TXmlNode;
  fn: string;
  cnt, Index: integer;
  Info: TSFFSFileBaseInfoEx;
begin
  xml := TNativeXml.CreateEx(nil, False, False, True, S_PRJ_ROOT);
  try
    xml.XmlFormat := xfReadable;

    // dat file name
    xml.Root.WriteString(S_PRJ_DAT_FILENAME, UTF8String(Self.FDatFileName));

    // appkey
    xml.Root.WriteString(S_PRJ_APPKEY, UTF8String(string(Self.FAppKey)));

    // files
    nFiles := xml.Root.NodeNew(S_PRJ_FILES);
    if FAppKey.IsNull then
    begin
      nFiles.WriteInteger(S_PRJ_FILECOUNT, -1);
    end
    else
    begin
      cnt := Self.FFileInfoMap.Count;
      nFiles.WriteInteger(S_PRJ_FILECOUNT, cnt);
      for Index in Self.FFileInfoMap.Values do
      begin
        Info := FFileInfoList[Index];

        nFile := nFiles.NodeNew(S_PRJ_FILE);
        nFile.WriteString(S_PRJ_FILEMD5, UTF8String(Info.Base.FileNameMD5.ToString));
        nFile.WriteInt64(S_PRJ_FILEINDEX, Info.Base.FileIndex);
        nFile.WriteString(S_PRJ_FILENAME, UTF8String(Info.FileName));
      end;
    end;

    if FileName <> '' then
      fn := FileName
    else
      fn := GetPrjPath;

    xml.SaveToFile(fn);

    FModified := False;
  finally
    xml.Free;
  end;
end;

procedure TSFFSUnpackerProject.LoadHeader;
var
  fn: string;
  Stream: TFileStream;
begin
  fn := GetDatPath;
  Stream := TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
    if not LoadHeaderFromStream(Stream) then
      raise Exception.Create(SFailedToLoadSFFSHeader);
  finally
    Stream.Free;
  end;
end;

function TSFFSUnpackerProject.LoadHeaderFromStream(Stream: TStream): boolean;
type
  TSFFSFileBaseInfoArray = array of TSFFSFileBaseInfo;
var
  Header: TSFFSHeader;
  FileBaseInfo: TSFFSFileBaseInfoArray;
  i, Size: integer;
  DummyOffset: TFileOffset;
begin
  FilesClear;

  if
    (Stream.Read(Header, SizeOf(Header)) <> SizeOf(Header)) or
    (Header.Sig <> 'SFFS') or
    (Header.Ver <> 1) then
  begin
    Exit(False);
  end;

  FFileCount := Header.FileCount;
  BlowfishDecrypt(FAppKey, APPKEY_LEN, FFileCount, SizeOf(FFileCount));

  // Simple validation.
  if not GetFileInfoOffset(FFileCount - 1, DummyOffset, Stream.Size) then
    Exit(False);

  SetLength(FileBaseInfo, FFileCount);
  Size := FFileCount * SizeOf(TSFFSFileBaseInfo);
  if Stream.Read(FileBaseInfo[0], Size) <> Size then
  begin
    Exit(False);
  end;

  for i := 0 to high(FileBaseInfo) do
    FilesAdd(FileBaseInfo[i]);

  Exit(True);
end;

function TSFFSUnpackerProject.GetDatPath: string;
begin
  result := Self.FSrcDir + PathDelim + Self.FDatFileName;
end;

function TSFFSUnpackerProject.GetFileInfoOffset(
  FileIndex: integer;
  out Offset: TFileOffset;
  DatFileSize: uint64): boolean;
begin
  Offset :=
    SizeOf(TSFFSHeader) +
    FFileCount * SizeOf(TSFFSFileBaseInfo) +
    FileIndex * SizeOf(TSFFSFileHeader);

  result := DatFileSize > (Offset + SizeOf(TSFFSFileHeader));
end;

function TSFFSUnpackerProject.GetPrjPath: string;
begin
  result := GetDatPath + PROJECT_EXT;
end;

{ TSFFSUnpackerProject.TSFFSFileBaseInfoEx }

constructor TSFFSUnpackerProject.TSFFSFileBaseInfoEx.Create(const Base: TSFFSFileBaseInfo);
begin
  inherited Create;
  FBase := Base;
end;

end.
