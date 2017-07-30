unit SFFS.Unpacker.Thread;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,

  WinApi.Messages,
  WinApi.Windows,

  SFFS.Helpers,
  SFFS.Types,

  Blowfish;

const
  // WM_SFFS_EXTRACTION_STARTED  = WM_USER + 1;
  // WM_SFFS_EXTRACTION_FINISHED = WM_USER + 2;
  // WM_SFFS_EXTRACTION_ABORTED  = WM_USER + 3;
  WM_SFFS_PROGRESS = WM_USER + 4;

type
  TUnpackTask = record
  public
    FileHdr: TSFFSFileHeader;
    FileName: string;
    constructor Create(
      const FileHdr: TSFFSFileHeader;
      const FileName: string);
  end;

  TUnpackTaskList = TList<TUnpackTask>;

  {
    TUnpackerThread unpacks files (stored in tasks) from single container into
    destination directory.
  }
  TUnpackerThread = class(TThread)
  protected
    FList: TUnpackTaskList;
    FWnd: HWND;
    FSource: TFileStream;
    FOutDir: string;
    procedure Execute; override;
  public
    OpenFolderAfterExtraction: boolean;     // default: False
    SkipExistingFileWithValidSize: boolean; // default: True

    constructor Create(Wnd: HWND; Source: TFileStream; const OutDir: string);
    destructor Destroy; override;
    property List: TUnpackTaskList read FList;
    property Source: TFileStream read FSource;
  end;

implementation

{ TUnpackerThread }

constructor TUnpackerThread.Create;
begin
  inherited Create(True);
  SkipExistingFileWithValidSize := True;
  FWnd := Wnd;
  FSource := Source;
  FOutDir := OutDir;
  FList := TUnpackTaskList.Create;
end;

destructor TUnpackerThread.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TUnpackerThread.Execute;
const
  DEF_BUF_SIZE = 1024 * 256; // must be 8-aligned
var
  Task: TUnpackTask;
  Left: UInt64;
  Dst: TFileStream;
  Buf: Pointer;
  ReadCnt, ReadCntAligned: integer;
  bf: TBlowFish;
  key: string;
  Offset: UInt64;
  DstFn, DstDir, DstFnUnfinished: string;
  p: PUInt64;
  TotalSize, TotalDone: UInt64;
  TotalPercent, Tmp1: integer;
  DstFileExistsWithMatchingSize: boolean;
  DirToOpen, FileToSelect: string;
  procedure NotifyProgress(TotalPercent: integer);
  begin
    PostMessage(FWnd, WM_SFFS_PROGRESS, TotalPercent, 0);
  end;

begin
  if FList.Count = 0 then
    exit;

  // By default we open out dir (if there are many files).
  // In case there are only one file we open dir of this file.
  DirToOpen := '';
  FileToSelect := '';
  if OpenFolderAfterExtraction then
  begin
    if FList.Count = 1 then
    begin
      if (FList.First.FileHdr.FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        FileToSelect := FOutDir + PathDelim + FList.First.FileName // file
      else
        DirToOpen := FOutDir + PathDelim + FList.First.FileName; // dir
    end
    else
      DirToOpen := FOutDir;
  end;

  TotalPercent := 0;
  TotalDone := 0;
  TotalSize := 0;
  for Task in FList do
    inc(TotalSize, Task.FileHdr.FileSize);

  Buf := AllocMem(DEF_BUF_SIZE);
  try

    while (FList.Count <> 0) do
    begin
      // Get task.
      Task := FList.Last;
      FList.Delete(FList.Count - 1);

      DstFn := FOutDir + PathDelim + Task.FileName;

      // If it's dir just create dir and continue to next task
      if (Task.FileHdr.FileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
      begin
        TDirectory.CreateDirectory(DstFn);
        continue;
      end;

      // Check if destination file already exists and has valid size and skip
      // if it is.
      if SkipExistingFileWithValidSize then
      begin
        DstFileExistsWithMatchingSize := False;
        if FileExists(DstFn) then
        begin
          Dst := TFileStream.Create(DstFn, fmOpenRead or fmShareDenyWrite);
          try
            DstFileExistsWithMatchingSize := Dst.Size = Task.FileHdr.FileSize;
          finally
            FreeAndNil(Dst);
          end;
        end;
        if DstFileExistsWithMatchingSize then
        begin
          // Skip extraction.
          inc(TotalDone, Task.FileHdr.FileSize);
          continue;
        end;
      end;

      // Run task.
      DstDir := ExtractFileDir(DstFn);
      TDirectory.CreateDirectory(DstDir);

      DstFnUnfinished := DstFn + '.unfinished';
      Dst := TFileStream.Create(DstFnUnfinished, fmCreate);

      key := ReverseString(Task.FileName.ToUpper);
      bf := TBlowFish.Create(PChar(key)^, key.Length * SizeOf(char));

      FSource.Position := Task.FileHdr.StartOfFileContent;

      try
        Offset := Task.FileHdr.StartOfFileContent;
        Left := Task.FileHdr.FileSize;
        while Left > 0 do
        begin
          if Terminated then
          begin
            FreeAndNil(Dst);                    // close dest file
            DeleteFile(PChar(DstFnUnfinished)); // delete unfinished file
            exit;                               // abort
          end;

          if Left < DEF_BUF_SIZE then
            ReadCnt := Left
          else
            ReadCnt := DEF_BUF_SIZE;

          ReadCntAligned := AlignUp8(ReadCnt);

          if FSource.Read(Buf^, ReadCntAligned) <> ReadCntAligned then
          begin
            break; // read error
          end;

          // Decrypt buffer
          p := Buf;
          while ReadCntAligned > 0 do
          begin
            bf.Decrypt(p^);
            p^ := p^ xor Offset;
            inc(p);
            inc(Offset, 8);
            dec(ReadCntAligned, 8);
          end;

          // Write buffer
          Dst.Write(Buf^, ReadCnt);

          dec(Left, ReadCnt);

          // Update progress
          inc(TotalDone, ReadCnt);
          Tmp1 := Trunc(100 * (TotalDone / TotalSize));
          if (Tmp1 <> TotalPercent) and (Tmp1 <> 100) then
          begin
            TotalPercent := Tmp1;
            NotifyProgress(TotalPercent);
          end;
        end;

        // Data extracted.

        // Apply times.
        SetFileTime(Dst.Handle,
          @Task.FileHdr.CreationTime,
          @Task.FileHdr.LastAccessTime,
          @Task.FileHdr.LastWriteTime);

      finally
        bf.Free;
        FreeAndNil(Dst);
      end;

      // Rename unfinished to normal.
      RenameFile(DstFnUnfinished, DstFn);

      // Apply attributes (when file handle closed).
      SetFileAttributes(PChar(DstFn), Task.FileHdr.FileAttributes);

    end; // end of task loop

    // All tasks done.
    NotifyProgress(100);

    // Open dir or select file.
    if DirToOpen <> '' then
      OpenInExplorer(DirToOpen)
    else if FileToSelect <> '' then
      SelectFileInExplorer(FileToSelect);
  finally
    FreeAndNil(FSource);
    FreeMem(Buf);
  end;
end;

{ TUnpackTask }

constructor TUnpackTask.Create(const FileHdr: TSFFSFileHeader; const FileName: string);
begin
  self.FileHdr := FileHdr;
  self.FileName := FileName;
end;

end.
