unit SFFS.Types;

interface

uses
  WinApi.Windows,
  MD5;

type
  TFileIndex = type uint64;
  TFileOffset = type uint64;

  TSFFSHeader = packed record
    { 00 } Sig: array [0 .. 3] of AnsiChar; // SFFS
    { 04 } Ver: UInt32;                     // Version: 1
    { 08 } FileCount: uint64;               // Encrypted with AppKey
    { 16   end }
  end;

  TSFFSFileBaseInfo = packed record
    { 00 } FileNameMD5: TMD5Digest; // not encrypted
    { 16 } FileIndex: TFileIndex;   // encrypted with filename
    { 24   end }
  end;

  TSFFSFileHeader = packed record
    { 00 } StartOfFileContent: uint64; // content is encrypted with filename
    { 08 } FileSize: uint64;           // 0 for directory
    { 16 } LastAccessTime: FILETIME;   // 0 for directory
    { 24 } CreationTime: FILETIME;     // 0 for directory
    { 32 } LastWriteTime: FILETIME;    // 0 for directory
    { 40 } Reserved: uint64;           // if <> 0 then STATUS_NO_SUCH_FILE
    { 48 } FileAttributes: uint64;     // usually FILE_ATTRIBUTE_ARCHIVE, also FILE_ATTRIBUTE_DIRECTORY for directory
    { 56   end }
  end;

implementation

end.
