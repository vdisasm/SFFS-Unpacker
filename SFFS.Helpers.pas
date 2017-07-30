unit SFFS.Helpers;

interface

uses
  System.SysUtils,
  WinApi.Windows,
  WinApi.ShellApi,
  Blowfish;

function FileTime2DateTime(FileTime: TFileTime): TDateTime;
function AlignUp8(Value: uint64): uint64; // inline;
function ReverseString(const s: string): string;
procedure BlowfishDecrypt(const Key; KeySize: integer; var Buf; BufSize: integer; Base: uint64 = 0); overload;
procedure BlowfishDecrypt(const Key: string; var Buf; BufSize: integer; Base: uint64 = 0); overload;
procedure BlowfishDecryptWithFileName(const FileName: string; var Buf; BufSize: integer; Base: uint64 = 0);
procedure SelectFileInExplorer(const FileName: string);
procedure OpenInExplorer(const Dir: string);

implementation

function FileTime2DateTime(FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
end;

function AlignUp8(Value: uint64): uint64; // inline;
var
  d, m: uint64;
begin
  m := Value mod 8;
  if m = 0 then
    exit(Value);
  d := (Value div 8) + 1;
  exit(d * 8);
end;

function ReverseString(const s: string): string;
var
  o, i: integer;
begin
  SetLength(Result, s.Length);
  o := 1;
  for i := s.Length downto 1 do
  begin
    Result[i] := s[o];
    inc(o);
  end;
end;

procedure BlowfishDecrypt(const Key; KeySize: integer; var Buf; BufSize: integer; Base: uint64 = 0); overload;
type
  PBFBlock = ^TBFBlock;
var
  bf: TBlowFish;
  p: PBFBlock;
  i, BlockCount: uint64;
begin
  if BufSize = 0 then
    exit;

  if (BufSize mod 8) <> 0 then
    raise Exception.Create('Buffer size must be multiple of 8');

  bf := TBlowFish.Create(Key, KeySize);
  try
    p := @Buf;
    BlockCount := BufSize div 8;
    i := 0;
    while i < BlockCount do
    begin
      bf.Decrypt(p^);

      if Base <> 0 then
        PUInt64(p)^ := PUInt64(p)^ xor (Base + 8 * i);

      inc(p);
      inc(i);
    end;
  finally
    bf.Free;
  end;
end;

procedure BlowfishDecrypt(const Key: string; var Buf; BufSize: integer; Base: uint64 = 0); overload;
begin
  BlowfishDecrypt(PChar(Key)^, Key.Length * SizeOf(Key[1]), Buf, BufSize, Base);
end;

procedure BlowfishDecryptWithFileName(const FileName: string; var Buf; BufSize: integer; Base: uint64 = 0);
begin
  BlowfishDecrypt(ReverseString(FileName.ToUpper), Buf, BufSize, Base);
end;

procedure SelectFileInExplorer(const FileName: string);
begin
  ShellExecute(0, 'open', 'explorer.exe', PChar(format('/select,"%s"', [FileName])), nil, SW_SHOW);
end;

procedure OpenInExplorer(const Dir: string);
begin
  ShellExecute(0, 'open', 'explorer.exe', PChar(Dir), nil, SW_SHOW);
end;

end.
