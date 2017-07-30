unit SFFS.AppKey;

interface

uses
  System.SysUtils;

const
  APPKEY_LEN = 8;

type
  TAppKey = packed record
  public
    // key like '1122334455667788'
    class operator Implicit(const a: string): TAppKey; static;
    class operator Implicit(const a: TAppKey): string; static;
    procedure Clear; inline;
    function IsNull: boolean; inline;
    procedure &Xor(const Buffer; Size: integer);
  private
    case byte of
      0:
        (bytes: array [0 .. APPKEY_LEN - 1] of byte);
      1:
        (u64: uint64);
  end;

implementation


procedure TAppKey.&Xor(const Buffer; Size: integer);
var
  i: integer;
  p: PByte;
begin
  p := @Buffer;
  for i := 0 to Size - 1 do
  begin
    p^ := p^ xor bytes[i mod APPKEY_LEN];
    inc(p);
  end;
end;

procedure TAppKey.Clear;
begin
  FillChar(Self.bytes[0], APPKEY_LEN, 0);
end;

class operator TAppKey.Implicit(const a: string): TAppKey;
var
  i: integer;
begin
  if a.Length <> 2 * Length(result.bytes) then
    raise Exception.Create('Invalid APPKEY string length');

  for i := low(result.bytes) to high(result.bytes) do
    result.bytes[i] := ('$' + a.Substring(i * 2, 2)).ToInteger;
end;

class operator TAppKey.Implicit(const a: TAppKey): string;
var
  b: byte;
begin
  result := '';
  for b in a.bytes do
    result := result + IntToHex(b, 2);
end;

function TAppKey.IsNull: boolean;
begin
  result := Self.u64 = 0;
end;

end.
