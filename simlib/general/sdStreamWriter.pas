{ unit sdStreamWriter

  copyright (c) 2008 Nils Haeck M.Sc.
  
  creation date: 7aug2008
  
  description:
  - write streams with (indented) strings
  - function sdWriteNumber (writing numeric info out to string)
  
  depending projects:
  VRML storage
  
  hint: this functionality is mostly also available in:
  - NativeXmlUtils
  - sdStringUtils  
}
unit sdStreamWriter;

interface

uses
  Classes, SysUtils;

type

  TsdStreamWriter = class
  private
    FStream: TStream;
    FIndentString: string;
    FIndent: integer;
  public
    constructor Create; virtual;
    procedure Write(S: string);
    procedure WriteLn(S: string);
    procedure WriteIndent(S: string);
    procedure WriteIndentLn(S: string);
    procedure IncIndent;
    procedure DecIndent;
    property Stream: TStream read FStream write FStream;
    property IndentString: string read FIndentString write FIndentString;
    property Indent: integer read FIndent write FIndent;
  end;

function sdWriteNumber(Value: double; SignificantDigits: integer; AllowScientific: boolean): string;

resourcestring

  sxeSignificantDigitsOutOfRange = 'Significant digits out of range';
  
implementation

function sdWriteNumber(Value: double; SignificantDigits: integer; AllowScientific: boolean): string;
const
  Limits: array[1..9] of integer =
    (10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
var
  Limit, Limitd, PointPos, IntVal, ScPower: integer;
  Body: string;
begin
  if (SignificantDigits < 1) or (SignificantDigits > 9) then
    raise Exception.Create(sxeSignificantDigitsOutOfRange);

  // Zero
  if Value = 0 then
  begin
    Result := '0';
    exit;
  end;

  // Sign
  if Value < 0 then
  begin
    Result := '-';
    Value := -Value;
  end else
    Result := '';

  // Determine point position
  Limit := Limits[SignificantDigits];
  Limitd := Limit div 10;
  PointPos := SignificantDigits;
  while Value < Limitd do
  begin
    Value := Value * 10;
    dec(PointPos);
  end;
  while Value >= Limit do
  begin
    Value := Value * 0.1;
    inc(PointPos);
  end;

  // Round
  IntVal := round(Value);

  // Exceptional case which happens when the value rounds up to the limit
  if Intval = Limit then
  begin
    IntVal := IntVal div 10;
    inc(PointPos);
  end;

  // Strip off any zeros, these reduce significance count
  while (IntVal mod 10 = 0) and (PointPos < SignificantDigits) do
  begin
    dec(SignificantDigits);
    IntVal := IntVal div 10;
  end;

  // Check for scientific notation
  ScPower := 0;
  if AllowScientific and ((PointPos < -1) or (PointPos > SignificantDigits + 2)) then
  begin
    ScPower := PointPos - 1;
    dec(PointPos, ScPower);
  end;

  // Body
  Body := IntToStr(IntVal);
  while PointPos > SignificantDigits do
  begin
    Body := Body + '0';
    inc(SignificantDigits);
  end;
  while PointPos < 0 do
  begin
    Body := '0' + Body;
    inc(PointPos);
  end;
  if PointPos = 0 then
    Body := '0.' + Body
  else
    if PointPos < SignificantDigits then
      Body := copy(Body, 1, PointPos) + '.' + copy(Body, PointPos + 1, SignificantDigits);

  // Final result
  if ScPower = 0 then
    Result := Result + Body
  else
    Result := Result + Body + 'E' + IntToStr(ScPower);
end;

{ TsdStreamWriter }

constructor TsdStreamWriter.Create;
begin
  inherited;
  FIndentString := '  ';
end;

procedure TsdStreamWriter.DecIndent;
begin
  dec(FIndent);
end;

procedure TsdStreamWriter.IncIndent;
begin
  inc(FIndent);
end;

procedure TsdStreamWriter.Write(S: string);
begin
  if length(S) > 0 then
    FStream.Write(S[1], length(S));
end;

procedure TsdStreamWriter.WriteIndent(S: string);
var
  i: integer;
begin
  for i := 0 to Indent - 1 do
    S := FIndentString + S;
  if length(S) > 0 then
    FStream.Write(S[1], length(S));
end;

procedure TsdStreamWriter.WriteIndentLn(S: string);
var
  i: integer;
begin
  for i := 0 to Indent - 1 do
    S := FIndentString + S;
  S := S + #13#10;
  FStream.Write(S[1], length(S))
end;

procedure TsdStreamWriter.WriteLn(S: string);
begin
  S := S + #13#10;
  FStream.Write(S[1], length(S))
end;

end.
