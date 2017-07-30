unit sdTokenizer;
{
  Generally usable parser that reads and identifies tokens in the parsed file.

  Used in TsdVrmlFormat, the idea is to use this tokenizer in many more projects.

  original author: Nils Haeck M.Sc.
  copyright (c) 2008 by SimDesign BV
}
interface

uses
  Classes, SysUtils, sdSortedLists;

const
  cDefaultBufferLength = 64 * 1024;

type

  TSetOfChar = set of char;

  TsdTokenizer = class;

  TsdToken = class
  private
    FOwner: TsdTokenizer;
    FHash: longword;
    FValue: string;
    FID: integer;
  public
    constructor Create(AOwner: TsdTokenizer);
    property Value: string read FValue;
    property ID: integer read FID;
  end;

  TsdTokenList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TsdToken;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    property Items[Index: integer]: TsdToken read GetItems; default;
  end;

  TsdProgressEvent = procedure(Sender: TObject; APosition, ASize: longint) of object;

  TsdTokenizer = class
  private
    FTokens: TsdTokenList;
    FStream: TStream;
    FCaseSensitive: boolean;
    FFirst: PByte;
    FCurrent: PByte;
    FCount: int64; // remaining bytes
    FSize: int64; // total bytes
    FBuffer: array of char;
    FBufferLength: integer;
    FTerminators: TSetOfChar;
    FLastSymbol: string;
    FLastToken: integer;
    FSingleTokens: TSetOfChar;
    FProgress: double;
    FOnProgress: TsdProgressEvent;
    FUseSingleTokensAsTerminators: boolean;
    FLastSymbolPosition: integer;
    procedure SetCaseSensitive(const Value: boolean);
    procedure SetBufferLength(const Value: integer);
  protected
    function CaseString(const S: string): string;
    function CalculateHash(const S: string): longword;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddToken(const ATokenString: string; ATokenID: integer);
    procedure ProcessStream(S: TStream);
    procedure ProcessMemory(AFirst: PByte; ACount: int64);
    function CurrentPosition: integer;
    function CopySelectionToString(AStart, ACount: integer): string;
    // Read the next string of characters in the stream until any of
    // the characters in the terminators is reached. The string is then
    // compared with the tokens, and if any of them matches, its ID is returned.
    // If not, -1 is returned. LastSymbol will contain the literal string.
    function ReadToken: integer;
    // Read the next string of characters in the stream until any of the
    // characters in the terminators is reached. The stream is then positioned
    // after any of the characters in the terminators set.
    function ReadSymbol: string;
    function ReadSymbolUntil(const ATerminators: TSetOfChar): string;
    function IsEndOfFile: boolean;
    function LastSymbolAsFloat(const ADefault: double): double;
    function LastSymbolAsInt(const ADefault: integer): integer;
    function SymbolToTokenID(const S: string): integer;
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive;
    property BufferLength: integer read FBufferLength write SetBufferLength;
    property Terminators: TSetOfChar read FTerminators write FTerminators;
    property LastSymbol: string read FLastSymbol;
    property LastSymbolPosition: integer read FLastSymbolPosition;
    property LastToken: integer read FLastToken;
    property UseSingleTokensAsTerminators: boolean read FUseSingleTokensAsTerminators write FUseSingleTokensAsTerminators;
    property OnProgress: TsdProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TsdToken }

constructor TsdToken.Create(AOwner: TsdTokenizer);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ tsdTokenList }

function TsdTokenList.DoCompare(Item1, Item2: TObject): integer;
begin
  Result := CompareLongWord(TsdToken(Item1).FHash, TsdToken(Item2).FHash)
end;

function TsdTokenList.GetItems(Index: integer): TsdToken;
begin
  Result := Get(Index);
end;

{ TsdTokenizer }

procedure TsdTokenizer.AddToken(const ATokenString: string; ATokenID: integer);
var
  Token: TsdToken;
begin
  Token := TsdToken.Create(Self);
  Token.FValue := ATokenString;
  Token.FID := ATokenID;
  if FCaseSensitive then
    Token.FHash := CalculateHash(ATokenString)
  else
    Token.FHash := CalculateHash(LowerCase(ATokenString));
  FTokens.Add(Token);
  if (length(ATokenString) = 1) and FUseSingleTokensAsTerminators then
    FSingleTokens := FSingleTokens + [ATokenString[1]];
end;

function TsdTokenizer.CalculateHash(const S: string): longword;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(S) do
    Result := (Result XOR $FFFFFFFF) XOR (Result shl 8) XOR ord(S[i])
end;

function TsdTokenizer.CaseString(const S: string): string;
begin
  if FCaseSensitive then
    Result := S
  else
    Result := LowerCase(S);
end;

procedure TsdTokenizer.Clear;
begin
  FTokens.Clear;
  FSingleTokens := [];
end;

function TsdTokenizer.CopySelectionToString(AStart, ACount: integer): string;
var
  P: int64;
  F: PByte;
begin
  if( ACount < 0) or (AStart < 0) then
    exit;

  SetLength(Result, ACount);
  if assigned(FStream) then
  begin
    P := FStream.Position;
    FStream.Position := AStart;
    FStream.Read(Result[1], ACount);
    FStream.Position := P;
  end else
  begin
    F := FFirst;
    inc(F, AStart);
    Move(F^, Result[1], ACount);
  end;
end;

constructor TsdTokenizer.Create;
begin
  inherited;
  FTokens := TsdTokenList.Create(True);
  // Buffer
  SetBufferLength(cDefaultBufferLength);
  // Defaults
  FCaseSensitive := False;
end;

function TsdTokenizer.CurrentPosition: integer;
begin
  if assigned(FStream) then
    Result := FStream.Position
  else
    Result := integer(FCurrent) - integer(FFirst);
end;

destructor TsdTokenizer.Destroy;
begin
  FreeAndNil(FTokens);
  inherited;
end;

function TsdTokenizer.IsEndOfFile: boolean;
begin
  Result := FCount = 0;
end;

function TsdTokenizer.LastSymbolAsFloat(const ADefault: double): double;
begin
  Result := StrToFloatDef(FLastSymbol, ADefault);
end;

function TsdTokenizer.LastSymbolAsInt(const ADefault: integer): integer;
begin
  Result := StrToIntDef(FLastSymbol, ADefault);
end;

procedure TsdTokenizer.ProcessMemory(AFirst: PByte; ACount: int64);
begin
  FStream := nil;
  FCurrent := AFirst;
  FCount := ACount;
  FSize := FCount;
end;

procedure TsdTokenizer.ProcessStream(S: TStream);
begin
  if S is TMemoryStream then
  begin
    ProcessMemory(
      PByte(integer(TMemoryStream(S).Memory) + S.Position),
      S.Size - S.Position);
    exit;
  end;
  FCurrent := nil;
  FCount := S.Size - S.Position;
  FSize := FCount;
  FStream := S;
end;

function TsdTokenizer.ReadSymbol: string;
begin
  FLastSymbolPosition := CurrentPosition;
  FLastSymbol := ReadSymbolUntil(FTerminators);
  FLastToken := -1;
  Result := FLastSymbol;
end;

function TsdTokenizer.ReadSymbolUntil(const ATerminators: TSetOfChar): string;
var
  Idx: integer;
  Ch: char;
  CurProgress: double;
  // local
  procedure ReadChar;
  begin
    if assigned(FStream) then
    begin
      // Stream handling
      FStream.Read(Ch, 1);
    end else
    begin
      // Memory handling
      Ch := Char(FCurrent^);
      inc(FCurrent);
    end;
    dec(FCount);
  end;
  // local
  procedure MoveBack;
  begin
    if assigned(FStream) then
      FStream.Seek(-1, soFromCurrent)
    else
      dec(FCurrent);
    inc(FCount);
  end;
  // local
  procedure SkipTerminators;
  begin
    // while it is a terminator, go to next char (skip terminators)
    while (Ch in ATerminators) and (FCount > 0) do
      ReadChar;
    // not last char and not a terminator? go back one
    if not ((FCount = 0) and (Ch in ATerminators)) then
      MoveBack;
  end;
// main
begin
  Result := '';
  Idx := 0;
  if FCount = 0 then
    exit;

  while FCount > 0 do
  begin
    ReadChar;
    FBuffer[Idx] := Ch;
    inc(Idx);

    // Special case: single tokens
    if (Idx = 1) and (Ch in FSingleTokens) then
    begin
      inc(Idx);
      ReadChar;
      SkipTerminators;
      break;
    end;

    // new char a terminator, or a single token?
    if (Ch in ATerminators) or (Ch in FSingleTokens) then
    begin
      SkipTerminators;
      break;
    end;
  end;

  // Progress indication
  CurProgress := ((FSize - FCount) / FSize) * 100;
  if abs(CurProgress - FProgress) >= 0.1 then
  begin
    if assigned(FOnProgress) then
      FOnProgress(Self, FSize - FCount, FSize);
    FProgress := CurProgress;
  end;

  // Copy buffer to result string
  SetLength(Result, Idx - 1);
  Move(FBuffer[0], Result[1], Idx - 1);
end;

function TsdTokenizer.ReadToken: integer;
begin
  ReadSymbol;
  Result := SymbolToTokenID(FLastSymbol);
  FLastToken := Result;
end;

procedure TsdTokenizer.SetBufferLength(const Value: integer);
begin
  FBufferLength := Value;
  SetLength(FBuffer, FBufferLength);
end;

procedure TsdTokenizer.SetCaseSensitive(const Value: boolean);
var
  i: integer;
  Token: TsdToken;
begin
  if FCaseSensitive <> Value then
  begin
    // We have to recalculate the hashes and re-sort
    FCaseSensitive := Value;
    for i := 0 to FTokens.Count - 1 do
    begin
      Token := FTokens[i];
      if FCaseSensitive then
        Token.FHash := CalculateHash(Token.FValue)
      else
        Token.FHash := CalculateHash(LowerCase(Token.FValue))
    end;
    FTokens.Sort;
  end;
end;

function TsdTokenizer.SymbolToTokenID(const S: string): integer;
var
  T: TsdToken;
  i, Idx, Count: integer;
begin
  Result := -1;

  T := TsdToken.Create(nil);
  try

    // Get hash
    T.FHash := CalculateHash(CaseString(S));

    // Find token
    FTokens.FindMultiple(T, Idx, Count);

    if Count > 0 then
    begin
      if Count = 1 then
        Result := FTokens[Idx].ID
      else
      begin
        for i := Idx to Idx + Count - 1 do
          if CaseString(FTokens[i].Value) = CaseString(S) then
          begin
            Result := FTokens[i].ID;
            exit;
          end;
      end;
    end;

  finally
    T.Free;
  end;
end;

end.
