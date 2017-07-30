{ unit sdMetadata

  This unit supports extraction of all kinds of tags from files.
  These comprise of
  - EXIF and IPTC information from JPG and TIFF files
  - CIFF information in Canon CRW and JPG files
  - Marker information from JPG files
  - Comments from GIF files
  - ID3 marker information in MP3 files

  To do:
  Editing support is not yet implemented

  This unit uses:
  - sdMetadataExif: Read EXIF info
  - sdMetadataIptc: Read and write IPTC information
  - sdMetadataCiff: Read CIFF info
  - sdMetadataJpg: Parse JPG files
  - sdMetadataTiff: Parse TIFF files
  - NativeXml: Handling of XML

  Project(s): ABC-View Manager, NativeJpg

  Author: Nils Haeck M.Sc.
  Copyright (c) 2000 - 2011 by SimDesign B.V.

  It is NOT allowed to publish or copy this software without express permission
  of the author!

}
unit sdMetadata;

{$i simdesign.inc}

// Clear any of these defines to limit support
{$DEFINE SupportEXIF}   // Support the EXIF tags (Digital cameras)
{$DEFINE SupportJPG}    // Support the parsing of JPG files and embedded JPG comments
{$DEFINE SupportTIFF}   // Support the parsing of TIFF files
{$DEFINE SupportIPTC}   // Support the IPTC tags (General)
{$DEFINE SupportCIFF}   // Support the CIFF tags (Canon cameras mostly)
{$DEFINE SupportID3}    // Support the ID3 tags (MP3 music files)

interface

uses
  SysUtils, Classes, NativeXml, sdDebug;

type
  // Different file types that may contain metadata
  TsdMetadataFileType = (mftUnknown, mftJPG, mftGIF, mftTIFF, mftCRW, mftMPG);

  // TsdMetadata the base class for metadata decoders
  TsdMetadata = class(TDebugObject)
  private
    FVerbose: boolean;             // Detailed comments
  protected
    FOrder: word;                  // Byte order "MM" or "II"
    FStream: TStream;              // Pointer to the current stream
    FXML: TXmlNode;                // Pointer to the XML object
    function Get1: byte;           // Get 1 byte from the stream
    function Get2: word;           // Get word from the stream, using correct byte order
    function Get4: longword;       // Get longword from the stream, using correct byte order
    function GetSingle: single;    // Get a single (4byte) float from the stream
    function GetDouble: double;    // Get a double (8byte) float from the stream
    function GetString: Utf8String;    // Get a null-terminated string from the stream
    function MakePrintable(Value: Utf8String): Utf8String;
  public
    // Verbose = True will yield fields that are found but not defined
    // in the spec
    property Verbose: boolean read FVerbose write FVerbose;
    // Call IsValidSection to determine quickly if the Stream contains valid
    // tag information. This function must be overridden in desecendants.
    class function IsValidSection(Stream: TStream): boolean; virtual;
    // Call MetadataToXml to extract tag information from Stream and export it
    // in XML. XML must be a valid initialized TXMLObject (See XML unit).
    // This procedure must be overridden in descendants
    procedure MetadataToXml(Stream: TStream; Xml: TXmlNode); virtual;
  end;

  TsdMetadataClass = class of TsdMetadata;

{$IFDEF SupportID3}

  // TsdMetadataID3 will extract ID3 information from MP3 files
  TsdMetadataID3 = class(TsdMetadata)
    // Determine quickly if the Stream contains ID3 information.
    class function IsValidSection(Stream: TStream): boolean; override;
    // Read the MP3 Id3 tag, add a XML tag with <ID3> if present
    procedure MetadataToXml(Stream: TStream; Xml: TXmlNode); override;
  end;

{$ENDIF}

type
  TsdMetadataClassArray = array of TsdMetadataClass;

// Detect which are the metadata decoder classes to use with Stream.
function MetadataClassesFromStream(Stream: TStream): TsdMetadataClassArray;

// Read all metadata nodes from the stream in Stream, and put them in the XML object.
// Use Verbose = True to get detailed tag information. The function returns
// True if any format was recognised
function sdReadMetadata(Stream: TStream; AbsPos: integer; Xml: TXmlNode; Verbose: boolean; Debug: TsdDebugEvent = nil): boolean;

const
  cDateTimeFmt = 'YYYY.MM.DD HH:NN';

implementation

uses

{$IFDEF SupportCIFF}
  sdMetadataCiff,
{$ENDIF}
{$IFDEF SupportEXIF}
  sdMetadataExif,
{$ENDIF}
{$IFDEF SupportIPTC}
  sdMetadataIptc,
{$ENDIF}
{$IFDEF SupportJPG}
  sdMetadataJpg,
{$ENDIF}
{$IFDEF SupportTIFF}
  sdMetadataTiff,
{$ENDIF}
{$IFDEF SupportGIF}
  sdGifTags,
{$ENDIF}
  Math;

{ TsdMetadata }

function TsdMetadata.Get1: byte;
begin
  FStream.Read(Result, 1);
end;

function TsdMetadata.Get2: word;
var
  B: packed array[0..1] of byte;
begin
  FStream.Read(B, 2);
  if FOrder = $4d4d then // "MM" means big-endian
    result := (B[0] shl 8) + B[1]
  else
    result := (B[1] shl 8) + B[0];
end;

function TsdMetadata.Get4: longword;
var
  B: packed array[0..3] of byte;
begin
  FStream.Read(B, 4);
  if FOrder = $4d4d then
    result := (B[0] shl 24) + (B[1] shl 16) + (B[2] shl 8) + B[3]
  else
    result := (B[3] shl 24) + (B[2] shl 16) + (B[1] shl 8) + B[0];
end;

function TsdMetadata.GetSingle: single;
begin
  // endian issues??
  FStream.Read(Result, 4);
end;

function TsdMetadata.GetDouble: double;
begin
  // endian issues??
  FStream.Read(Result, 8);
end;

function TsdMetadata.GetString: Utf8String;
var
  c: AnsiChar;
begin
  Result := '';
  FStream.Read(c, 1);
  while ord(c) > 0 do
  begin
    Result := Result + c;
    FStream.Read(c, 1);
  end;
end;

function TsdMetadata.MakePrintable(Value: Utf8String): Utf8String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to min(length(Value), 100) do
    if not (ord(Value[i]) in [32..255]) then
      Result := Result + '.'
    else
      Result := Result + Value[i];
end;

class function TsdMetadata.IsValidSection(Stream: TStream): boolean;
begin
  // Default returns True, should be overridden
  Result := True;
end;

procedure TsdMetadata.MetadataToXml(Stream: TStream; Xml: TXmlNode);
begin
  // Base class does nothing much except initialize
  FStream := Stream;
  FStream.Position := 0;
  FXML := XML;
end;

{$IFDEF SupportID3}
{ TsdMetadataID3 }

type

  TId3Tag = packed record
    Tag:     array[0..2] of AnsiChar;
    Title:   array[0..29] of AnsiChar;
    Artist:  array[0..29] of AnsiChar;
    Album:   array[0..29] of AnsiChar;
    Year:    array[0..3] of AnsiChar;
    Comment: array[0..29] of AnsiChar;
    GenreId: byte;
  end;

const

  cMp3Genre: array[0..79] of Utf8String =
   ('Blues', 'Classic Rock', 'Country', 'Dance', 'Disco',
    'Funk', 'Grunge', 'Hip-Hop', 'Jazz', 'Metal', 'New Age',
    'Oldies', 'Other', 'Pop', 'R&B', 'Rap', 'Reggae', 'Rock',
    'Techno', 'Industrial', 'Alternative', 'Ska', 'Death Metal' ,
    'Pranks', 'Soundtrack', 'Euro-Techno', 'Ambient', 'Trip-Hop',
    'Vocal', 'Jazz+Funk', 'Fusion', 'Trance', 'Classical',
    'Instrumental', 'Acid', 'House', 'Game', 'Sound Clip',
    'Gospel',  'Noise', 'AlternRock', 'Bass', 'Soul', 'Punk',
    'Space', 'Meditative', 'Instrumental Pop', 'Instrumental Rock',
    'Ethnic', 'Gothic', 'Darkwave', 'Techno-Industrial', 'Electronic',
    'Pop-Folk', 'Eurodance', 'Dream', 'Southern Rock', 'Comedy',
    'Cult', 'Gangsta', 'Top 40', 'Christian Rap', 'Pop/Funk',
    'Jungle', 'Native American', 'Cabaret', 'New Wave', 'Psychadelic',
    'Rave', 'Showtunes', 'Trailer', 'Lo-Fi', 'Tribal', 'Acid Punk',
    'Acid Jazz', 'Polka', 'Retro', 'Musical', 'Rock & Roll',
    'Hard Rock');

class function TsdMetadataID3.IsValidSection(Stream: TStream): boolean;
var
  MP3: word;
  Tag: array[0..2] of char;
begin
  Result := False;
  Stream.Seek(0, soFromBeginning);
  if Stream.Size >= 2 then
  begin
    Stream.Read(MP3, 2);
    MP3 := swap(MP3);
    // Check the framesync (11 bits set)
    if (MP3 and $FFE0) = $FFE0 then
    begin
      // Check TAG
      Stream.Seek(-128, soFromEnd);
      Stream.Read(Tag, 3);
      if Tag = 'TAG' then
        Result := True;
    end;
  end;
end;

// Read the MP3 Id3 tag, add a XML tag with <ID3> if present
procedure TsdMetadataID3.MetadataToXml(Stream: TStream; Xml: TXmlNode);
var
  Id3Tag: TId3Tag;
  Genre: Utf8String;
begin
  Stream.Seek(-128, soFromEnd);
  Stream.Read(Id3Tag, SizeOf(Id3tag));
  if (Id3Tag.Tag = 'TAG') and assigned(XML) then
  begin
    XML.WriteString('Title', trim(ID3Tag.Title));
    XML.WriteString('Artist', trim(ID3Tag.Artist));
    XML.WriteString('Album', trim(ID3Tag.Album));
    XML.WriteString('Year', trim(ID3Tag.Year));
    XML.WriteString('Comment', trim(ID3Tag.Comment));
    Genre := 'Unknown';
    if ID3Tag.GenreId <= 79 then
      Genre := cMp3Genre[ID3Tag.GenreID];
    XML.WriteString('Genre', Genre);
  end;
end;
{$ENDIF} //SupportID3

// General functions and procedures

function MetadataClassesFromStream(Stream: TStream): TsdMetadataClassArray;
begin
  // Try each metadata class
  SetLength(Result, 0);

{$IFDEF SupportJPG}
  if TsdMetadataJpg.IsValidSection(Stream) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TsdMetadataJpg;
  end;
{$ENDIF}

{$IFDEF SupportTIFF}
  if TsdMetadataTiff.IsValidSection(Stream) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TsdMetadataTiff;
  end;
{$ENDIF}

{$IFDEF SupportEXIF}
  if TsdMetadataExif.IsValidSection(Stream) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TsdMetadataExif;
  end;
{$ENDIF}

{$IFDEF SupportCIFF}
  if TsdMetadataCiff.IsValidSection(Stream) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TsdMetadataCiff;
  end;
{$ENDIF}

{$IFDEF SupportIPTC}
  if TsdMetadataIptc.IsValidSection(Stream) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TsdMetadataIptc;
  end;
{$ENDIF}

{$IFDEF SupportID3}
  if TsdMetadataID3.IsValidSection(Stream) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := TsdMetadataId3;
  end;
{$ENDIF}
end;

function sdReadMetadata(Stream: TStream; AbsPos: integer; Xml: TXmlNode; Verbose: boolean; Debug: TsdDebugEvent): boolean;
var
  MetadataClass: TsdMetadataClass;
  MetadataClasses: TsdMetadataClassArray;
  Decoder: TsdMetadata;
  Child: TXmlNode;
  i: integer;
begin
  Result := False;
  // Find the metadata classes
  MetadataClasses := MetadataClassesFromStream(Stream);
  for i := 0 to High(MetadataClasses) do
  begin
    MetadataClass := MetadataClasses[i];
    Child := XML;
{$IFDEF SupportEXIF}
    if (MetadataClass = TsdMetadataExif) or (MetadataClass = TsdMetadataTiff) then
    begin
      Child := XML.NodeNew('EXIF');
      // Store the absolute position in the original file
      Child.AttributeAdd('SPOS', IntToHex(AbsPos, 8));
    end;
{$ENDIF}
{$IFDEF SupportCIFF}
    if MetadataClass = TsdMetadataCiff then
      Child := XML.NodeNew('CIFF');
{$ENDIF}
{$IFDEF SupportIPTC}
    if MetadataClass = TsdMetadataIptc then
      Child := XML.NodeNew('IPTC');
{$ENDIF}
{$IFDEF SupportID3}
    if MetadataClass = TsdMetadataId3 then
      Child := XML.NodeNew('ID3');
{$ENDIF}
    if assigned(Child) then
    begin
      Decoder := MetadataClass.Create;
      try
        Decoder.Verbose := Verbose;
        Decoder.OnDebugOut := Debug;
        Decoder.MetadataToXml(Stream, Child);
      finally
        Decoder.Free;
      end;
      Result := True;
    end;
  end;
  SetLength(MetadataClasses, 0);
end;

end.
