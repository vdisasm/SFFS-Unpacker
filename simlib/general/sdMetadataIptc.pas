{ Unit sdMetadataIptc

  This unit is able to extract IPTC-NAA information from a stream and export it
  in XML. IPTC information can be extracted from any stream, provided it
  adopts to the IPTC-NAA specification.
  
  IPTC stands for "International Press Telecommunications Council"
  and NAA stands for "Newspaper Association of America".

  The IPTC format is mainly found in Photoshop images.

  Use this module together with JPG modules to extract embedded information.

  Author: Nils Haeck M.Sc.
  Copyright (c) 2000 - 2005 by SimDesign B.V.

  Partly based on (and many thanks to):
  unit dIPTC - Copyright 2001, Gerry McGuire

  Version: 1.0
  Date:    08 Mar 2002

}
unit sdMetadataIptc;

{$i simdesign.inc}

interface

uses
  Classes, SysUtils, NativeXml, Math, sdMetadata, sdDebug;

{$ifndef UNICODE}
type RawByteString = AnsiString;
{$endif}

type

  TIptcTag = record
    Code: word;
    Tag:  word;
    Name: Utf8String;
    Desc: Utf8String;
    Size: word;
    Data: RawByteString;
  end;

  // A TsdMetadataIptc decodes a IPTC section
  TsdMetadataIptc = class(TsdMetadata)
  private
    FName: Utf8String;
    FValue: Utf8String;
    function ExtractTag: integer;
  public
    // Call TagToXML to extract information from Stream and export it
    // in XML. XML must be a valid initialized TXMLObject (See XML unit).
    procedure MetadataToXml(Stream: TStream; Xml: TXmlNode); override;
    // Call IsValidSection to determine quickly if the Stream contains valid IPTC
    // information
    class function IsValidSection(Stream: TStream): boolean; override;
  end;

implementation

{ TIptcDecode }

const

  cIptcTagCnt = 32;

  cIPTCTable: array [0 .. cIptcTagCnt - 1] of TIptcTag =
    (( Code: 2; Tag:  0; Name:'SKIP';         Desc:'Record Version';  Size:64),
     ( Code: 2; Tag:  5; Name:'ObjectName';   Desc:'Object name';  Size:64),
     ( Code: 2; Tag: 10; Name:'Urgency';      Desc:'Urgency';      Size:1),
     ( Code: 2; Tag: 12; Name:'SubRef';       Desc:'Subject Reference';     Size:236),
     ( Code: 2; Tag: 15; Name:'Category';     Desc:'Category';     Size:3),
     ( Code: 2; Tag: 20; Name:'SuppCategory'; Desc:'Supplemental category'; Size:32),
     ( Code: 2; Tag: 22; Name:'FixtureID';    Desc:'Fixture ID';   Size:32),
     ( Code: 2; Tag: 25; Name:'Keywords';     Desc:'Keywords';     Size:32),
     ( Code: 2; Tag: 40; Name:'SpecialInstru'; Desc:'Special Instructions'; Size:256),
     ( Code: 2; Tag: 55; Name:'DateCreated';  Desc:'Date created'; Size:8),
     ( Code: 2; Tag: 60; Name:'TimeCreated';  Desc:'Time created'; Size:11),
     ( Code: 2; Tag: 65; Name:'OriginatingProgram';
                        Desc:'Originating Program'; Size:   32),
     ( Code: 2; Tag: 70; Name:'ProgramVersion'; Desc:'Program version'; Size:   10),
     ( Code: 2; Tag: 80; Name:'ByLine';       Desc:'ByLine';       Size:32),
     ( Code: 2; Tag: 85; Name:'ByLineTitle';  Desc:'ByLine Title'; Size:32),
     ( Code: 2; Tag: 90; Name:'City';         Desc:'City';         Size:32),
     ( Code: 2; Tag: 92; Name:'SubLocation';  Desc:'Sublocation';  Size:32),
     ( Code: 2; Tag: 95; Name:'State';        Desc:'Province/State';  Size:32),
     ( Code: 2; Tag:100; Name:'LocationCode';
                        Desc:'Country/Primary Location Code';       Size: 3),
     ( Code: 2; Tag:101; Name:'LocationName';
                        Desc:'Country/Primary Location Name';       Size:   64),
     ( Code: 2; Tag:103; Name:'TransmissionRef';
                        Desc:'Original Transmission Reference';     Size:   32),
     ( Code: 2; Tag:105; Name:'ImageHeadline';
                        Desc:'Image headline'; Size:256),
     ( Code: 2; Tag:110; Name:'ImageCredit';  Desc:'Image credit';  Size:32),
     ( Code: 2; Tag:115; Name:'Source';       Desc:'Source';        Size:32),
     ( Code: 2; Tag:116; Name:'Copyright';    Desc:'Copyright Notice';  Size:128),
     ( Code: 2; Tag:118; Name:'Contact';      Desc:'Contact';       Size:128),
     ( Code: 2; Tag:120; Name:'ImageCaption'; Desc:'Image caption'; Size:2000),
     ( Code: 2; Tag:122; Name:'ImageCaptionWriter';
                        Desc:'Image caption writer';                Size:32),
     ( Code: 2; Tag:130; Name:'ImageType';    Desc:'Image type';    Size:2 ),
     ( Code: 2; Tag:131; Name:'Orientation';  Desc:'Image Orientation';  Size:1 ),
     ( Code: 2; Tag:135; Name:'LangID';       Desc:'Language ID';   Size:2 ),
     ( Code: 8; Tag:10;  Name:'Subfile';      Desc:'Subfile';       Size:2 )
    );

function TsdMetadataIptc.ExtractTag: integer;
var
  i: integer;
  Code, Tag: byte;
  BLen: word;
  Save: longword;
begin
  Result := 0;
  FName := '';
  FValue := '';
  Code := Get1;
  Tag := Get1;
  BLen := Get2;
  Save := FStream.Position;
  if Code in [$02, $08] then
  begin
    // Search the table
    for i := 0 to cIptcTagCnt - 1 do
      // Find the matching table entry
      if (cIptcTable[i].Tag  = Tag) and
         (cIptcTable[i].Code = Code) then
      begin
        if cIptcTable[i].Name <> 'SKIP' then
        begin
          Result := Tag;
          FName := cIptcTable[i].Name;
          SetLength(FValue, BLen);
          FStream.Read(FValue[1], BLen);
        end;
        break;
      end;
  end;
  FStream.Seek(Save + BLen + 1, soFromBeginning);
end;

procedure TsdMetadataIptc.MetadataToXml(Stream: TStream; Xml: TXmlNode);
var
  Head: array[0..13] of AnsiChar;
  Tag: byte;
  Child: TXmlNode;
begin
  inherited;
  FOrder := $4d4d; // IPTC is "Big Endian"
  Stream.Read(Head, 14);
  while FStream.Position < FStream.Size do
    if Get1 = $1C then
      if Get1 in [$02, $08] then
        break;

  FStream.Seek(-1, soFromCurrent);
  while FStream.Position + 4 < FStream.Size do
  begin
    Tag := ExtractTag;
    // Multi-node tags
    if Tag in [20, 25] then
    begin
      Child := FXml.NodeByName(FName);
      if not assigned(Child) then
        Child := FXML.NodeNew(FName);
      Child.WriteString('Item', FValue)
    end else
    begin
      if Tag > 0 then
        FXml.WriteString(FName, FValue);
    end;
  end;
end;

class function TsdMetadataIptc.IsValidSection(Stream: TStream): boolean;
var
  Head: array[0..12] of AnsiChar;
begin
  Result := False;
  Stream.Position := 0;
  if Stream.Size >= 25 then
  begin
    Stream.Read(Head, 13);
    if Head = 'Photoshop 3.0' then
      Result := True;
  end;
end;

end.
