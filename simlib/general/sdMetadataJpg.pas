{ Unit sdMetadataJpg

  This unit is able to extract JPG information from a stream and export it
  in XML. JPG information can be extracted from any stream, provided it
  adopts to the JPG specification.

  Use this module together with EXIF, CIFF and IPTC modules to extract
  embedded information

  Author: Nils Haeck M.Sc.
  Copyright (c) 2000 - 2005 by SimDesign B.V.

  Partly based on (and many thanks to):
  unit dEXIF - Copyright 2001, Gerry McGuire

  Version: 1.0
  Date:    08 Mar 2002

}
unit sdMetadataJpg;

{$i simdesign.inc}

interface

uses

  Classes, SysUtils, NativeXml, Math, sdMetadata, sdDebug;

type

  // A TsdMetadataJpg decodes a JPG file and extracts (recursively) any EXIF and CIFF
  // information as well as comment blocks
  TsdMetadataJpg = class(TsdMetadata)
  private
    FErrorStr: Utf8String;
  protected
    procedure Parse;
  public
    // Call TagToXML to extract JPG information from Stream and export it
    // in XML. XML must be a valid initialized TXMLObject (See XML unit).
    procedure MetadataToXml(Stream: TStream; Xml: TXmlNode); override;
    // Call IsValidSection to determine quickly if the Stream contains valid JPG
    // information
    class function IsValidSection(Stream: TStream): boolean; override;
  end;

implementation

{ TsdMetadataJpg }

const
//--------------------------------------------------------------------------
// JPEG markers consist of one or more= $FF bytes, followed by a marker
// code byte (which is not an FF).  Here are the marker codes of interest
// in this program.
//--------------------------------------------------------------------------

  M_SOF0 = $C0;            // Start Of Frame N
  M_SOF1 = $C1;            // N indicates which compression process
  M_SOF2 = $C2;            // Only SOF0-SOF2 are now in common use
  M_SOF3 = $C3;
  M_SOF5 = $C5;            // NB: codes C4 and CC are NOT SOF markers
  M_SOF6 = $C6;
  M_SOF7 = $C7;
  M_SOF9 = $C9;
  M_SOF10= $CA;
  M_SOF11= $CB;
  M_SOF13= $CD;
  M_SOF14= $CE;
  M_SOF15= $CF;
  M_SOI  = $D8;            // Start Of Image (beginning of datastream)
  M_EOI  = $D9;            // End Of Image (end of datastream)
  M_SOS  = $DA;            // Start Of Scan (begins compressed data)
  M_JFIF = $E0;            // Jfif marker
  M_EXIF = $E1;            // Exif marker
  M_COM  = $FE;            // Comment
  M_IPTC = $ED;            // IPTC (e.g. Photoshop)

procedure TsdMetadataJpg.Parse;
var
  Save: longword;
  FMarker: byte;
  FRecLength: word;
  // Local
  function RecursiveRead(AXml: TXmlNode): boolean;
  var
    M: TStream;
    APos: integer;
  begin
    M := TMemoryStream.Create;
    try
      // Copy the stream
      APos := FStream.Position;
      M.CopyFrom(FStream, FRecLength);
      // And try to decode it
      Result := sdReadMetadata(M, APos, AXml, Verbose);
    finally
      M.Free;
    end;
  end;
// main
begin
  try
    // Read a marker
    repeat
      // Get marker
      FMarker := Get1;
      if FMarker <> $FF then
      begin
        FErrorStr := 'Illegal marker value';
        exit;
      end;
      while FMarker = $FF do
        FMarker := Get1;
      Save := FStream.Position;
      FRecLength := Get2;

      case FMarker of
      // Start Of Scan: no more information sections
      M_SOS: Exit;

      // Skip - nothing to do
      M_EOI, M_JFIF, M_SOF0..M_SOF15: ;

      // Comment section
      M_COM:
        if not RecursiveRead(FXML.NodeNew('JpgComment')) then
          FXML.WriteString('Comment', GetString);

      M_IPTC:
        RecursiveRead(FXML);

      M_EXIF:
        RecursiveRead(FXML);
      else
        // Unknown marker?
        if Verbose and (FRecLength > 0) then
        begin
          RecursiveRead(FXML);
        end;
      end;//case
      FStream.Position := Save + FRecLength;
    until FMarker = M_EOI;
  except
    FErrorStr := 'Premature end of file';
  end;
end;

procedure TsdMetadataJpg.MetadataToXml(Stream: TStream; Xml: TXmlNode);
var
  Marker: array[0..1] of AnsiChar;
begin
  inherited;
  FErrorStr := '';
  FOrder := $4d4d; // JPG is Big Endian

  // Parse the file
  if FStream.Size >= 4 then
  begin
    FStream.Read(Marker, 2);
    if Marker = #$FF#$D8 then
      Parse;
  end;

end;

class function TsdMetadataJpg.IsValidSection(Stream: TStream): boolean;
var
  Marker: array[0..1] of AnsiChar;
begin
  Result := False;
  Stream.Seek(0, soFromBeginning);
  if Stream.Size >= 4 then
  begin
    Stream.Read(Marker, 2);
    if Marker = #$FF#$D8 then
      Result := True;
  end;
end;

end.
