{ Unit sdMetadataTiff

  This unit is able to extract TIFF information from a stream and export it
  in XML. TIFF information can be extracted from any stream, provided it
  adopts to the TIFF specification.

  Currently, only EXIF embedded info is known to exist inside TIFF.

  Author: Nils Haeck M.Sc.
  Copyright (c) 2000 - 2005 by SimDesign B.V.

  Partly based on (and many thanks to):
  unit dEXIF - Copyright 2001, Gerry McGuire

  Version: 1.0
  Date:    08 Mar 2002

}
unit sdMetadataTiff;

interface

uses

  Classes, SysUtils, NativeXml, Math, sdMetadata, sdMetadataExif;

type

  // A TsdMetadataTiff decodes a TIFF file and extracts (recursively) any EXIF, CIFF
  // and IPTC information
  TsdMetadataTiff = class(TsdMetadataExif)
  public
    // Call TagToXML to extract JPG information from Stream and export it
    // in XML. XML must be a valid initialized TXMLObject (See XML unit).
    procedure MetadataToXml(Stream: TStream; Xml: TXmlNode); override;
    // Call IsValidSection to determine quickly if the Stream contains valid JPG
    // information
    class function IsValidSection(Stream: TStream): boolean; override;
  end;

implementation

{ TTiffDecode }

type
  TTIFFHeader = packed record
    ByteOrder: Word;
    Version: Word;
  end;

const
  TIFF_BIGENDIAN    = $4D4D;
  TIFF_LITTLEENDIAN = $4949;
  TIFF_VERSION = 42;

procedure TsdMetadataTiff.MetadataToXml(Stream: TStream; Xml: TXmlNode);
var
  Version: word;
  M: TStream;
begin
  inherited;
  // Check
  if FStream.Size >= 4 then
  begin
    FOrder := Get2;
    if (FOrder = TIFF_BIGENDIAN) or
       (FOrder = TIFF_LITTLEENDIAN) then
    begin
      Version := Get2;
      if (Version = TIFF_VERSION) then
      begin
        Get4;
        IsTiff := True;

        // Parse
        Parse(8, 0, FStream.Size, cExifTag);

        // MakerNote
        if (FMNLength > 0) and (length(FMake) > 0) then
        begin
          // Create a copy of the stream
          M := TMemoryStream.Create;
          try
            FStream.Position := FMNOffset;
            M.CopyFrom(FStream, FMNLength);
            FStream := M; // Point to it again
            ReadHSData;
          finally
            M.Free;
          end;
        end;

      end;
    end;
  end;

end;

class function TsdMetadataTiff.IsValidSection(Stream: TStream): boolean;
var
  TiffHeader: TTiffHeader;
begin
  Result := False;
  Stream.Seek(0, soFromBeginning);
  if Stream.Size >= SizeOf(TiffHeader) then
  begin
    Stream.Read(TiffHeader, SizeOf(TiffHeader));
    if (TiffHeader.ByteOrder = TIFF_BIGENDIAN) or
       (TiffHeader.ByteOrder = TIFF_LITTLEENDIAN) then
    begin
      if TiffHeader.ByteOrder = TIFF_BIGENDIAN then
        TiffHeader.Version := Swap(TiffHeader.Version);
      if (TiffHeader.Version = TIFF_VERSION) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
end;

end.
