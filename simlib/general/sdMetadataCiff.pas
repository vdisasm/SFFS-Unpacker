{ unit sdMetadataCiff

  This unit is able to extract CIFF information from a stream and export it
  in XML. XML is especially helpful here because it allows the directory
  structure found in CIFF to be replicated.

  CIFF information is currently found in Canon CRW and JPG files.
  This work is based on CIFF spec 1.0 rev 4 (Dec24 1997)

  Author: Nils Haeck M.Sc.
  Copyright (c) 2000 - 2005 by SimDesign B.V.

  Partly based on (and many thanks to):
  Canon PowerShot Converter
  Copyright 1997-2002 by Dave Coffin <dcoffin@shore.net>

  CIFF information can be found here:
  http://www.ciff-forum.com/

  Version: 1.0
  Date:    07 Mar 2002

}
unit sdMetadataCiff;

interface

uses
  Classes, SysUtils, NativeXml, Math, sdMetadata;

type

  // TsdMetadataCiff implements the CIFF decoder.
  TsdMetadataCiff = class(TsdMetadata)
  private
    FOffset: longword;
    FVersion: longword;            // Version - must be $0001$0002 (major/minor)
    FSubType: array[0..3] of char; // Subtype of CIFF structure
    FStg,
    FDataType,
    FIDCode: word;
    FRecLength,
    FRecOffset: longword;
    FThumbStream: TStream;
    procedure Parse(AOffset, ALength: longword);
    procedure ReadProp(Datatype: word);
  public
    // Assign a valid stream to this to obtain a copy of the JPG thumbnail
    property ThumbStream: TStream read FThumbStream write FThumbStream;
    // Call MetadataToXML to extract CIFF information from Stream and export it
    // in XML. XML must be a valid initialized TXMLObject (See XML unit).
    procedure MetadataToXml(Stream: TStream; Xml: TXmlNode); override;
    // Call IsValidSection to determine quickly if the Stream contains valid CIFF
    // information
    class function IsValidSection(Stream: TStream): boolean; override;
  end;

const
  cDateTimeFmt = 'YYYY.MM.DD HH:NN';

implementation

const

  // Data types

  kDT_BYTE  = 0;
  kDT_ASCII = 1;
  kDT_WORD  = 2;
  kDT_DWORD = 3;
  kDT_BYTE2 = 4;

  // Property IDs

  kTC_Description              = $0005;
  kTC_ModelName                = $000a;
  kTC_FirmwareVersion          = $000b;
  kTC_ComponentVersion         = $000c;
  kTC_ROMOperationMode         = $000d;
  kTC_OwnerName                = $0010;
  kTC_ImageFileName            = $0016;
  kTC_ThumbnailFileName        = $0017;

  kTC_TargetImageType          = $000a;
  kTC_SR_ReleaseMethod         = $0010;
  kTC_SR_ReleaseTiming         = $0011;
  kTC_ReleaseSetting           = $0016;
  kTC_BodySensitivity          = $001c;

  kTC_ImageFormat              = $0003;
  kTC_RecordID                 = $0004;
  kTC_SelfTimerTime            = $0006;
  kTC_SR_TargetDistanceSetting = $0007;
  kTC_BodyID                   = $000b;
  kTC_CapturedTime             = $000e;
  kTC_ImageSpec                = $0010;
  kTC_SR_EF                    = $0013;
  kTC_MI_EV                    = $0014;
  kTC_SerialNumber             = $0017;
  kTC_SR_Exposure              = $0018;

  kTC_Thumbnail                = $0007;
  kTC_DecodeTable              = $0035;

  kTC_ShootingRecord           = $0002;
  kTC_MeasuredInfo             = $0003;
  kTC_CameraSpecification      = $0004;
  kTC_CameraObject             = $0007;
  kTC_ImageProps               = $000a;
  kTC_Decoder                  = $000b;

{ TsdMetadataCiff}

// Read Heap properties
procedure TsdMetadataCiff.ReadProp(Datatype: word);
var
  i: integer;
  ALong, ALong2: longword;
  AInt: integer;
  ADouble: double;
  AWord: word;
  Value: string;
  Child: TXmlNode;
begin
  try
    Value := '';
    case DataType of

    kDT_BYTE:
  {    case FIDCode of
      else}
        if Verbose then
          FXml.WriteString(Format('Field%d', [FIDCode]), Format('<binary length %d>', [FRecLength]));
  {    end;}

    kDT_ASCII:
      case FIDCode of
      kTC_Description:
        FXml.WriteString('Description', GetString);
      kTC_ModelName:
        begin
          FXml.WriteString('Manufacturer', GetString);
          FXml.WriteString('ModelName', GetString);
        end;
      kTC_FirmwareVersion:
        FXml.WriteString('FirmwareVersion', GetString);
      kTC_ComponentVersion:
        FXml.WriteString('ComponentVersion', GetString);
      kTC_ROMOperationMode:
        FXml.WriteString('ROM_OperationMode', GetString);
      kTC_OwnerName:
        FXml.WriteString('OwnerName', GetString);
      kTC_ImageFileName:
        FXml.WriteString('ImageFileName', GetString);
      kTC_ThumbnailFileName:
        FXml.WriteString('ThumbnailFileName', GetString);
      else
        if Verbose then
        begin
          SetLength(Value, Min(FRecLength, 255));
          FStream.Read(Value[1], length(Value));
          FXml.WriteString(Format('Field%d', [FIDCode]), Value);
        end;
      end;

    kDT_WORD:
      case FIDCode of
      kTC_TargetImageType:
        begin
          case Get2 of
          0: Value := 'Real-world subject';
          1: Value := 'Written document';
          end;//case
          if length(Value) > 0 then
            FXml.WriteString('TargetImageType', Value);
        end;
      kTC_SR_ReleaseMethod:
        begin
          AWord := Get2;
          case AWord of
          0: Value := 'Single shot';
          1: Value := 'Continuous/successive exposures';
          end;
          if length(Value) > 0 then
            FXml.WriteString('ReleaseMethod', Value);
        end;
      kTC_SR_ReleaseTiming:
        begin
          AWord := Get2;
          case AWord of
          0: Value := 'Priority on shutter';
          1: Value := 'Priority on focus';
          end;
          if length(Value) > 0 then
            FXml.WriteString('ReleaseTiming', Value);
        end;
      kTC_ReleaseSetting:;
      kTC_BodySensitivity:
        begin
          Child := FXml.NodeNew('BodySensitivity');
          Child.WriteString('DefaultISO', IntToStr(Get2));
        end;
      else
        if Verbose then
        begin
          for i := 1 to min(FRecLength div 2, 16) do
            Value := Value + ' ' + IntToHex(Get2, 4);
          FXml.WriteString(Format('Field%d', [FIDCode]), '$'+Value);
        end;
      end;

    kDT_DWORD:
      case FIDCode of
      kTC_ImageFormat:
        begin
          ALong := Get4;
          Child := FXml.NodeNew('ImageFormat');
          case ALong shr 16 of
          1: Value := 'JPEG';
          2: Value := 'Canon CRW format';
          end;//case
          if length(Value)>0 then
            Child.WriteString('FileFormat', Value);
          Value := '';
          case ALong and $FFFF of
          0: Value := 'lossy JPEG';
          1: Value := 'none (CRW)';
          2: Value := 'non-quantization JPEG';
          3: Value := 'toggled';
          end;//case
          if length(Value)>0 then
            Child.WriteString('CompressionType', Value);
          Child.WriteString('TargetCompressionRatio', Format('%3.2f', [GetSingle]));
        end;
      kTC_RecordID:
        FXml.WriteString('RecordID', IntToStr(get4));
      kTC_SelfTimerTime:
        FXml.WriteString('SelfTimerTime', Format('%d ms', [Get4]));
      kTC_SR_TargetDistanceSetting:
        FXml.WriteString('TargetDistanceSetting', Format('%3.1 mm', [GetSingle]));
      kTC_BodyID:
        FXml.WriteString('BodyID', IntToStr(Get4));
      kTC_CapturedTime:
        begin
          ALong := Get4;  // Timecount (seconds since 01jan1970 00:00)
          AInt := Get4;   // Time code (add to timecount)
          ALong2 := Get4; // if bit 31 is 1 then timecode is valid
          if (ALong2 shr 31) = 1 then
            inc(ALong, AInt);
          ADouble := EncodeDate(1970, 1, 1) + (ALong / 86400);
          FXml.WriteString('CapturedTime', FormatDateTime(cDateTimeFmt, ADouble));
        end;
      kTC_ImageSpec:
        begin
          Child := FXML.NodeNew('ImageSpec');
          Child.WriteString('ImageWidth', Format('%d pixels', [Get4]));
          Child.WriteString('ImageHeight', Format('%d pixels', [Get4]));
          Child.WriteString('PixelAspectRatio', Format('%3.2f',[GetSingle]));
          Child.WriteString('RotationAngle', Format('%d deg', [Get4]));
          Child.WriteString('ComponentBitDepth', IntToStr(Get4));
          Child.WriteString('ColorBitDepth', IntToStr(Get4));
          ALong := Get4;
          case ALong and $FF of
          0: Value := 'grayscale';
          1: Value := 'color';
          end;
          if length(Value) > 0 then
            Child.WriteString('ColorBW', Value);
          if ((ALong shr 8) and $FF) = 0 then
            Value := 'Yes'
          else
            Value := 'No';
          Child.WriteString('ColorBW_NeedsPostprocess', Value);
        end;
      kTC_SR_EF:
        begin
          Child := FXML.NodeNew('FlashUse');
          Child.WriteString('GuideNumber', Format('%3.2', [GetSingle]));
          Child.WriteString('Treshold', Format('%3.2', [GetSingle]));
        end;
      kTC_MI_EV:
        FXML.WriteString('Measured_EV_Value', Format('%3.2', [GetSingle]));
      kTC_SerialNumber:
        FXML.WriteString('SerialNumber', Format('%.6d',[get4]));
      kTC_SR_Exposure:
        begin
          Child := FXML.NodeNew('Exposure');
          Child.WriteString('ExposureCompensation', Format('%3.2', [GetSingle]));
          Child.WriteString('TV_Value', Format('%3.2', [GetSingle]));
          Child.WriteString('AV_Value', Format('%3.2', [GetSingle]));
        end;
      kTC_DecodeTable:
        if Verbose then
          FXML.WriteString('DecodeTable', IntToStr(Get4));
      else
        if Verbose then
        begin
          for i := 1 to min(FRecLength div 4, 8) do
            Value := Value + ' ' + IntToHex(Get4, 8);
          FXML.WriteString(Format('Field%d', [FIDCode]), '$'+Value);
        end;
      end;//case

    kDT_BYTE2:
      case FIDCode of
      kTC_Thumbnail:
        begin
          if assigned(FThumbStream) then
            FThumbStream.CopyFrom(FStream, FRecLength);
          if Verbose then
            FXML.WriteString('Thumbnail', 'Present');
        end;

      else
        if Verbose then
        begin
          for i := 1 to min(FRecLength, 32) do
            Value := Value + IntToHex(Get1, 2);
          FXML.WriteString(Format('Field%d', [FIDCode]), '$'+ Value);
        end;
      end;//case
    end;//case
  except
    FXML.WriteString(Format('Field%d', [FIdCode]), '<Error>');
  end;
end;

procedure TsdMetadataCiff.Parse(AOffset, ALength: longword);
var
  i, NumRecords: word;
  Typecode: word;
  Save: longword;
  Parent: TXmlNode;
  SubName: string;
begin
  // Go to OffsetTblOffset
  FStream.Position := AOffset + ALength - 4;
  // Read OffsetTblOffset and go to start of OffsetTbl
  FStream.Position := Get4 + AOffset;
  // Read OffsetTbl
  NumRecords := Get2;
  // Iterate through record headers
  for i := 0 to NumRecords - 1 do
  begin
    TypeCode := Get2;
    Save := FStream.Position;
    FStg := TypeCode shr 14;
    FDataType := (TypeCode shr 11) and $07;
    FIDCode := TypeCode and $03FF;
    if FStg = 01 then
    begin
      // In-Record space
      FRecLength := 8;
      ReadProp(FDataType);
    end else
    begin
      // Subheap
      FRecLength := Get4;
      FRecOffset := Get4;
      if FDataType in [5,6] then
      begin
        if Verbose then
          SubName := Format('Sub%d', [FIdCode])
        else
          SubName := '';
        case FIDCode of
        kTC_CameraObject:        SubName := 'CameraObject';
        kTC_ShootingRecord:      SubName := 'ShootingRecord';
        kTC_MeasuredInfo:        SubName := 'MeasuredInfo';
        kTC_CameraSpecification: SubName := 'CameraSpecification';
        kTC_ImageProps:          SubName := ''; // No sub name to avoid clodding
        kTC_Decoder:             SubName := 'Decoder';
        end;
        if length(SubName) > 0 then
        begin
          Parent := FXML;
          FXML := FXML.NodeNew(SubName);
          Parse(FRecOffset + AOffset, FRecLength);
          FXML := Parent;
        end else
          Parse(FRecOffset + AOffset, FRecLength);
      end else
      begin
        if FStg = 00 then
        begin
          // Heap space
          FStream.Position := AOffset + FRecOffset;
          ReadProp(FDataType);
        end;
      end;
    end;
    FStream.Position := Save + 8;
  end;
end;

procedure TsdMetadataCiff.MetadataToXml(Stream: TStream; Xml: TXmlNode);
var
  FType: array[0..3] of char;
begin
  inherited;
  if FStream.Size >= 26 then
  begin
    FOrder := Get2;
    if ((FOrder = $4949) or (FOrder = $4d4d)) then
    begin
      FOffset := Get4;
      FStream.Read(FType, 4);
      FStream.Read(FSubType, 4);
      // Check Type and Subtype. Type is always 'HEAP', and subtype may vary
      if (FType = 'HEAP') and
         ((FSubType = 'JPGM') or (FSubType = 'CCDR') or
          (FSubType = 'TIFP') or (FSubType = 'ARCH')) then
      begin
        FVersion := Get4;
        if FVersion = $00010002 then
          // Parse
          Parse(FOffset, FStream.Size - integer(FOffset));
      end;
    end;
  end;
end;

class function TsdMetadataCiff.IsValidSection(Stream: TStream): boolean;
var
  MainType: array[0..3] of char;
  SubType: array[0..3] of char;
  ByteOrder: word;
  Offset: longword;
  Version: longword;
begin
  Result := False;
  Stream.Position := 0;
  if Stream.Size >= 26 then
  begin
    Stream.Read(ByteOrder, 2);
    if ((ByteOrder = $4949) or (ByteOrder = $4d4d)) then
    begin
      Stream.Read(Offset, 4);
      Stream.Read(MainType, 4);
      Stream.Read(SubType, 4);
      // Check Type and Subtype. Type is always 'HEAP', and subtype may vary
      if (MainType = 'HEAP') and
         ((SubType = 'JPGM') or (SubType = 'CCDR') or
          (SubType = 'TIFP') or (SubType = 'ARCH')) then
      begin
        Stream.Read(Version, 4);
        if ((Version shr 16) <= 2) and ((Version and $FFFF) <= 2) then
          Result := True;
      end;
    end;
  end;
end;

end.
