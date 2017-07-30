{ Unit sdMetadataExif

  This unit is able to extract EXIF information from a stream and export it
  in XML. EXIF information can be extracted from any stream, provided it
  adopts to the EXIF specification.

  This work is based on the exif standard and files found at:
  http://www.pima.net/standards/iso/tc42/wg18/WG18_POW.htm
  http://www.butaman.ne.jp/~tsuruzoh/Computer/Digicams/exif-e.html

  Author: Nils Haeck M.Sc.
  Copyright (c) 2002 - 2011 by SimDesign B.V.

  Partly based on (and many thanks to):
  unit dEXIF - Copyright 2001, Gerry McGuire

  Version: 1.0
  Date: 08mar2002

  Modifications:
  14jul2011: fixed DXE warning (implicit string conversion with potential loss)

}
unit sdMetadataExif;

interface

uses
  Classes, SysUtils, NativeXml, Math, sdMetadata, sdDebug;

type

  // Record type for Tag Table data
  TsdExifTagTable = record
    Tag: word;
    Desc: Utf8String;
    Code: Utf8String;
    CallBack: integer;
  end;

  // TsdMetadataExif implements the EXIF decoder.
  TsdMetadataExif = class(TsdMetadata)
  private
    FIsTiff: boolean; // Exif belongs to TIFF instead JPG?
    FRecLength: longword;
    function Callback(Index: integer; Value: Utf8String): Utf8String;
    function DecodeField(DecodeStr, Idx: Utf8String): Utf8String;
    function FormatNumber(Fmt: integer; DecodeStr: Utf8String): Utf8String;
    function LookupHSTagID(Idx:integer; TagTbl: array of TsdExifTagTable): integer;
    function LookupTagID(Idx: integer; TagType: integer): integer;
    function LookupHSCode(Idx: integer; TagTbl: array of TsdExifTagTable): Utf8String;
    function LookupCode(Idx: integer; TagType: integer): Utf8String;
  protected
    FMake, FModel: Utf8String;
    FMNOffset, FMNLength: longword;
    // Parse normal EXIF structure
    procedure Parse(ADirStart, AOffset, ALength: longword; TagType: integer);
    // Parse Hardware Specific EXIF structure
    procedure ParseHS(TagTbl: array of TsdExifTagTable; ADirStart, AOffset: longint);
    // Read Hardware Specific data
    procedure ReadHSData;
    // Convert an Exif Date (YYYY:MM:DD HH:NN:SS) to a TDateTime
    function ExifDateToDateTime(ADate: Utf8String): TDateTime;
  public
    property IsTiff: boolean read FIsTiff write FIsTiff;
    // Call TabToXML to extract EXIF information from Stream and export it
    // in XML. XML must be a valid initialized TXMLObject (See XML unit).
    procedure MetadataToXml(Stream: TStream; Xml: TXmlNode); override;
    // Call IsValidSection to determine quickly if the Stream contains valid EXIF
    // information
    class function IsValidSection(Stream: TStream): boolean; override;
  end;

function sdExifDateToDateTime(ExifDate: Utf8String; var DateTime: TDateTime): boolean;

const

  // Tag types
  cExifTag = 1;
  cGpsTag  = 2;

resourcestring
  sNonValidDateString    = 'non-valid date string';
  sIllSizedEXIFDirectory = 'Ill-sized EXIF directory';

implementation

const

  // Number of bytes each format uses
  cBytesPerFormat: array [1..12] of integer = (1,1,2,4,8,1,1,2,4,8,4,8);

  // Format descriptor
  NUM_FORMATS   = 12;
  FMT_BYTE      =  1;
  FMT_STRING    =  2;
  FMT_USHORT    =  3;
  FMT_ULONG     =  4;
  FMT_URATIONAL =  5;
  FMT_SBYTE     =  6;
  FMT_UNDEFINED =  7;
  FMT_SSHORT    =  8;
  FMT_SLONG     =  9;
  FMT_SRATIONAL = 10;
  FMT_SINGLE    = 11;
  FMT_DOUBLE    = 12;

  // Tag values
  TAG_EXIF_OFFSET      = $8769;
  TAG_GPS_OFFSET       = $8825;
  TAG_INTEROP_OFFSET   = $a005;

  TAG_MAKE             = $010F;
  TAG_MODEL            = $0110;
  TAG_DATETIME         = $0132;

  TAG_EXPOSURETIME     = $829A;
  TAG_FNUMBER          = $829D;

  TAG_SHUTTERSPEED     = $9201;
  TAG_APERTURE         = $9202;
  TAG_MAXAPERTURE      = $9205;
  TAG_FOCALLENGTH      = $920A;

  TAG_DATETIME_ORIGINAL= $9003;
  TAG_USERCOMMENT      = $9286;

  TAG_SUBJECT_DISTANCE = $9206;
  TAG_LIGHT_SOURCE     = $9208;
  TAG_FLASH            = $9209;

  TAG_MAKERNOTE        = $927C;

  TAG_FOCALPLANEXRES   = $a20E;
  TAG_FOCALPLANEUNITS  = $a210;
  TAG_EXIF_IMAGEWIDTH  = $A002;
  TAG_EXIF_IMAGELENGTH = $A003;

  TAG_IMAGEWIDTH       = $0100;
  TAG_IMAGELENGTH      = $0101;

  cExifTagCnt = 87;

  cTagTable : array [0..cExifTagCnt - 1] of TsdExifTagTable =
   ((Tag: $001;   Desc:'InteroperabilityIndex'),
    (Tag: $002;   Desc:'InteroperabilityVersion'),
    (Tag: $100;   Desc:'ImageWidth'),
    (Tag: $101;   Desc:'ImageLength'),
    (Tag: $102;   Desc:'BitsPerSample'),
    (Tag: $103;   Desc:'Compression'),
    (Tag: $106;   Desc:'PhotometricInterpretation'),
    (Tag: $10A;   Desc:'FillOrder'),
    (Tag: $10D;   Desc:'DocumentName'),
    (Tag: $10E;   Desc:'ImageDescription'),
    (Tag: $10F;   Desc:'Make'),
    (Tag: $110;   Desc:'Model'),
    (Tag: $111;   Desc:'StripOffsets'),
    (Tag: $112;   Desc:'Orientation'; Code:'1:Normal,2:Flipped Horizontal,3:Rotated 180°,4:Flipped Vertical,6:CounterClockwise 90°,8:Clockwise 90°'),
    (Tag: $115;   Desc:'SamplesPerPixel'),
    (Tag: $116;   Desc:'RowsPerStrip'),
    (Tag: $117;   Desc:'StripByteCounts'),
    (Tag: $11A;   Desc:'XResolution'),
    (Tag: $11B;   Desc:'YResolution'),
    (Tag: $11C;   Desc:'PlanarConfiguration'),
    (Tag: $128;   Desc:'ResolutionUnit'; Code:'1:None Specified,2:Inch,3:Centimeter'),        // ; Code:''
    (Tag: $12D;   Desc:'TransferFunction'),
    (Tag: $131;   Desc:'Software'),
    (Tag: $132;   Desc:'DateTime'),
    (Tag: $13B;   Desc:'Artist'),
    (Tag: $13E;   Desc:'WhitePoint'),
    (Tag: $13F;   Desc:'PrimaryChromaticities'),
    (Tag: $156;   Desc:'TransferRange'),
    (Tag: $200;   Desc:'JPEGProc'),
    (Tag: $201;   Desc:'JPEGInterchangeFormat'),
    (Tag: $202;   Desc:'JPEGInterchangeFormatLength'),
    (Tag: $211;   Desc:'YCbCrCoefficients'),
    (Tag: $212;   Desc:'YCbCrSubSampling'),
    (Tag: $213;   Desc:'YCbCrPositioning'; Code:'1:Centered,2:Cosited'),
    (Tag: $214;   Desc:'ReferenceBlackWhite'),
    (Tag: $1001;  Desc:'RelatedImageWidth'),
    (Tag: $1002;  Desc:'RelatedImageHeight'),
    (Tag: $828D;  Desc:'CFARepeatPatternDim'),
    (Tag: $828E;  Desc:'CFAPattern'),
    (Tag: $828F;  Desc:'BatteryLevel'),
    (Tag: $8298;  Desc:'Copyright'),
    (Tag: $829A;  Desc:'ExposureTime'),
    (Tag: $829D;  Desc:'FNumber'),
    (Tag: $83BB;  Desc:'IPTC/NAA'),
    (Tag: $8769;  Desc:'ExifOffset'),
    (Tag: $8773;  Desc:'InterColorProfile'),
    (Tag: $8822;  Desc:'ExposureProgram'; Code:
          '0:Unidentified,1:Manual,2:Normal,3:Aperture priority,'+
          '4:Shutter priority,5:Creative(slow),'+
          '6:Action(high-speed),7:Portrait mode,8:Landscape mode'),
    (Tag: $8824;  Desc:'SpectralSensitivity'),
    (Tag: $8825;  Desc:'GPSInfo'),
    (Tag: $8827;  Desc:'ISOSpeedRatings'),
    (Tag: $8828;  Desc:'OECF'),
    (Tag: $9000;  Desc:'ExifVersion'),
    (Tag: $9003;  Desc:'DateTimeOriginal'),
    (Tag: $9004;  Desc:'DateTimeDigitized'),
    (Tag: $9101;  Desc:'ComponentsConfiguration'),
    (Tag: $9102;  Desc:'CompressedBitsPerPixel'),
    (Tag: $9201;  Desc:'ShutterSpeedValue'),
    (Tag: $9202;  Desc:'ApertureValue'),
    (Tag: $9203;  Desc:'BrightnessValue'),
    (Tag: $9204;  Desc:'ExposureBiasValue'),
    (Tag: $9205;  Desc:'MaxApertureValue'),
    (Tag: $9206;  Desc:'SubjectDistance'),
    (Tag: $9207;  Desc:'MeteringMode'; Code:'1:Average,4:MultiSpot,5:Multi,2:Center,3:Spot'),
    (Tag: $9208;  Desc:'LightSource'; Code:'0:Unidentified,1:Daylight,2:Fluorescent,3:Tungsten,10:Flash,17:Std A,18:Std B,19:Std C'),
    (Tag: $9209;  Desc:'Flash'; Code:'0:Off,1:On'),
    (Tag: $920A;  Desc:'FocalLength'),
    (Tag: $927C;  Desc:'MakerNote'),
    (Tag: $9286;  Desc:'UserComment'),
    (Tag: $9290;  Desc:'SubSecTime'),
    (Tag: $9291;  Desc:'SubSecTimeOriginal'),
    (Tag: $9292;  Desc:'SubSecTimeDigitized'),
    (Tag: $A000;  Desc:'FlashPixVersion'),
    (Tag: $A001;  Desc:'ColorSpace'; Code:'0:sBW,1:sRGB'),
    (Tag: $A002;  Desc:'ExifImageWidth'),
    (Tag: $A003;  Desc:'ExifImageLength'),
    (Tag: $A005;  Desc:'InteroperabilityOffset'),
    (Tag: $A20B;  Desc:'FlashEnergy'),		    // Tag: $920B in TIFF/EP
    (Tag: $A20C;  Desc:'SpatialFrequencyResponse'),   // Tag: $920C    -  -
    (Tag: $A20E;  Desc:'FocalPlaneXResolution'),      // Tag: $920E    -  -
    (Tag: $A20F;  Desc:'FocalPlaneYResolution'),	    // Tag: $920F    -  -
    (Tag: $A210;  Desc:'FocalPlaneResolutionUnit'; Code:'1:Inch,2:Meter,'+
      '3:Centimeter,4:Millimeter,5:Micrometer'),      // Tag: $9210    -  -
    (Tag: $A214;  Desc:'SubjectLocation'),	    // Tag: $9214    -  -
    (Tag: $A215;  Desc:'ExposureIndex'),		    // Tag: $9215    -  -
    (Tag: $A217;  Desc:'SensingMethod'; Code:'0:Unknown,1:MonochromeArea,'+
      '2:OneChipColorArea,3:TwoChipColorArea,4:ThreeChipColorArea,'+
      '5:ColorSequentialArea,6:MonochromeLinear,7:TriLinear,'+
      '8:ColorSequentialLinear'),	       	           // Tag: $9217    -  -
    (Tag: $A300;  Desc:'FileSource'),
    (Tag: $A301;  Desc:'SceneType'),
    (Tag: 0;      Desc:'' )) ;

  // Callback procedure indices
  cGpsPosn   = 1;
  cCvtTime   = 2;
  cNikonLens = 3;

  // GPS Tag Count
  cGPSCnt = 27;

  // GPS Tags
  cGPSTable : array [0..cGPSCnt - 1] of TsdExifTagTable =
   ((Tag: $000;   Desc:'GPSVersionID'),
    (Tag: $001;   Desc:'GPSLatitudeRef'),
    (Tag: $002;   Desc:'GPSLatitude'; CallBack: cGpsPosn),
    (Tag: $003;   Desc:'GPSLongitudeRef'),
    (Tag: $004;   Desc:'GPSLongitude'; CallBack: cGpsPosn),
    (Tag: $005;   Desc:'GPSAltitudeRef'; Code:'0:Sealevel'),
    (Tag: $006;   Desc:'GPSAltitude'),
    (Tag: $007;   Desc:'GPSTimeStamp'; CallBack: cCvtTime),
    (Tag: $008;   Desc:'GPSSatellites'),
    (Tag: $009;   Desc:'GPSStatus'),
    (Tag: $00A;   Desc:'GPSMeasureMode'),
    (Tag: $00B;   Desc:'GPSDOP'),
    (Tag: $00C;   Desc:'GPSSpeedRef'),
    (Tag: $00D;   Desc:'GPSSpeed'),
    (Tag: $00E;   Desc:'GPSTrackRef'),
    (Tag: $00F;   Desc:'GPSTrack'),
    (Tag: $010;   Desc:'GPSImageDirectionRef'),
    (Tag: $011;   Desc:'GPSImageDirection'),
    (Tag: $012;   Desc:'GPSMapDatum'),
    (Tag: $013;   Desc:'GPSDestLatitudeRef'),
    (Tag: $014;   Desc:'GPSDestLatitude'; CallBack: cGpsPosn),
    (Tag: $015;   Desc:'GPSDestLongitudeRef'),
    (Tag: $016;   Desc:'GPSDestLongitude'; CallBack: cGpsPosn),
    (Tag: $017;   Desc:'GPSDestBearingkRef'),
    (Tag: $018;   Desc:'GPSDestBearing'),
    (Tag: $019;   Desc:'GPSDestDistanceRef'),
    (Tag: $01A;   Desc:'GPSDestDistance'));

const
  Nikon1Table : array [0..10] of TsdExifTagTable =
   ((Tag: $02;    Desc:'FamilyID'),
    (Tag: $03;    Desc:'Quality'; Code:'1:Vga Basic,2:Vga Normal,'+
        '3:Vga Fine,4:SXGA Basic,5:SXGA Normal,6:SXGA Fine'+
        '10:2 Mpixel Basic,11:2 Mpixel Normal,12:2 Mpixel Fine'),
    (Tag: $04;    Desc:'ColorMode'; Code:'1:Color,2:Monochrome'),
    (Tag: $05;    Desc:'ImageAdjustment'; Code:'0:Normal,1:Bright+,'+
        '2:Bright-,3:Contrast+,4:Contrast-.'),
    (Tag: $06;    Desc:'ISOSpeed'; Code:'0:ISO80,2:ISO160,4:ISO320,'+
        '5:ISO100'),
    (Tag: $07;    Desc:'WhiteBalance'; Code:'0:Auto,1:Preset,2:Daylight,'+
        '3:Incandescense,4:Fluorescence,5:Cloudy,6:SpeedLight'),
    (Tag: $08;    Desc:'Focus'),
    (Tag: $09;    Desc:'Skip'),
    (Tag: $0A;    Desc:'DigitalZoom'),
    (Tag: $0B;    Desc:'Converter'; Code:'0:Not used,1:Used'),
    (Tag: $0F00;  Desc:'Skip')) ;

  Nikon2Table : array [0..18] of TsdExifTagTable =
   ((Tag: $01;    Desc:'FamilyID'),
    (Tag: $02;    Desc:'ISOSpeed'),
    (Tag: $03;    Desc:'ColorMode'),
    (Tag: $04;    Desc:'Quality'),
    (Tag: $05;    Desc:'WhiteBalance'),
    (Tag: $06;    Desc:'ImageSharpening'),
    (Tag: $07;    Desc:'FocusMode'),
    (Tag: $08;    Desc:'FlashSetting'),
    (Tag: $0A;    Desc:'Skip'),
    (Tag: $0F;    Desc:'DigitalZoom'),
    (Tag: $10;    Desc:'Skip'),
    (Tag: $80;    Desc:'ImageAdjustment'),
    (Tag: $82;    Desc:'Adapter'),
    (Tag: $84;    Desc:'Lens information'; CallBack: cNikonLens),
    (Tag: $85;    Desc:'ManualFocusDistance'),
    (Tag: $86;    Desc:'DigitalZoom'),
    (Tag: $88;    Desc:'AFFocusPosition'),
    (Tag: $89;    Desc:'Mode'),
    (Tag: $0F00;  Desc:'Skip')) ;

  Olympus1Table : array [0..10] of TsdExifTagTable =
   ((Tag: $0200;    Desc:'SpecialMode'),
    (Tag: $0201;    Desc:'JpegQual'; Code:'1:SQ,2:HQ,3:SHQ'),
    (Tag: $0202;    Desc:'Macro';    Code:'0=Normal,1:Macro;'),
    (Tag: $0203;    Desc:'Skip'),
    (Tag: $0204;    Desc:'DigiZoom'),
    (Tag: $0205;    Desc:'Skip'),
    (Tag: $0206;    Desc:'Skip'),
    (Tag: $0207;    Desc:'SoftwareRelease'),
    (Tag: $0208;    Desc:'PictInfo'),
    (Tag: $0209;    Desc:'CameraID'),
    (Tag: $0F00;    Desc:'Skip'));

  Casio1Table : array [0..25] of TsdExifTagTable =
   ((Tag: $01;   Desc:'RecordingMode'; Code:'1:Single Shutter,2:Panorama,'+
        '3:Night Scene,4:Portrait,5:Landscape'),
    (Tag: $02;   Desc:'Quality'; Code:'1:Economy,2:Normal,3:Fine'),
    (Tag: $03;   Desc:'FocusingMode'; Code:'2:Macro,3:Auto Focus,'+
        '4:Manual Focus,5:Infinity'),
    (Tag: $04;   Desc:'FlashMode'; Code:'1:Auto,2:On,3:Off,'+
        '4:Red Eye Reduction'),
    (Tag: $05;   Desc:'FlashIntensity'; Code:'11:Weak,13:Normal,15:Strong'),
    (Tag: $06;   Desc:'ObjectDistance'),
    (Tag: $07;   Desc:'WhiteBalance'; Code:'1:Auto,2:Tungsten,'+
        '3:Daylight,4:Fluorescent,5:Shade,129:Manual'),
    (Tag: $08;   Desc:'Skip'),
    (Tag: $09;   Desc:'Skip'),
    (Tag: $0A;   Desc:'DigitalZoom'; Code:'65536:Off,65537:2X Digital Zoom'),
    (Tag: $0B;   Desc:'Sharpness'; Code:'0:Normal,1:Soft,2:Hard'),
    (Tag: $0C;   Desc:'Contrast'; Code:'0:Normal,1:Low,2:High'),
    (Tag: $0D;   Desc:'Saturation'; Code:'0:Normal,1:Low,2:High'),
    (Tag: $0E;   Desc:'Skip'),
    (Tag: $0F;   Desc:'Skip'),
    (Tag: $10;   Desc:'Skip'),
    (Tag: $11;   Desc:'Skip'),
    (Tag: $12;   Desc:'Skip'),
    (Tag: $13;   Desc:'Skip'),
    (Tag: $14;   Desc:'CCDSensitivity'; Code:'64:Normal,125:+1.0,250:+2.0,'+
        '244:+3.0,80:Normal,100:High'),
    (Tag: $15;   Desc:'Skip'),
    (Tag: $16;   Desc:'Skip'),
    (Tag: $17;   Desc:'Skip'),
    (Tag: $18;   Desc:'Skip'),
    (Tag: $19;   Desc:'Skip'),
    (Tag: $1A;   Desc:'Skip'));

  Fuji1Table : array [0..17] of TsdExifTagTable =
   ((Tag: $0000;    Desc:'Version'),
    (Tag: $1000;    Desc:'Quality'; Code:''),
    (Tag: $1001;    Desc:'Sharpness'; Code:'1:Soft,2:Soft,3:Normal,'+
          '4:Hard,5:Hard'),
    (Tag: $1002;    Desc:'WhiteBalance'; Code:'0:Auto,256:Daylight,'+
          '512:Cloudy,768:DaylightColor-fluorescence,'+
          '769:DaywhiteColor-fluorescence,770:White-fluorescence,'+
          '1024:Incandenscense,3840:Custom white balance.'),
    (Tag: $1003;    Desc:'Color'; Code:''),
    (Tag: $1004;    Desc:'Tone'; Code:''),
    (Tag: $1010;    Desc:'FlashMode'; Code:'0:Auto,1:On,2:Off,'+
          '3:Red-eye reduction'),
    (Tag: $1011;    Desc:'FlashStrength'; Code:''),
    (Tag: $1020;    Desc:'Macro'; Code:'0:Off,1:On'),
    (Tag: $1021;    Desc:'Focusmode'; Code:'0:Auto Focus,1:Manual Focus'),
    (Tag: $1030;    Desc:'SlowSync'; Code:'0:Off,1:On'),
    (Tag: $1031;    Desc:'PictureMode'; Code:'0:Auto,1:Portrait scene,'+
          '2:Landscape scene,4:Sports scene,5:Night scene,6:Program AE,'+
          '256:Aperture prior AE,512:Shutter prior AE,768:Manual exposure'),
    (Tag: $1032;    Desc:'Skip'),
    (Tag: $1100;    Desc:'ContTake/Bracket'; Code:'0:Off,1:On'),
    (Tag: $1200;    Desc:'Skip'),
    (Tag: $1300;    Desc:'BlurWarning'; Code:'0:No blur warning,'+
          '1:Blur warning'),
    (Tag: $1301;    Desc:'FocusWarning'; Code:'0:Auto Focus good,'+
          '1:Out of focus'),
    (Tag: $1302;    Desc:'AEWarning'; Code:'0:AE good,1:Over exposure')) ;

  Canon1Table : array [0..13] of TsdExifTagTable =
   ((Tag: $00;    Desc:'Skip'),
    (Tag: $01;    Desc:'ExposureInfo1'),
    (Tag: $02;    Desc:'Skip'),
    (Tag: $03;    Desc:'Skip'),
    (Tag: $04;    Desc:'ExposureInfo2'),
    (Tag: $06;    Desc:'ImageType'),
    (Tag: $07;    Desc:'FirmwareVersion'),
    (Tag: $08;    Desc:'ImageNumber'),
    (Tag: $09;    Desc:'OwnerName'),
    (Tag: $0A;    Desc:'Skip'),
    (Tag: $0C;    Desc:'CameraSerialNumber'),
    (Tag: $0D;    Desc:'Skip'),
    (Tag: $0F;    Desc:'CustomFunctions' ),
    (Tag: $10;    Desc:'Skip'));

  Epson1Table : array [0..11] of TsdExifTagTable =           //For Epson pc850Z     Lucas P.
   ((Tag: $0200;    Desc:'Special Mode'),
    (Tag: $0201;    Desc:'JpegQuality'),
    (Tag: $0202;    Desc:'Macro'),
    (Tag: $0203;    Desc:'Skip'),     // ??
    (Tag: $0204;    Desc:'DigiZoom'),
    (Tag: $0209;    Desc:'CameraID'),
    (Tag: $020a;    Desc:'Comments'),
    (Tag: $020b;    Desc:'Width'),
    (Tag: $020c;    Desc:'Height'),
    (Tag: $020d;    Desc:'SoftRelease'),
    (Tag: $0300;    Desc:'??'),       // ??
    (Tag: $0f00;    Desc:'skip'));

  Sanyo1Table : array [0..5] of TsdExifTagTable =
   ((Tag: $0200;    Desc:'Special Mode'),
    (Tag: $0201;    Desc:'JpegQuality'),
    (Tag: $0202;    Desc:'Macro'),
    (Tag: $0203;    Desc:'Skip'),
    (Tag: $0204;    Desc:'DigiZoom'),
    (Tag: $0F00;    Desc:'DataDump' ));



function DoGpsPosn(Value: Utf8String): Utf8String;
begin
  // To do: change!
  Result := Value;
end;

function DoCvtTime(Value: Utf8String): Utf8String;
begin
  // To do: change!
  Result := Value;
end;

function sdExifDateToDateTime(ExifDate: Utf8String; var DateTime: TDateTime): boolean;
begin
  Result := True;
  try
    DateTime :=
      EncodeDate(
        StrToInt(copy(ExifDate, 1, 4)),
        StrToInt(copy(ExifDate, 6, 2)),
        StrToInt(copy(ExifDate, 9, 2))) +
      EncodeTime(
        StrToInt(copy(ExifDate, 12, 2)),
        StrToInt(copy(ExifDate, 15, 2)),
        StrToInt(copy(ExifDate, 18, 2)), 0);
  except
    // Some error indicates non-valid date string, make date=0 to signal
    Result := False;
    DateTime := 0;
  end;
end;



{ TsdMetadataExif }

function TsdMetadataExif.Callback(Index: integer; Value: Utf8String): Utf8String;
begin
  case Index of
  cGpsPosn:   Value := DoGpsPosn(Value);
  cCvtTime:    Value := DoCvtTime(Value);
//  cNikonLens: Value := DoNikonLens(Value);
  else
    Result := '??' + Value;
  end;
end;

function TsdMetadataExif.DecodeField(DecodeStr, Idx: Utf8String): Utf8String;
var
  StPos: integer;
begin
   Result := '';
   // ease parsing
   Idx := Format(',%s:', [trim(Idx)]);
   DecodeStr := Format(',%s,', [DecodeStr]);
   StPos := pos(Idx, DecodeStr);
   if StPos > 0 then
   begin
     Result := copy(DecodeStr, stPos + length(Idx), length(DecodeStr));
     Result := copy(Result, 1, pos(',', Result) - 1);
   end;
end;

function TsdMetadataExif.ExifDateToDateTime(ADate: Utf8String): TDateTime;
var
  Res: boolean;
begin
  Res := sdExifDateToDateTime(ADate, Result);
  if not Res then
  begin
    // Some error indicates non-valid date string
    DoDebugOut(Self, wsWarn, sNonValidDateString);
  end;
end;

function TsdMetadataExif.FormatNumber(Fmt: integer; DecodeStr: Utf8String): Utf8String;
var
  i, Vlen: integer;
  tmp, tmp2: longint;
  ADouble: double;
begin
  Result := '';
  tmp := 0; tmp2 := 0; ADouble := 0;
  if (Fmt < 1) or (Fmt > 12) then raise
    Exception.Create('Illegal format in EXIF');
  Vlen := cBytesPerFormat[Fmt];
  for i := 0 to integer(FRecLength) div Vlen - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';  // Used for data display

    // Read the data
    case Fmt of
    FMT_SBYTE, FMT_BYTE:    Tmp := Get1;
    FMT_USHORT, FMT_SSHORT: Tmp := Get2;
    FMT_ULONG, FMT_SLONG:   Tmp := Get4;
    FMT_URATIONAL, FMT_SRATIONAL:
      begin
        Tmp := Get4;
        Tmp2 := Get4;
      end;
    FMT_SINGLE: ADouble := GetSingle;
    FMT_DOUBLE: ADouble := GetDouble;
    end;

    // Format the data
    case Fmt of
    FMT_SBYTE, FMT_BYTE, FMT_USHORT, FMT_SSHORT, FMT_ULONG, FMT_SLONG:
      if DecodeStr = '' then
        Result := Result + IntToStr(tmp)
      else
        Result := Result + Format('%s (%d)',
          [DecodeField(DecodeStr, IntToStr(tmp)), Tmp]);
    FMT_URATIONAL, FMT_SRATIONAL:
      if DecodeStr = '' then
        Result := Result + format('%d/%d', [Tmp, Tmp2])
      else
        Result := Result + format('%s (%d/%d)',
          [DecodeField(DecodeStr, format('%d/%d', [Tmp, Tmp2])), Tmp, Tmp2]);
    FMT_SINGLE, FMT_DOUBLE:
      Result := Result + Format('%3.2f', [ADouble]);
    else
      Result := Result + '?';
    end;
  end;
end;

function TsdMetadataExif.LookupHSTagID(Idx: integer; TagTbl: array of TsdExifTagTable): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to high(TagTbl) do
    if TagTbl[i].Tag = Idx then
    begin
      Result := i;
      break;
    end;
end;

function TsdMetadataExif.LookupTagID(Idx: integer; TagType: integer): integer;
var
 i: integer;
begin
  result := -1;
  case TagType of
  cExifTag:
    for i := 0 to cExifTagCnt - 1 do
      if cTagTable[i].Tag = Idx then
      begin
        Result := i;
        exit;
      end;
  cGpsTag:
    for i := 0 to cGPSCnt - 1 do
      if cGPSTable[i].Tag = Idx then
      begin
        Result := i;
        exit;
      end;
  end;
end;

function TsdMetadataExif.LookupHSCode(Idx: integer; TagTbl: array of TsdExifTagTable): Utf8String;
var
  i: integer;
begin
  Result := '';
  for i := 0 to high(TagTbl) do
    if TagTbl[i].Tag = Idx then
      Result := TagTbl[i].Code;
end;

function TsdMetadataExif.LookupCode(Idx: integer; TagType: integer): Utf8String;
var
  i: integer;
begin
  result := '';
  case TagType of
  cExifTag:
    for i := 0 to cExifTagCnt - 1 do
      if cTagTable[i].Tag = Idx then
        Result := cTagTable[i].Code;
  cGpsTag:
    for i := 0 to cGPSCnt - 1 do
      if cGPSTable[i].Tag = idx then
        Result := cGPSTable[i].Code;
  end;
end;

procedure TsdMetadataExif.Parse(ADirStart, AOffset, ALength: longword; TagType: integer);
var
  Entry, EntryCount: integer;
  Tag, AFormat: word;
  CompCount: longword;
  SavedPos: longword;
  Raw: AnsiString;
  Name, Value: Utf8String;
  CodeLine: Utf8String;
  TagID: integer;
  AChild: TXmlNode;
  // local: convert ascii characters to hex notation
  function StringToHex(AStr: Utf8String): Utf8String;
  var
    i: integer;
  begin
    for i := 1 to length(AStr) do
      Result := Result + IntToHex(ord(AStr[i]), 2);
  end;
// main
begin
  FStream.Position := ADirStart;
  EntryCount := integer(Get2);
  if (ADirStart + 2 + longword(EntryCount) * 12) > (AOffset + ALength) then
  begin
    DoDebugOut(Self, wsWarn, sIllSizedEXIFDirectory);
    exit;
  end;

  for Entry := 0 to EntryCount - 1 do
  begin
    // Initialize
    Name := '';
    Value := '';

    // Find record
    FStream.Position := integer(ADirstart) + 2 + 12 * Entry;
    Tag := Get2;
    AFormat := Get2;
    CompCount := Get4;
    FRecLength := integer(CompCount) * cBytesPerFormat[AFormat];
    if FRecLength > 4 then
      FStream.Position := AOffset + Get4;
    SavedPos := FStream.Position;

    // Construct value field
    case AFormat of
    FMT_UNDEFINED:
      begin
        SetLength(Raw, FRecLength);
        FStream.Read(Raw[1], FRecLength);
        Value := Raw;
      end;
    FMT_STRING:
      Value := GetString;
    else
      Setlength(Raw, FRecLength);
      FStream.Read(Raw[1], FRecLength);
      FStream.Position := SavedPos;

      CodeLine := LookupCode(Tag, TagType);
      Value := FormatNumber(AFormat, CodeLine);
    end;

    TagID := LookupTagID(Tag, TagType);
    if TagType = cGpsTag then
    begin
      if (TagID > 0) and (cGpsTable[TagID].CallBack > 0) then
        Value := Callback(cGpsTable[TagID].CallBack, Value)
    end else
    begin
      if (TagID > 0) and (cTagTable[TagID].CallBack > 0) then
        Value := Callback(cTagTable[TagID].CallBack, Value)
    end;

    // Replace any non-printable characters
    Value := MakePrintable(Value);

    // Name
    if TagID >= 0 then
      if TagType = cGpsTag then
        Name := cGpsTable[TagID].Desc
      else
        Name := cTagTable[TagID].Desc;

    // Add to XML
    if Name = '' then
      Name := Format('Tag%s', [IntToHex(Tag,4)]);
    AChild := FXML.NodeNew(Name);
    AChild.Value := Value;

    // XML properties that we add: Stream Pos, Data Type, Raw data
    if Verbose then
    begin
      AChild.AttributeAdd('SPOS', IntToHex(SavedPos, 8));
      AChild.AttributeAdd('DTYP', IntToStr(AFormat));
      AChild.AttributeAdd('COMP', IntToStr(CompCount));
    end;
    Raw := Copy(Raw, 1, 8); // just 8 first bytes
    if Verbose then
      AChild.AttributeAdd('RDAT', StringToHex(Raw));

    // Special processing for certain tags
    FStream.Position := SavedPos;
    case Tag of

    // EXIF Subdirectory
    TAG_EXIF_OFFSET, TAG_INTEROP_OFFSET:
      Parse(AOffset + Get4, AOffset, ALength, cExifTag);

    // GPS Subdirectory
    TAG_GPS_OFFSET:
      Parse(AOffset + Get4, AOffset, ALength, cGpsTag);

    // Store make
    TAG_MAKE:
      FMake := Value;
    TAG_MODEL:
      FModel := Value;
    TAG_MAKERNOTE:
      begin
        FMNOffset := SavedPos;
        FMNLength := FRecLength;
      end;
    end;// case
  end;
end;

procedure TsdMetadataExif.ParseHS(TagTbl: array of TsdExifTagTable;
  ADirStart, AOffset: longint);
var
  Entry, EntryCount: word;
  Base: integer;
  Tag, AFormat: word;
  Count: longword;
  Raw: AnsiString;
  Name, Value: Utf8String;
  CodeLine: Utf8String;
  TagID: integer;
begin
  FStream.Position := ADirStart;
  Base := ADirstart - AOffset - 2;
  EntryCount := Get2;
  for Entry := 0 to EntryCount - 1 do
  begin
    FStream.Position := ADirstart + 2 + 12 * Entry;
    Tag := Get2;
    AFormat := Get2;
    Count := Get4;
    FRecLength := integer(Count) * cBytesPerFormat[AFormat];
    if FRecLength > 4 then
      FStream.Position := Base + integer(Get4);

    // Initialize
    Value := '';

    // Construct value field
    case AFormat of
    FMT_UNDEFINED:
      begin
        SetLength(Raw, FRecLength);
        FStream.Read(Raw[1], FRecLength);
        Value := Raw;
      end;
    FMT_STRING:
      Value := GetString;
    else
      CodeLine := LookupHSCode(Tag, TagTbl);
      Value := FormatNumber(AFormat, CodeLine);
    end;

    TagID := LookupHSTagID(Tag, TagTbl);
    if TagID < 0 then
      Name := 'Unknown'
    else
      Name := TagTbl[TagID].Desc;
    if UpperCase(Name) = 'SKIP' then
      continue;

    // Callback... add!
    // To Do

    // Replace any non-printable characters
    Value := MakePrintable(Value);

    // Add to XML
    if Name = '' then Name := Format('Field%d', [Tag]);

    FXML.WriteString(Name, Value);

  end;
end;

// Read hardware specific data
procedure TsdMetadataExif.ReadHSData;
var
  UCMaker: Utf8String;
  Offset: longword;
  Head: array[0..4] of AnsiChar;
  OldOrder: word;
begin
  try
  UCMaker := UpperCase(Copy(FMake, 1, 5));

  // Offset
  if FIsTiff then
    Offset := FMNOffset + 16
  else
    Offset := FMNOffset + 0;

  if (UCMaker = 'NIKON') then
  begin
    FStream.Read(Head, 5);
    if Head = 'Nikon' then
      ParseHS(Nikon1Table, 8, Offset)
    else
      ParseHS(Nikon2Table, 0, Offset-8);
  end
  else if (UCMaker = 'OLYMP') then
  begin
    ParseHS(Olympus1Table, 8, Offset)
  end
{  else if (UCMaker = 'CASIO') then
  begin
//    ParseHS(Casio1Table, 0, Offset-8) -> obviously wrong
    ParseHS(Casio1Table, 8, Offset)
  end}
  else if (UCMaker = 'FUJIF') then
  begin
    //  Fuji uses motorola format for exif but not for MakerNote!!
    OldOrder := FOrder;
    FOrder := $4949;
    ParseHS(Fuji1Table, 12, 12);
    FOrder := OldOrder;
  end
  else if (UCMaker = 'CANON') then
  begin
    ParseHS(Canon1Table, 0, Offset-8);
  end
  else if (UCMaker = 'SEIKO') then
  begin
    ParseHS(Epson1Table, 8, Offset)
  end
  else if (UCMaker = 'SANYO') then
  begin
    ParseHS(Sanyo1Table, 8, Offset)
  end;
//    else if (UCMaker = 'MINOL') then
//    begin
//     ProcessHWSpecific(MakerNote,Nikon2Table,0,MakerOffset-8)
//    end
  except
    // silent exception
  end;
end;

procedure TsdMetadataExif.MetadataToXml(Stream: TStream; Xml: TXmlNode);
var
  Header: array[0..3] of char;
  M: TStream;
begin
  inherited;
  FStream.Read(Header, 4);
  if Header = 'Exif' then
  begin
    Get2;
    FOrder := Get2;
    if ((FOrder = $4949) or (FOrder = $4d4d)) then
    begin
      // Standard EXIF
      Parse(14, 6, Stream.Size - 6, cExifTag);
      // MakerNote
      if FMNLength > 0 then
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

class function TsdMetadataExif.IsValidSection(Stream: TStream): boolean;
var
  Header: array[0..3] of char;
  ByteOrder: word;
begin
  Result := False;
  Stream.Position := 0;
  Stream.Read(Header, 4);
  if Header = 'Exif' then
  begin
    Stream.Read(ByteOrder, 2);
    Stream.Read(ByteOrder, 2);
    if ((ByteOrder = $4949) or (ByteOrder = $4d4d)) then
      Result := True;
  end;
end;

end.
