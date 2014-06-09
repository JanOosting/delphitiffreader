unit TIFFAnalySIS;

interface
Uses Classes, SysUtils, Tiff;
// AnalySIS tiff type
const
  ttt_SISAnalysis = 33560; // LONG : offset to data structure of 60 bytes followed by pointers to other structures

type
  TSIS_main = packed record
    Magic : Array[0..3] of AnsiChar;    // Contains 'SIS0'
    Unknown :  array[0..21] of byte;
    Name : array[0..31] of AnsiChar;
    TagCount : word;
  end;

  TSIS_pointer = record
    TagType : word;
    Count : word;
    StreamPos : cardinal;
  end;

  TSIS_general = packed record    // TagType 1 : 220 bytes general data
    Unknown1 : array[0..9] of byte;
    LengthExponent : SmallInt;
    XCalibration : double;  //  m x 10 e LengthExponent per pixel (unit may be dependent on other vars)
    YCalibration : double;
    Unknown2 : array[0..7] of byte;
    Magnification : double;
    Unknown3 : array[0..1] of byte;
    CameraName : array[0..33] of AnsiChar;
    PictureType : array[0..31] of AnsiChar;
    // rest unknown
  end;

  TSIS_ChannelData = packed record  // TagType 10: 720 bytes
    Length : word;
    Unknown1 : array[0..21] of byte;
    ExposureTime : Cardinal;
    EMVoltage : double;
    Unknown2 : array[0..3] of byte;
    CameraName : array[0..31] of Ansichar;
    Unknown : array[0..47] of byte;
    MicroscopeType : array[0..31] of Ansichar;
    // rest unknown;
  end;

  { other tagtypes + some data from a picture
  2 : 770 bytes Lookup table ?
  3 : 16 bytes 00 00 08 00 60 05 08 04 01 00 00 00 20 00 00 00
  5 : 3 bytes  01 00 00
  7 : 14 bytes 00 00 00 00 FF FF 00 00  FF 00 00 00 00 00
  13: 24 bytes 14 00 00 00 14 00 00 00 00 00 00 00 0A 00 00 00 00 00 00 00 00 00 00 00
  14: 32 bytes 1C 00 00 00 1C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  }

  TSIS_Tiff = class
  private
    FYCalibration: double;
    FXCalibration: double;
    FMagnification: double;
    FExposureTime: integer;
    FLengthUnits: string;
  public
    constructor Create(Subimage:TTiffSubImage);
    property XCalibration : double read FXCalibration ;
    property YCalibration : double read FYCalibration ;
    property Magnification : double read FMagnification ;
    property ExposureTime : integer read FExposureTime;
    property LengthUnits : string read FLengthUnits;
  end;

  function HasAnalySIStag(SubImage:TTiffSubImage):boolean;

implementation

function HasAnalySIStag(SubImage:TTiffSubImage):boolean;
var
  IFDItem:TIFDItem;
begin
  IFDItem:=Subimage.IFDItems.FindItem(ttt_SISAnalysis);
  If Assigned(IFDItem) then
    Result:=True
  else
    Result:=False;
end;
{ TSIS_Tiff }

constructor TSIS_Tiff.Create(Subimage: TTiffSubImage);
var
  IFDItem:TIFDItem;
  Stream : TStream;
  SIS_Main : TSIS_Main;
  SIS_Pointer : TSIS_Pointer;
  SIS_General:TSIS_General;
  SIS_ChannelData : TSIS_ChannelData;
  CurrentPos,i :Integer;
begin
  // check to see if this image has analySIS structure
  IFDItem:=Subimage.IFDItems.FindItem(ttt_SISAnalysis);
  If Assigned(IFDItem) then
  begin
    // fill in data fields
    Stream:=SubImage.Owner.Data;
    Stream.Seek(IFDItem.IntValues[0],soFromBeginning);
    Stream.Read(SIS_Main,SizeOf(SIS_Main));
    If SIS_Main.Magic<>'SIS0' then
      Raise ETiffException.Create('Tiff file contains invalid AnalySIS structure');
    for i:= 1 to SIS_Main.TagCount do
    begin
      Stream.Read(SIS_Pointer,SizeOf(SIS_Pointer));
      CurrentPos:=Stream.Position;
      If SIS_Pointer.TagType=1 then
      begin
        Stream.Seek(SIS_Pointer.StreamPos,soFromBeginning);
        Stream.Read(SIS_General,SizeOf(SIS_general));
        FXCalibration:=SIS_General.XCalibration;
        FYCalibration:=SIS_General.YCalibration;
        FMagnification:=SIS_General.Magnification;
        case SIS_General.LengthExponent of
          -9: FLengthUnits := 'nm';
          -6: FLengthUnits := 'um';
          -3: FLengthUnits := 'mm';
          0 : FLengthUnits := 'm';
          3 : FLengthUnits := 'km';
        else
          FLengthUnits := 'e'+IntToStr(SIS_General.LengthExponent)+' m ';
        end;
      end;
      If SIS_Pointer.TagType=10 then
      begin
        Stream.Seek(SIS_Pointer.StreamPos,soFromBeginning);
        Stream.Read(SIS_ChannelData,SizeOf(SIS_ChannelData));
        FExposureTime:=SIS_ChannelData.ExposureTime;
      end;
      Stream.Position:=CurrentPos;
    end;
  end
  else begin
    Raise ETiffException.Create('Tiff file does not contain AnalySIS information');
  end;
end;

end.
