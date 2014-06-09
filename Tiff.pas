{**************************************************************

    Smooth Imaging Components - TIFF Components for Delphi
    Copyright (C) 2002 - Poseidonware.com

    Author:
        Steven Boren - sboren@poseidonware.com
        Jan Oosting  - j.oosting@lumc.nl

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

**************************************************************}
unit Tiff;

interface

uses classes, SysUtils, contnrs, windows, Graphics;


var
  DEBUG:boolean;
// TIFF Format values
const
  tiff_LittleEndian = $4949;
  tiff_BigEndian = $4D4D;
  tiff_MagicNumber = 42;

Type TNumberFormat = (nfLittleEndian,nfBigEndian);
// TagTypes
const
  ttt_NewSubfileType = 254; // LONG 1
  ttt_SubfileType = 255; // SHORT 1
  ttt_ImageWidth = 256; // 100 SHORT or LONG 1
  ttt_ImageLength = 257; // 101 SHORT or LONG 1
  ttt_BitsPerSample = 258; // 102 SHORT SamplesPerPixel
  ttt_Compression = 259; // 103 SHORT 1
  ttt_PhotometricInterpretation = 262; // 106 SHORT 1
  ttt_Threshholding = 263; // 107 SHORT 1
  ttt_CellWidth = 264; // 108 SHORT 1
  ttt_CellLength = 265; // 109 SHORT 1
  ttt_FillOrder = 266; // 10A SHORT 1
  ttt_DocumentName = 269; // 10D ASCII
  ttt_ImageDescription = 270; // 10E ASCII
  ttt_Make = 271; // 10F ASCII
  ttt_Model = 272; // 110 ASCII
  ttt_StripOffsets = 273; // 111 SHORT or LONG StripsPerImage
  ttt_Orientation = 274; // 112 SHORT 1
  ttt_SamplesPerPixel = 277; // 115 SHORT 1
  ttt_RowsPerStrip = 278; // 116 SHORT or LONG 1
  ttt_StripByteCounts = 279; // 117 LONG or SHORT StripsPerImage
  ttt_MinSampleValue = 280; // 118 SHORT SamplesPerPixel
  ttt_MaxSampleValue = 281; // 119 SHORT SamplesPerPixel
  ttt_XResolution = 282; // 11A RATIONAL 1
  ttt_YResolution = 283; // 11B RATIONAL 1
  ttt_PlanarConfiguration = 284; // 11C SHORT 1
  ttt_PageName = 285; // 11D ASCII
  ttt_XPosition = 286; // 11E RATIONAL
  ttt_YPosition = 287; // 11F RATIONAL
  ttt_FreeOffsets = 288; // 120 LONG
  ttt_FreeByteCounts = 289; // 121 LONG
  ttt_GrayResponseUnit = 290; // 122 SHORT 1
  ttt_GrayResponseCurve = 291; // 123 SHORT 2**BitsPerSample
  ttt_T4Options = 292; // 124 LONG 1
  ttt_T6Options = 293; // 125 LONG 1
  ttt_ResolutionUnit = 296; // 128 SHORT 1
  ttt_PageNumber = 297; // 129 SHORT 2
  ttt_TransferFunction = 301; // 12D SHORT {1 or SamplesPerPixel}* 2** BitsPerSample
  ttt_Software = 305; // 131 ASCII
  ttt_DateTime = 306; // 132 ASCII 20
  ttt_Artist = 315; // 13B ASCII
  ttt_HostComputer = 316; // 13C ASCII
  ttt_Predictor = 317; // 13D SHORT 1
  ttt_WhitePoint = 318; // 13E RATIONAL 2
  ttt_PrimaryChromaticities = 319; // 13F RATIONAL 6
  ttt_ColorMap = 320; // 140 SHORT 3 * (2**BitsPerSample)
  ttt_HalftoneHints = 321; // 141 SHORT 2
  ttt_TileWidth = 322; // 142 SHORT or LONG 1
  ttt_TileLength = 323; // 143 SHORT or LONG 1
  ttt_TileOffsets = 324; // 144 LONG TilesPerImage
  ttt_TileByteCounts = 325; // 145 SHORT or LONG TilesPerImage
  ttt_InkSet = 332; // 14C SHORT 1
  ttt_InkNames = 333; // 14D ASCII total number of characters in all ink name strings, including zeros
  ttt_NumberOfInks = 334; // 14E SHORT 1
  ttt_DotRange = 336; // 150 BYTE or SHORT 2, or 2*NumberOfInks
  ttt_TargetPrinter = 337; // 151 ASCII any
  ttt_ExtraSamples = 338; // 152 BYTE number of extra compo-nents per pixel
  ttt_SampleFormat = 339; // 153 SHORT SamplesPerPixel
  ttt_SMinSampleValue = 340; // 154 Any SamplesPerPixel
  ttt_SMaxSampleValue = 341; // 155 Any SamplesPerPixel
  ttt_TransferRange = 342; // 156 SHORT 6
  ttt_JPEGTables = 347; //15B Any
  ttt_JPEGProc = 512; // 200 SHORT 1
  ttt_JPEGInterchangeFormat = 513; // 201 LONG 1
  ttt_JPEGInterchangeFormatLngth = 514; // 202 LONG 1
  ttt_JPEGRestartInterval = 515; // 203 SHORT 1
  ttt_JPEGLosslessPredictors = 517; // 205 SHORT SamplesPerPixel
  ttt_JPEGPointTransforms = 518; // 206 SHORT SamplesPerPixel
  ttt_JPEGQTables = 519; // 207 LONG SamplesPerPixel
  ttt_JPEGDCTables = 520; // 208 LONG SamplesPerPixel
  ttt_JPEGACTables = 521; // 209 LONG SamplesPerPixel
  ttt_YCbCrCoefficients = 529; // 211 RATIONAL 3
  ttt_YCbCrSubSampling = 530; // 212 SHORT 2
  ttt_YCbCrPositioning = 531; // 213 SHORT 1
  ttt_ReferenceBlackWhite = 532; // 214 LONG 2*SamplesPerPixel
  ttt_Copyright = 33432; // 8298 ASCII Any


// ValueTypes
const
  tvt_BYTE = 1;
  tvt_ASCII = 2;
  tvt_SHORT = 3;
  tvt_LONG = 4;
  tvt_RATIONAL = 5;
  tvt_SBYTE = 6;
  tvt_UNDEFINED = 7;
  tvt_SSHORT = 8;
  tvt_SLONG = 9;
  tvt_SRATIONAL = 10;
  tvt_FLOAT = 11;
  tvt_DOUBLE = 12;

  tvt_Sizes : Array[tvt_Byte..tvt_Double] of Word = (1,1,2,4,8,1,1,2,4,8,4,8);
// CompressionTypes
const
  tct_Uncompressed = 1;
  tct_GROUP_3_HUFFMAN = 2;
  tct_GROUP_3_FAX = 3;
  tct_GROUP_4_FAX = 4;
  tct_LZW = 5;
  tct_JPEG  =6;
  tct_PACKBITS = 32773;

// PhotometricInterpretation
const
  tpi_WhiteIsZero = 0;
  tpi_BlackIsZero = 1;
  tpi_RGB = 2;
  tpi_RGB_Palette = 3;
  tpi_Transparency_mask = 4;
  tpi_CMYK = 5;
  tpi_YCbCr = 6;
  tpi_CIELab = 8;


type
    TTIFFType = (tf1Bit, tf4Bit, tf8BitGrayScale, tf8BitColor, tf24Bit);

const
    PixelCountMax = 32768;

type
    pRGBArray  = ^TRGBArray;
    TRGBArray   = ARRAY[0..PixelCountMax-1] OF TRGBTriple;
    TPaletteEntries = array [0..0] of TPaletteEntry;
    TPaletteEntriesPtr = ^TPaletteEntries;

type
    ETiffException = class( Exception );

type
    RATIONAL = record
      numerator,denominator : Cardinal;
    end;

    SRATIONAL = record
      numerator,denominator : Integer;
    end;

    TIFDEntry = record
      Tag : Word;
      ValueType : Word;
      Count : Cardinal;

      Case Word of
        { tvt_RATIONAL , tvt_SRATIONAL and tvt_DOUBLE only available through Offset}
        tvt_BYTE : (ByteValue : Array[0..3] of Byte);
        tvt_ASCII : (AsciValue:Array[0..3] of Ansichar);
        tvt_SHORT : (ShortValue: Array[0..1] of Word);
        tvt_LONG : (LongValue: Cardinal);
        tvt_SBYTE : (SByteValue:Array[0..3] of ShortInt);
        tvt_UNDEFINED : (SUndefinedValue:Array[0..3] of Byte);
        tvt_SSHORT : (SShortValue : Array[0..1] of SmallInt);
        tvt_SLONG : (SLongValue : Integer);
        tvt_FLOAT : (FloatValue : Single);
        tvt_RATIONAL,tvt_SRATIONAL,tvt_DOUBLE: (OffSetValue : Cardinal) ;

    end;

  TIFDItem = class
  private
    FNumberFormat: TNumberFormat;
    SmallValue : Boolean;
    function GetFloatValues(index: integer): double;
    function GetIntValues(index: integer): Integer;
    function GetStrValue: string;
    procedure SetFloatValues(index: integer; const Value: double);
    procedure SetIntValues(index: integer; const Value: Integer);
    procedure SetStrValue(const Value: string);
    procedure AdjustEntry;
    procedure AdjustValues;
    procedure SetNumberFormat(const Value: TNumberFormat);
  public
    Entry : TIFDEntry;
    Values: Pointer;
    constructor Create(aNumberFormat:TNumberFormat);
    destructor Destroy;Override;
    function DisplayNameValues:String;
    property IntValues[index:integer]:Integer read GetIntValues write SetIntValues;
    property FloatValues[index:integer]:double read GetFloatValues write SetFloatValues;
    property StrValue : string read GetStrValue write SetStrValue;
    property Tag : Word read Entry.Tag;
    property Count : cardinal read Entry.Count;
    property NumberFormat:TNumberFormat read FNumberFormat write SetNumberFormat;
  end;

type
  TIFDList = class( TObjectList )
  private
    function GetIFDItem(Index: Integer): TIFDItem;
    procedure SetIFDItem(Index: Integer; const Value: TIFDItem);
  public
    StartPos:cardinal;
    LastPos:cardinal;
    IFDPos:cardinal;
    function FindItem( aTag: Integer ): TIFDItem;
    function GetIFD(aStream: TStream ): cardinal;
    procedure ShowData;
    procedure UpdateRange(offsetTag, byteCountTag: integer);
    property IFDItems[Index: Integer]: TIFDItem read GetIFDItem write SetIFDItem; default;
  end;

type
  TTIFFImage = class (TBitmap)
    Data : TMemoryStream;
    SubImages:TObjectList;
  private
    FPage: integer;
    procedure SetPage(const Value: integer);
    function GetPageCount: integer;
    procedure DoProgress(Stage: TProgressStage;PercentDone: Byte; const Msg: string);
  public
    NativeFormat : Boolean;
    constructor Create;override;
    destructor Destroy;override;
    procedure LoadFromStream(Stream:TStream);override;
    procedure SaveToStream(Stream:TStream);override;
    property Page:integer read FPage write SetPage;
    property PageCount:integer read GetPageCount;
  end;

  // Loads tags from tif only
  TTIFFInfo = class (TTIFFImage)
  public
    procedure LoadFromStream(Stream:TStream);override;
    procedure SaveToStream(Stream:TStream);override;
  end;

  TTiffSubImage = class
  private
    // mFileStream: TStream;
    mTiffType: TTIFFType;
    FOwner: TTiffImage;

    procedure InitBitmap;
    procedure CopyToBitmap(ImageStream:TStream);
    procedure GetImageData(Stream:TStream);
    procedure GetImageDescription;
    procedure SetOwner(const Value: TTiffImage);
  public
    Startpos:cardinal;
    LastPos:cardinal;
    DataSize:cardinal;
    Bitmap:TBitmap;
    IFDItems : TIFDList;
    ImageWidth: Integer;
    ImageHeight: Integer;
    RowsPerStrip: Integer;
    BytesPerRowUncompressed: Integer;
    StripOffsets: Integer;
    BitsPerSample: Integer;
    SamplesPerPixel: Integer;
    PhotometricInterpretation: Integer;
    IsGrayScale: Boolean;
    ColorMapCount : Integer;
    ColorMapAddress : Integer;
    Compression: Integer;
    ImageSize: Integer;
    constructor Create(aOwner:TTiffImage);
    destructor Destroy;override;
    function LoadFromStream(aStream: TStream ): Integer; virtual;
    property Owner : TTiffImage read FOwner write SetOwner;
  end;

  TTiffSubInfoImage = class(TTiffSubImage)
    function LoadFromStream(aStream: TStream ): Integer; virtual;
  end;

implementation

uses Compression, Dialogs, Math;


var
  CurrentNumberFormat : TNumberFormat;

function ReadWord(Stream:TStream):Word;
begin
  Stream.Read(result,2);
  If CurrentNumberFormat=nfBigEndian then
  begin
    result:=Swap(Result);
  end;
end;

function ReadShortInt(Stream:TStream):ShortInt;
begin
  Stream.Read(result,2);
  If CurrentNumberFormat=nfBigEndian then
  begin
    result:=Swap(result);
  end;
end;

type
  Byte4 = Array[1..4] of Byte;

function Swap32(Value:Cardinal):Cardinal;overload;
begin
  Result:=Byte4(Value)[1] + Byte4(Value)[2] Shl 8 +Byte4(Value)[3] Shl 16 +Byte4(Value)[4] Shl 24;
end;

function Swap32(Value:Integer):Integer;overload;
begin
  Result:=Byte4(Value)[1] + Byte4(Value)[2] Shl 8 +Byte4(Value)[3] Shl 16 +(Byte4(Value)[4] and 127) Shl 24;
  If Byte4(Value)[4]>127 Then
    Result:=-Result;
end;

function ReadCardinal(Stream:TStream):Cardinal;
begin
  Stream.Read(result,4);
  If CurrentNumberFormat=nfBigEndian then
  begin
    result:=Swap32(Result);
  end;
end;

function ReadInteger(Stream:TStream):Integer;
begin
  Stream.Read(result,4);
  If CurrentNumberFormat=nfBigEndian then
  begin
    result:=Swap32(Result);
  end;
end;

{ TIFDList }

function TIFDList.FindItem(aTag: Integer): TIFDItem;
var
  idx: Integer;
begin
  result := nil;
  for idx:=0 to Count-1 do
    if IFDItems[ idx ].Entry.Tag = aTag then
    begin
      result := IFDItems[ idx ];
      break;
    end;
end;

function TIFDList.GetIFDItem(Index: Integer): TIFDItem;
begin
    result := TIFDItem( inherited GetItem( Index ) );
end;

function TIFDList.GetIFD(aStream: TStream): Cardinal;
var
  anItem : TIFDItem;

  IFDCount: Word;
  idx: Integer;
  position: Integer;
  ValuesSize : cardinal;
begin
  //number of IFD entries
  StartPos:=aStream.Position;
  IFDPos:=StartPos;
  IFDCount:=ReadWord(aStream);
  LastPos:=aStream.Position+12*IFDCount;
  for idx:=1 to IFDCount do begin
    anItem := TIFDItem.Create(CurrentNumberFormat);
    //tag
    aStream.Read( anItem.Entry, 12 );
    anItem.AdjustEntry;
    //make sure the tag is valid
    { JO:All tags should be read
    if ( anItem.Entry.Tag > 532 ) and ( anItem.Entry.Tag <> 33432 ) then begin
      anItem.Free;
      continue;
    end;}

    //load values
    ValuesSize:=anItem.Entry.Count * tvt_Sizes[anItem.Entry.ValueType];
    If ValuesSize<=4 then begin
      anItem.SmallValue:=true;
      Cardinal(anItem.Values):=anItem.Entry.LongValue
    end
    else begin
      anItem.SmallValue:=false;
      GetMem(anItem.Values,ValuesSize);
      position := aStream.Position;
      aStream.Seek( anItem.Entry.OffSetValue, soFromBeginning );
      StartPos:=Min(StartPos,aStream.Position);
      aStream.Read( anItem.Values^, ValuesSize);
      LastPos:=Max(LastPos,aStream.Position);
      aStream.Seek( position, soFromBeginning );
    end;
    anItem.AdjustValues;
    Add( anItem );
  end;
  // Check the offsets + ByteCounts to update startpos and lastpos
  UpdateRange(ttt_TileOffsets,ttt_TileByteCounts);
  UpdateRange(ttt_StripOffsets,ttt_StripByteCounts);
  UpdateRange(ttt_FreeOffsets,ttt_FreeByteCounts);
  //next IFD offset
  result:=ReadCardinal(aStream);
end;

procedure TIFDList.SetIFDItem(Index: Integer; const Value: TIFDItem);
begin
    //TODO
end;

procedure TIFDList.ShowData;
var
  msgstr:String;
  i:integer;
begin
  for i:=0 to Count-1 do
    msgStr:=msgStr+IFDItems[i].DisplayNameValues+#13#10;
  ShowMessage(msgstr);
end;

procedure TIFDList.UpdateRange(offsetTag, byteCountTag: integer);
var
  offsetItem, bytesCountItem : TIFDItem;
  i:integer;
begin
  offsetItem:=FindItem(offsetTag);
  bytesCountItem:=FindItem(byteCountTag);
  if assigned(offsetItem) and assigned(bytesCountItem) then
  begin
    for I := 0 to offsetItem.Count - 1 do
    begin
      startPos:=min(startpos,offsetItem.IntValues[i]);
      lastPos:=max(lastpos,offsetItem.IntValues[i]+bytesCountItem.IntValues[i]);
    end;
  end;
end;

{ TTiffSubImage }

constructor TTiffSubImage.Create(aOwner:TTiffImage);
begin
  Owner:=aOwner;
  Bitmap := TBitmap.Create;
  IFDItems := TIFDList.Create( true );
  // mImageStream:= TMemoryStream.Create;
end;

destructor TTiffSubImage.Destroy;
begin
  IFDItems.Free;
  Bitmap.Free;
  // mImageStream.Free;
  inherited;
end;

procedure TTiffSubImage.GetImageData(Stream:TStream);
var
  mImageStream: TMemoryStream;
  CurrentRow : Pointer;
  ScanLineInterval : Integer;

  procedure LoadStrips;
  var
    stripIdx: Integer;
    stripCount: Integer;
    stripAddresses: TIFDItem;
    stripSizes: TIFDItem;
    stripBytes: Integer;
    FStripStream: TBitStream;
    rowsToDecompress: Integer;
    rowsDecompressed: Integer;

  begin
    FStripStream := TBitStream.Create;
    try
      stripSizes := IFDItems.FindItem( ttt_StripByteCounts );
      stripAddresses := IFDItems.FindItem( ttt_StripOffsets );
      if assigned(StripSizes) and assigned(stripAddresses) then
      begin
        stripCount := stripAddresses.Entry.Count;
        if ImageHeight < RowsPerStrip then
          RowsPerStrip:=ImageHeight;
        rowsToDecompress := RowsPerStrip ;
        rowsDecompressed := 0;
        // Prepare copying to bitmap directly
        CurrentRow:=Bitmap.ScanLine[0];
        ScanLineInterval:=Integer(Bitmap.Scanline[1]) - Integer(CurrentRow);
         //Read each strip and decompress it if nessesary...
        for stripIdx:=0 to stripCount-1 do begin
          Owner.DoProgress(psRunning,5+Round(((StripIdx+1)/(StripCount+1))*90),'Uncompressing');
          stripBytes := stripSizes.IntValues[ stripIdx ];
          Stream.Seek( stripAddresses.IntValues[ stripIdx ], soFromBeginning );
          FStripStream.Clear;
          FStripStream.CopyFrom(Stream, stripBytes );
          case Compression of
            tct_PACKBITS: DecompressPackBits( FStripStream, BytesPerRowUncompressed,
                rowsToDecompress );
            tct_GROUP_3_HUFFMAN: DecompressModifiedHuffman( FStripStream, ImageWidth, rowsToDecompress );
            tct_GROUP_3_FAX: DecompressCCITTFax3( FStripStream, ImageWidth, rowsToDecompress, true, stripIdx = 0 );
            tct_GROUP_4_FAX: DecompressCCITTFax4( FStripStream, ImageWidth, rowsToDecompress, CurrentRow, ScanLineInterval);
            tct_LZW : Raise ETiffException.Create('Tiff: LZW Compression not supported');
            tct_JPEG : Raise ETiffException.Create('Tiff: JPEG Compression not supported');
          end;
          FStripStream.Seek(0,soFromBeginning);
          mImageStream.CopyFrom(FStripStream, FStripStream.Size );
          Inc( rowsDecompressed, RowsPerStrip );
          Inc(Integer(CurrentRow),ScanLineInterval*RowsPerStrip);
          //the last strip often does not have all the rows
          if ( ImageHeight - rowsDecompressed ) < RowsPerStrip then
              rowsToDecompress := ImageHeight - rowsDecompressed;
        end;
      end
      else begin
        // probably has tiles and not strips
      end;
      FStripStream.Free;
    except
      on ETiffException do begin
        Owner.DoProgress(psEnding,100,'Error decompressing');
        raise;
      end;
    end;
  end;

begin
  try
    mImageStream:= TMemoryStream.Create;
    InitBitmap;
    LoadStrips;
    If Not(Compression in [tct_GROUP_4_FAX]) then
      CopyToBitmap(mImageStream);
    // set Palette in initbitmap



    // All data is now in bitmap
    //If debug then
    //  mImageStream.SaveToFile('c:\testtiff.strm');
    mImageStream.Free;
    mImageStream:=nil;
  except on
    e: ETiffException do Raise;
  end;
end;

procedure TTiffSubImage.InitBitmap;
var
  bitsPerPixel: Integer;

  function GetColorPalette:hPalette;
  var
    Palette: TMaxLogPalette;
    ColorMap: TIFDItem;
    i:integer;
  begin
    Colormap:=IFDItems.FindItem(ttt_Colormap);
    Palette.palVersion := $300;
    Palette.palNumEntries := 256;
    for i:=0 to 255 do
    begin
      with Palette.palPalEntry[i] do
      begin
        peRed := Hi(Colormap.IntValues[i]);
        peGreen := Hi(Colormap.IntValues[256+i]);
        peBlue := Hi(Colormap.IntValues[512+i]);
        peFlags := 0;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Palette)^)
  end;

  function GetGrayScalePalette:hPalette;
  var
    Palette: TMaxLogPalette;
    i:integer;
  begin
    Palette.palVersion := $300;
    Palette.palNumEntries := 256;
    for i:=0 to 255 do
    begin
      with Palette.palPalEntry[i] do
      begin
        peRed := i;
        peGreen := i;
        peBlue := i;
        peFlags := 0;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Palette)^)
  end;

  FUNCTION GetTwoColorPalette(BlackIsZero:Boolean): hPalette;
  VAR
    Palette: TMaxLogPalette;
  BEGIN
    Palette.palVersion := $300;
    Palette.palNumEntries := 2;
    if BlackIsZero then
    begin
      WITH Palette.palPalEntry[0] DO
      BEGIN
        peRed := 255;
        peGreen := 255;
        peBlue := 255;
        peFlags := 0;
      END;

      WITH Palette.palPalEntry[1] DO
      BEGIN
        peRed := 0;
        peGreen := 0;
        peBlue := 0;
        peFlags := 0;
      END;
    end
    else begin
      WITH Palette.palPalEntry[0] DO
      BEGIN
        peRed := 0;
        peGreen := 0;
        peBlue := 0;
        peFlags := 0;
      END;

      WITH Palette.palPalEntry[1] DO
      BEGIN
        peRed := 255;
        peGreen := 255;
        peBlue := 255;
        peFlags := 0;
      END;
    end;
    RESULT := CreatePalette(pLogPalette(@Palette)^)
  END {GetTwoColorPalette};
begin
  // Due to bug in Delphi <= 5, Width and Height have to be declared before pixelformat
  // see http://www.efg2.com/Lab/ImageProcessing/Scanline.htm#Scanline
  // Setting pixelformat to 1 bit first decreases memory requirements for smaller than 24bit formats
  Bitmap.Height:=0;
  Bitmap.PixelFormat:=pf1Bit;
  Bitmap.Width := ImageWidth;
  Bitmap.Height := ImageHeight;

  //set the pixel format
  if SamplesPerPixel = 0 then
    SamplesPerPixel := 1;

  bitsPerPixel := BitsPerSample * SamplesPerPixel;
  case bitsPerPixel of
      1: begin
         end;
  else
  end;

  //set the TIFF type flag
  case bitsPerPixel of
  1 : begin
        mTiffType := tf1Bit;
        Bitmap.PixelFormat := pf1bit;
        // Palette has to be re-instated
        Bitmap.Palette := GetTwoColorPalette(PhotometricInterpretation = tpi_BlackIsZero);
      end;
  4 : begin
        mTiffType := tf4Bit;
        Bitmap.PixelFormat := pf24bit;
      end;
  8 : begin
        Bitmap.PixelFormat := pf8bit;
        if PhotometricInterpretation = tpi_RGB_Palette then
        begin
          mTiffType := tf8BitColor;
          Bitmap.Palette := GetColorPalette;
        end
        else begin
          mTiffType := tf8BitGrayScale;
          Bitmap.Palette := GetGrayScalePalette;
        end;
      end;
  24: begin
        mTiffType := tf24Bit;
        Bitmap.PixelFormat := pf24bit;
      end;
  end;

end;

function TTiffSubImage.LoadFromStream(aStream: TStream): Integer;
begin
  // mFileStream := aStream;
  result := IFDItems.GetIFD(aStream );
  startPos:=IFDItems.StartPos;
  lastPos:=max(aStream.Position,IFDItems.LastPos);
  GetImageDescription;
  GetImageData(aStream);
  AStream.Seek(result,soFromBeginning);
end;

procedure TTiffSubImage.GetImageDescription;
var
    anItem: TIFDItem;
    idx: Integer;

begin
  Owner.DoProgress(psStarting,5, 'Loading IFD Values');
  if DEBUG then
    IFDItems.ShowData;
  //set all the useful fields
  for idx:=0 to IFDItems.Count-1 do begin
    anItem := IFDItems[idx];
    case anItem.Tag of
      ttt_ImageWidth: ImageWidth := anItem.IntValues[0];
      ttt_BitsPerSample: BitsPerSample := anItem.IntValues[0];
      ttt_ImageLength: ImageHeight := anItem.IntValues[0];
      ttt_PhotometricInterpretation: PhotometricInterpretation := anItem.IntValues[0];
      ttt_SamplesPerPixel: SamplesPerPixel := anItem.IntValues[0];
      ttt_RowsPerStrip: RowsPerStrip := anItem.IntValues[0];
      ttt_Compression: Compression := anItem.IntValues[0];
    end;
  end;
  if RowsPerStrip=0 then
    RowsPerStrip:=ImageHeight;
  //calculated fields
  if BitsPerSample = 1 then begin
    ImageSize := (((ImageWidth + 7) div 8)) * ImageHeight;
    BytesPerRowUncompressed := (ImageWidth + 7) div 8;
  end;
end;

procedure TTiffSubImage.CopyToBitmap(ImageStream:TStream);

  procedure Write1BitBmp;
  var
      row: Integer;
      aRow: PByteArray;
  begin
      If Not Bitmap.Monochrome then
        Bitmap.monochrome:=true;
      for row:=0 to Bitmap.Height-1 do begin
        aRow := Bitmap.ScanLine[row];
        ImageStream.ReadBuffer(aRow[0],BytesPerRowUnCompressed);
      end;
  end;


  procedure Write4BitBmp;
  var
      row: Integer;
      aRow: pRGBArray;
      byteIdx: Integer;
      aByte: Byte;
      width: Integer;
      imgByte: Byte;
      pixIdx: Integer;
  begin
      if Bitmap.Width mod 2 = 0 then
          width := Bitmap.Width div 2
      else
          width := Bitmap.Width div 2 + 1;

      for row:=0 to Bitmap.Height-1 do begin
          aRow := pRGBArray(Bitmap.ScanLine[row]);
          pixIdx := 0;
          for byteIdx:=0 to width-1 do begin
              ImageStream.Read( aByte, SizeOf( aByte ) );

              imgByte := (aByte shr 4) + 1;
              aRow[pixIdx].rgbtRed := imgByte * 16-1;
              aRow[pixIdx].rgbtGreen := imgByte * 16-1;
              aRow[pixIdx].rgbtBlue := imgByte * 16-1;
              inc( pixIdx );

              imgByte := (aByte and 15) + 1;
              aRow[pixIdx].rgbtRed := imgByte * 16-1;
              aRow[pixIdx].rgbtGreen := imgByte * 16-1;
              aRow[pixIdx].rgbtBlue := imgByte * 16-1;
              inc( pixIdx );
          end;
      end;
      Bitmap.PixelFormat := pf4bit;
  end;

  procedure Write8BitBmp;
  var
      row: Integer;
      aRow: PByteArray;
  begin
      for row:=0 to Bitmap.Height-1 do begin
        aRow := Bitmap.ScanLine[row];
        ImageStream.ReadBuffer(aRow[0],ImageWidth);
      end;
  end;


  procedure Write8BitGrayScaleBmp;
  var
      row: Integer;
      aRow: pRGBArray;
      byteIdx: Integer;
      aByte: Byte;
  begin
      for row:=0 to Bitmap.Height-1 do begin
          aRow := pRGBArray(Bitmap.ScanLine[row]);
          for byteIdx:=0 to Bitmap.Width-1 do begin
              ImageStream.Read( aByte, SizeOf( aByte ) );
              aRow[byteIdx].rgbtRed := aByte;
              aRow[byteIdx].rgbtGreen := aByte;
              aRow[byteIdx].rgbtBlue := aByte;
          end;
      end;
      Bitmap.PixelFormat := pf8bit;
  end;

  procedure Write8BitColorBmp;
  var
      row: Integer;
      aRow: pRGBArray;
      byteIdx: Integer;
      aByte: Byte;

      procedure LoadColorMap;
      begin
      end;

      procedure GetColor( anID: Integer; var red, green, blue: Byte );
      begin

      end;


  begin
      LoadColorMap;

      for row:=0 to Bitmap.Height-1 do begin
          aRow := pRGBArray(Bitmap.ScanLine[row]);
          for byteIdx:=0 to Bitmap.Width-1 do begin
              ImageStream.Read( aByte, SizeOf( aByte ) );
              aRow[byteIdx].rgbtRed := aByte;
              aRow[byteIdx].rgbtGreen := aByte;
              aRow[byteIdx].rgbtBlue := aByte;
          end;
      end;
      Bitmap.PixelFormat := pf8bit;
  end;


  procedure Write24BitBmp;
  var
      row: Integer;
      aRow: pRGBArray;
      byteIdx: Integer;
      aByte: Byte;
  begin
      for row:=0 to Bitmap.Height-1 do begin
          aRow := pRGBArray(Bitmap.ScanLine[row]);
          for byteIdx:=0 to Bitmap.Width-1 do begin
              ImageStream.Read( aByte, SizeOf( aByte ) );

              if not IsGrayScale then begin
                  aRow[byteIdx].rgbtRed := aByte;
                  ImageStream.Read( aByte, SizeOf( aByte ) );
                  aRow[byteIdx].rgbtGreen := aByte;
                  ImageStream.Read( aByte, SizeOf( aByte ) );
                  aRow[byteIdx].rgbtBlue := aByte;
              end
              else begin
                  aRow[byteIdx].rgbtRed := aByte;
                  aRow[byteIdx].rgbtGreen := aByte;
                  aRow[byteIdx].rgbtBlue := aByte;
              end;
          end;
      end;
  end;

begin
  ImageStream.Seek( 0, soFromBeginning );
  case mTiffType of
    tf1Bit: Write1BitBmp;
    tf4Bit: Write4BitBmp;
    tf8BitGrayScale: Write8BitBmp;
    tf8BitColor: Write8BitBmp;
    tf24Bit: Write24BitBmp;
  end;
end;

procedure TTiffSubImage.SetOwner(const Value: TTiffImage);
begin
  FOwner := Value;
end;

{ TIFDItem }

procedure TIFDItem.AdjustEntry;
begin
  If NumberFormat=nfBigEndian then
  begin
    Entry.Tag:=Swap(Entry.Tag);
    Entry.Count:=Swap32(Entry.Count);
    Entry.ValueType:=Swap(Entry.ValueType);
    // Smaller variables will be adjusted with AdjustValues
    If (Entry.Count * tvt_Sizes[Entry.ValueType]) > 4 then
      Entry.OffsetValue:=Swap32(Entry.OffSetValue);
  end;
end;

procedure TIFDItem.AdjustValues;
{var
  i:integer;}
begin
  if (NumberFormat=nfBigEndian) and (tvt_Sizes[Entry.ValueType]>1) then
  begin
{    Case Entry.ValueType of
      tvt_SHORT,tvt_SSHORT:for i:=0 to Count-1 do
                             ;
      tvt_LONG = 4;
      tvt_RATIONAL = 5;
      tvt_SSHORT = 8;
      tvt_SLONG = 9;
      tvt_SRATIONAL = 10;
      tvt_FLOAT = 11;
      tvt_DOUBLE = 12;

    end; }
  end;
end;

constructor TIFDItem.Create(aNumberFormat: TNumberFormat);
begin
  Inherited Create;
  NumberFormat:=aNumberFormat;
end;

destructor TIFDItem.Destroy;
begin
  If Not SmallValue then
    FreeMem(Values);
  inherited;
end;

type
  ArrRATIONAL = Array[0..0] of RATIONAL;
  ArrSRATIONAL = Array[0..0] of SRATIONAL;
  ArrFLOAT = Array[0..0] of real;
  ArrDOUBLE = Array[0..0] of Double;

function TIFDItem.DisplayNameValues: String;
var
  idx:Integer;
begin
  Case Tag of
    ttt_NewSubfileType : Result:='NewSubfileType'; // LONG 1
    ttt_SubfileType  : Result:='SubfileType'; //= 255; // SHORT 1
    ttt_ImageWidth  : Result:='ImageWidth'; //= 256; // 100 SHORT or LONG 1
    ttt_ImageLength  : Result:='ImageLength'; //= 257; // 101 SHORT or LONG 1
    ttt_BitsPerSample  : Result:='BitsPerSample'; //= 258; // 102 SHORT SamplesPerPixel
    ttt_Compression  : Result:='Compression'; //= 259; // 103 SHORT 1
    ttt_PhotometricInterpretation  : Result:='PhotometricInterpretation'; //= 262; // 106 SHORT 1
    ttt_Threshholding  : Result:='Threshholding'; //= 263; // 107 SHORT 1
    ttt_CellWidth  : Result:='CellWidth'; //= 264; // 108 SHORT 1
    ttt_CellLength  : Result:='CellLength'; //= 265; // 109 SHORT 1
    ttt_FillOrder  : Result:='FillOrder'; //= 266; // 10A SHORT 1
    ttt_DocumentName  : Result:='DocumentName'; //= 269; // 10D ASCII
    ttt_ImageDescription  : Result:='ImageDescription'; //= 270; // 10E ASCII
    ttt_Make  : Result:='Make'; //= 271; // 10F ASCII
    ttt_Model  : Result:='Model'; //= 272; // 110 ASCII
    ttt_StripOffsets  : Result:='StripOffsets'; //= 273; // 111 SHORT or LONG StripsPerImage
    ttt_Orientation  : Result:='Orientation'; //= 274; // 112 SHORT 1
    ttt_SamplesPerPixel  : Result:='SamplesPerPixel'; //= 277; // 115 SHORT 1
    ttt_RowsPerStrip  : Result:='RowsPerStrip'; //= 278; // 116 SHORT or LONG 1
    ttt_StripByteCounts  : Result:='StripByteCounts'; //= 279; // 117 LONG or SHORT StripsPerImage
    ttt_MinSampleValue  : Result:='MinSampleValue'; //= 280; // 118 SHORT SamplesPerPixel
    ttt_MaxSampleValue  : Result:='MaxSampleValue'; //= 281; // 119 SHORT SamplesPerPixel
    ttt_XResolution  : Result:='XResolution'; //= 282; // 11A RATIONAL 1
    ttt_YResolution  : Result:='YResolution'; //= 283; // 11B RATIONAL 1
    ttt_PlanarConfiguration  : Result:='PlanarConfiguration'; //= 284; // 11C SHORT 1
    ttt_PageName  : Result:='PageName'; //= 285; // 11D ASCII
    ttt_XPosition  : Result:='XPosition'; //= 286; // 11E RATIONAL
    ttt_YPosition  : Result:='YPosition'; //= 287; // 11F RATIONAL
    ttt_FreeOffsets  : Result:='FreeOffsets'; //= 288; // 120 LONG
    ttt_FreeByteCounts  : Result:='FreeByteCounts'; //= 289; // 121 LONG
    ttt_GrayResponseUnit  : Result:='GrayResponseUnit'; //= 290; // 122 SHORT 1
    ttt_GrayResponseCurve  : Result:='GrayResponseCurve'; //= 291; // 123 SHORT 2**BitsPerSample
    ttt_T4Options  : Result:='T4Options'; //= 292; // 124 LONG 1
    ttt_T6Options  : Result:='T6Options'; //= 293; // 125 LONG 1
    ttt_ResolutionUnit  : Result:='ResolutionUnit'; //= 296; // 128 SHORT 1
    ttt_PageNumber  : Result:='PageNumber'; //= 297; // 129 SHORT 2
    ttt_TransferFunction  : Result:='TransferFunction'; //= 301; // 12D SHORT {1 or SamplesPerPixel}* 2** BitsPerSample
    ttt_Software  : Result:='Software'; //= 305; // 131 ASCII
    ttt_DateTime  : Result:='DateTime'; //= 306; // 132 ASCII 20
    ttt_Artist  : Result:='Artist'; //= 315; // 13B ASCII
    ttt_HostComputer  : Result:='HostComputer'; //= 316; // 13C ASCII
    ttt_Predictor  : Result:='Predictor'; //= 317; // 13D SHORT 1
    ttt_WhitePoint  : Result:='WhitePoint'; //= 318; // 13E RATIONAL 2
    ttt_PrimaryChromaticities  : Result:='PrimaryChromaticities'; //= 319; // 13F RATIONAL 6
    ttt_ColorMap  : Result:='ColorMap'; //= 320; // 140 SHORT 3 * (2**BitsPerSample)
    ttt_HalftoneHints  : Result:='HalftoneHints'; //= 321; // 141 SHORT 2
    ttt_TileWidth  : Result:='TileWidth'; //= 322; // 142 SHORT or LONG 1
    ttt_TileLength  : Result:='TileLength'; //= 323; // 143 SHORT or LONG 1
    ttt_TileOffsets  : Result:='TileOffsets'; //= 324; // 144 LONG TilesPerImage
    ttt_TileByteCounts  : Result:='TileByteCounts'; //= 325; // 145 SHORT or LONG TilesPerImage
    ttt_InkSet  : Result:='InkSet'; //= 332; // 14C SHORT 1
    ttt_InkNames  : Result:='InkNames'; //= 333; // 14D ASCII total number of characters in all ink name strings, including zeros
    ttt_NumberOfInks  : Result:='NumberOfInks'; //= 334; // 14E SHORT 1
    ttt_DotRange  : Result:='DotRange'; //= 336; // 150 BYTE or SHORT 2, or 2*NumberOfInks
    ttt_TargetPrinter  : Result:='TargetPrinter'; //= 337; // 151 ASCII any
    ttt_ExtraSamples  : Result:='ExtraSamples'; //= 338; // 152 BYTE number of extra compo-nents per pixel
    ttt_SampleFormat  : Result:='SampleFormat'; //= 339; // 153 SHORT SamplesPerPixel
    ttt_SMinSampleValue  : Result:='SMinSampleValue'; //= 340; // 154 Any SamplesPerPixel
    ttt_SMaxSampleValue  : Result:='SMaxSampleValue'; //= 341; // 155 Any SamplesPerPixel
    ttt_TransferRange  : Result:='TransferRange'; //= 342; // 156 SHORT 6
    ttt_JPEGTables : Result:='JPEGTables';
    ttt_JPEGProc  : Result:='JPEGProc'; //= 512; // 200 SHORT 1
    ttt_JPEGInterchangeFormat  : Result:='JPEGInterchangeFormat'; //= 513; // 201 LONG 1
    ttt_JPEGInterchangeFormatLngth  : Result:='JPEGInterchangeFormatLngth'; //= 514; // 202 LONG 1
    ttt_JPEGRestartInterval  : Result:='JPEGRestartInterval'; //= 515; // 203 SHORT 1
    ttt_JPEGLosslessPredictors  : Result:='JPEGLosslessPredictors'; //= 517; // 205 SHORT SamplesPerPixel
    ttt_JPEGPointTransforms  : Result:='JPEGPointTransforms'; //= 518; // 206 SHORT SamplesPerPixel
    ttt_JPEGQTables  : Result:='JPEGQTables'; //= 519; // 207 LONG SamplesPerPixel
    ttt_JPEGDCTables  : Result:='JPEGDCTables'; //= 520; // 208 LONG SamplesPerPixel
    ttt_JPEGACTables  : Result:='JPEGACTables'; //= 521; // 209 LONG SamplesPerPixel
    ttt_YCbCrCoefficients  : Result:='YCbCrCoefficients'; //= 529; // 211 RATIONAL 3
    ttt_YCbCrSubSampling  : Result:='YCbCrSubSampling'; //= 530; // 212 SHORT 2
    ttt_YCbCrPositioning  : Result:='YCbCrPositioning'; //= 531; // 213 SHORT 1
    ttt_ReferenceBlackWhite  : Result:='ReferenceBlackWhite'; //= 532; // 214 LONG 2*SamplesPerPixel
    ttt_Copyright  : Result:='Copyright'; //= 33432; // 8298 ASCII Any
  else
    Result:='Unknown ('+IntToStr(Tag)+')';
  end;
  // Return Name,Count and Values
  Result:=Copy(Result+'                                    ',1,36);
  Result:=Result+Format('%5d',[Count]);
  If Entry.valueType in [tvt_RATIONAL,tvt_SRATIONAL,tvt_FLOAT,tvt_DOUBLE] then
  begin
    for idx:=0 to min(20,Count-1) do
      Result:=Result+' '+FloatToStr(FloatValues[idx]);
  end
  else if Entry.valueType in [tvt_BYTE,tvt_SHORT,tvt_LONG,tvt_SBYTE,tvt_SSHORT,tvt_SLONG] then
  begin
    for idx:=0 to min(20,Count-1) do
      Result:=Result+' '+IntToStr(IntValues[idx]);
  end
  else if Entry.ValueType = tvt_ASCII then
  begin
    Result:=Result+' '+StrValue
  end
  else
    result:=result + ' Valuetype not recognized: '+IntToStr(Entry.ValueType);
end;

function TIFDItem.GetFloatValues(index: integer): double;
var
  P:Pointer;
begin
  If SmallValue then
    P:=@Values
  else
    P:=Values;
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
  case Entry.ValueType of
    tvt_RATIONAL:result:=ArrRATIONAL(P^)[index].numerator/ArrRATIONAL(P^)[index].denominator;
    tvt_SRATIONAL:result:=ArrSRATIONAL(P^)[index].numerator/ArrSRATIONAL(P^)[index].denominator;
    tvt_FLOAT:result:=ArrFLOAT(P^)[index];
    tvt_DOUBLE:result:=ArrDOUBLE(P^)[index];
  else
    Raise ETiffException.Create('Value type should be a floating-point type');
  end;
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}
end;

type
  ArrBYTE = Array[0..0] of byte;
  ArrSHORT = Array[0..0] of word;
  ArrLONG = Array[0..0] of cardinal;
  ArrSBYTE = Array[0..0] of smallint;
  ArrSSHORT = Array[0..0] of shortint;
  ArrSLONG = Array[0..0] of integer;

function TIFDItem.GetIntValues(index: integer): Integer;
var
  P:Pointer;
begin
  If SmallValue then
    P:=@Values
  else
    P:=Values;
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
  case Entry.ValueType of
    tvt_BYTE:result:=ArrBYTE(P^)[index];
    tvt_SHORT:result:=ArrSHORT(P^)[index];
    tvt_LONG:result:=ArrLONG(P^)[index];
    tvt_SBYTE:result:=ArrBYTE(P^)[index];
    tvt_SSHORT:result:=ArrSSHORT(P^)[index];
    tvt_SLONG:result:=ArrSLONG(P^)[index];
  else
    Raise ETiffException.Create('Value type should be a integer type');
  end;
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}
end;

function TIFDItem.GetStrValue: string;
var
  P:PAnsiChar;
begin
  If SmallValue then
    P:=@Values
  else
    P:=Values;
  If Entry.ValueType = tvt_ASCII then
  begin
    Result:=P;
  end
  else
    Raise ETiffException.Create('Value type should be a ascii type');
end;

procedure TIFDItem.SetFloatValues(index: integer; const Value: double);
begin
   Raise ETiffException.Create('IDF values can not be changed');
end;

procedure TIFDItem.SetIntValues(index: integer; const Value: Integer);
begin
   Raise ETiffException.Create('IDF values can not be changed');
end;

procedure TIFDItem.SetNumberFormat(const Value: TNumberFormat);
begin
  FNumberFormat := Value;
end;

procedure TIFDItem.SetStrValue(const Value: string);
begin
   Raise ETiffException.Create('IDF values can not be changed');
end;

{ TTIFFImage }

constructor TTIFFImage.Create;
begin
  inherited;
  SubImages:=TObjectList.Create;
  Data:=TMemoryStream.Create;
end;

destructor TTIFFImage.Destroy;
begin
  SubImages.Free;
  Data.Free;
  inherited;
end;

procedure TTIFFImage.DoProgress(Stage: TProgressStage; PercentDone: Byte;
  const Msg: string);
begin
  Progress(Self,Stage,PercentDone,False,Rect(0,0,0,0),Msg);
end;

function TTIFFImage.GetPageCount: integer;
begin
  result:=SubImages.Count;
end;

procedure TTIFFImage.LoadFromStream(Stream: TStream);
// Expects TIFF image on current position of stream
// does some checking and reads all subimages
var
  aFormat : Word;
  address: Cardinal;
  anIFDImage: TTiffSubImage;
  OldPosition : Cardinal;
  // TODO: set stream pointer to end of picture following reading it
  //       copy only needed data to memorystream;
begin
  // Check Tiff
  OldPosition:=Stream.Position;
  // this copy assumes that tiff image is at end of stream, rest of stream is copied to Data
  Data.CopyFrom(Stream,0);

  Data.Position:=0;
  Data.Read(aFormat,2);
  If aFormat=tiff_LittleEndian then
    NativeFormat:=true
  else if aFormat=tiff_BigEndian then
  begin
    NativeFormat:=false;
    Raise ETiffException.Create('This application does not fully support non-native TIF images')
  end
  else
    Raise ETiffException.Create('Invalid file type');
  CurrentNumberformat:=nfLittleEndian;
  Progress(Self,psStarting,0,false,Rect(0,0,0,0),'Reading properties');
  If ReadWord(Data)<>tiff_MagicNumber then
    Raise ETiffException.Create('Invalid file type');
  SubImages.Clear;
  address:=ReadCardinal(Data);
  //get all the IFD Images from the tiff document
  Data.Seek(address,soFromBeginning);
  while address<>0 do
  begin
    anIFDImage:= TTiffSubImage.Create(Self);
    anIFDImage.Startpos:=address;
    // anIFDImage.ProgressCallback := FProgessCallBK;
    address := anIFDImage.LoadFromStream(Data);

    SubImages.Add(anIFDImage);
  end;
  Progress(Self,psEnding,100,false,Rect(0,0,0,0),'Reading properties');
  If SubImages.Count>0 then
    Page:=0;
  Stream.Position:=OldPosition+Data.Position;
end;

procedure TTIFFImage.SaveToStream(Stream: TStream);
begin
  Stream.CopyFrom(Data,0);
end;

procedure TTIFFImage.SetPage(const Value: integer);
begin
  If (Value>=0) and (Value<SubImages.Count) then
  begin
    FPage := Value;
    Assign(TTiffSubImage(SubImages[FPage]).Bitmap);
  end;
end;

{ TTIFFInfo }

procedure TTIFFInfo.LoadFromStream(Stream: TStream);
// Expects TIFF image on start position of stream
// does some checking and reads all subimages
var
  aFormat : Word;
  address: Cardinal;
  anIFDImage: TTiffSubInfoImage;
  OldPosition : Cardinal;
  // TODO: set stream pointer to end of picture following reading it
  //       copy only needed data to memorystream;
begin
  // Check Tiff
  // this copy assumes that tiff image is at end of stream, rest of stream is copied to Data
  Stream.Position:=0;
  Stream.Read(aFormat,2);
  If aFormat=tiff_LittleEndian then
    NativeFormat:=true
  else if aFormat=tiff_BigEndian then
  begin
    NativeFormat:=false;
    Raise ETiffException.Create('This application does not fully support non-native TIF images')
  end
  else
    Raise ETiffException.Create('Invalid file type');
  CurrentNumberformat:=nfLittleEndian;
  Progress(Self,psStarting,0,false,Rect(0,0,0,0),'Reading properties');
  If ReadWord(Stream)<>tiff_MagicNumber then
    Raise ETiffException.Create('Invalid file type');
  SubImages.Clear;
  address:=ReadCardinal(Stream);
  //get all the IFD Images from the tiff document
  Stream.Seek(address,soFromBeginning);
  while address<>0 do
  begin
    anIFDImage:= TTiffSubInfoImage.Create(Self);
    anIFDImage.Startpos:=address;
    // anIFDImage.ProgressCallback := FProgessCallBK;
    address := anIFDImage.LoadFromStream(Stream);
    SubImages.Add(anIFDImage);
  end;
  Progress(Self,psEnding,100,false,Rect(0,0,0,0),'Reading properties');
  If SubImages.Count>0 then
    Page:=0;
end;

procedure TTIFFInfo.SaveToStream(Stream: TStream);
begin
  Raise ETiffException.Create('TTIFFInfo objects can not be saved');
end;

{ TTiffSubInfoImage }

function TTiffSubInfoImage.LoadFromStream(aStream: TStream): Integer;
begin
  // mFileStream := aStream;
  result := IFDItems.GetIFD(aStream );
  startpos:=IFDItems.StartPos;
  lastPos:=max(aStream.Position,IFDItems.LastPos);
  GetImageDescription;
  AStream.Seek(result,soFromBeginning);
end;

initialization
  TPicture.RegisterFileFormat('TIF','Tiff images',TTiffImage);
finalization
  TPicture.UnregisterGraphicClass(TTiffImage);
end.
