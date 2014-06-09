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
unit Compression;

{**************************************************************
Compression.pas
    This unit contains generic compression functions.

**************************************************************}

interface

uses Classes, SysUtils, CompressionConstants;

const DEBUG = false;
const DEBUG_IMAGE_LINE = 221;


const WHITE = true;
const BLACK = false;

type
    TBitStream = class( TMemoryStream  )
    private
      FBitPosition: Integer;
      FBitSize: Integer;
      function GetBits(idx: Integer): Boolean;
      procedure SetBits(idx: Integer; const Value: Boolean);
      function GetBinaryString: String;
      function GetBitStr(idx: Integer): String;
      procedure SetBitSize(const Value: Integer);
    public
      constructor Create;
      // Override functions to update BitSize
      function Write(const Buffer; Count: Longint): Longint; override;
      procedure SetSize(NewSize: Longint); override;
      procedure Clear;

      procedure AdjustFillOrder;
      procedure WriteBits( Value: Boolean; aCount: Integer = 1 );
      function PeekBits:word;
      procedure AdvanceBits(Count:Integer);
      function ReadBit:Boolean;
      function ReadBitChar:Char;
      function WriteBitsToNextByteMarker( Value: Boolean ):integer;

      property BitSize: Integer read FBitSize write SetBitSize;
      property BitPosition: Integer read FBitPosition write FBitPosition;
      property Bits[idx: Integer]: Boolean read GetBits write SetBits; default;

      property BitsStr[idx: Integer]: String read GetBitStr;
      // Debug functions
      procedure DumpBitStream( aFileName: String );
      property BinaryString: String read GetBinaryString;
    end;

type
    TReferenceLine = class( TBitStream )
    public
      LineWidth: Integer;
      function GetB1( anIdx: Integer; isIdxWhite: Boolean ): Integer;
      function GetB2( anIdx: Integer; isIdxWhite: Boolean ): Integer;
    end;

type
    THuffmanDecompressor = class
    private
      FCompressedBitStream: TBitStream;
      // FDecompressedBitStream: TBitStream;
      FImageWidth: Integer;
      FImageHeight: Integer;
      FHasEOL: Boolean;
      FAdvanceToByteBoundary: Boolean;

      FReferenceLine: TReferenceLine;
      FCurrentLine :  TReferenceLine;

      PixelsAreWhite: Boolean;
      PixelsDecompressed: Integer;
      BitmapRow:Pointer;
      DecompressToBitmap:Boolean;
      ScanLineInterval : Integer;
      function HuffmanCodeLookUp(isWhite: Boolean ):Boolean;
    public
      constructor create;
      destructor Destroy;override;
      procedure Decompress( aCompressedStream: TBitStream;StartingIdx: Integer );virtual;
      procedure DecompressTwoCodes( IsWhite: Boolean);
      function DecompressLine: Boolean;
      property ReferenceLine: TReferenceLine read FReferenceLine write FReferenceLine;
      property AdvanceToByteBoundary: Boolean read FAdvanceToByteBoundary write FAdvanceToByteBoundary;
      property HasEOL: Boolean read FHasEOL write FHasEOL;
      property ImageWidth: Integer read FImageWidth write FImageWidth;
      property ImageHeight: Integer read FImageHeight write FImageHeight;
      property CompressedBitStream: TBitStream read FCompressedBitStream write FCompressedBitStream;
    end;


    TCCITT4Decompressor = class(THuffmanDecompressor)
    private
      rowCount: Integer;
      isWhite :Boolean;
      Function DoModeLookup:TModeType;
      procedure ProcessPass;
      procedure ProcessHorzCodes;
      procedure ProcessVerticalMode( aMode: TModeType );
    public
      procedure Decompress( aCompressedStream: TBitStream;StartingIdx: Integer );override;
    end;

function CompareStreams( x, y: TStream ): Boolean;
function BinaryStr(const Buffer; Count: Longint): String;
procedure ShowDebugMessage( const msg: String; aLine: Integer = -1 );

procedure DecompressCCITTFax4Old( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight: Integer; rowsPerBlock: Integer  );

procedure DecompressCCITTFax4( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight:integer; StartingRow:Pointer;Interval:Integer);
procedure DecompressCCITTFax3( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight: Integer; hasEOL: Boolean; firstStrip: Boolean );
procedure DecompressModifiedHuffman( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight: Integer  );
procedure DecompressPackBits( aCompressedStream: TMemoryStream;
    bytesPerRow: Integer; ImageHeight: Integer  );

type
    ETiffException = class( Exception );


implementation

uses Windows,Dialogs;

procedure ShowDebugMessage( const msg: String; aLine: Integer = -1 );
begin
    if DEBUG then
        if aLine = -1 then
            ShowMessage( msg )
        else if ( DEBUG_IMAGE_LINE <= aLine ) then
            ShowMessage( msg );
end;


function BinaryStr(const Buffer; Count: Longint): String;
var
    aBitStream: TBitStream;
    idx: Integer;
    bitStr: String;
    byteCount: Integer;
    aByte: Byte;
begin
    aBitStream:= TBitStream.Create;
    try
        byteCount := 0;
        aBitStream.Write( Buffer, Count );
        aBitStream.Seek( 0, soFromBeginning );
        for idx:=0 to aBitStream.BitSize-1 do begin
            if (idx <> 0) and (idx and 7 = 0) then begin
                bitStr := bitStr + ' ';
                inc( byteCount );
                if byteCount mod 6 = 0 then
                    bitStr := bitStr+#13;
            end;

            if aBitStream[idx] = WHITE then
                bitStr := bitStr + '1'
            else
                bitStr := bitStr + '0';
        end;

        bitStr := bitStr + #13#13;
        byteCount := 0;
        aBitStream.Seek( 0, soFromBeginning );
        for idx:=0 to aBitStream.Size-1 do begin
            inc( byteCount );
            if byteCount mod 6 = 0 then
                bitStr := bitStr+#13;

            aBitStream.Read( aByte, 1 );
            bitStr := bitStr + format( '%x', [aByte] ) + ' ';
        end;

        result := bitStr;
    finally
        aBitStream.Free;
    end;
end;

function CompareStreams( x, y: TStream ): Boolean;
var
    xByte, yByte: Byte;
    idx: Integer;
begin
    result := false;
    if x.Size <> y.size then begin
        ShowMessage(
            'Streams have different sizes: '#13+
            'Stream X: ' + IntToStr( x.Size ) + #13+
            'Stream Y: ' + IntToStr( y.Size )
            );
        exit;
    end;

    x.Seek( 0, soFromBeginning );
    y.Seek( 0, soFromBeginning );
    for idx:=0 to x.Size-1 do begin
        x.Read( xByte, 1 );
        y.Read( yByte, 1 );
        if xByte <> yByte then begin
            ShowMessage(
                'Streams byte differs: '#13+
                'at position: ' + IntToStr( idx ) + #13+
                'byte X: ' + IntToStr( xByte ) + #13+
                'byte Y: ' + IntToStr( yByte )
            );
            exit;
        end;
    end;
    result := true;
end;


procedure DecompressCCITTFax3( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight: Integer; hasEOL: Boolean; firstStrip: Boolean );
{
	This procedure takes a stream that has been compressed according to the
    CCITT Group 3 1-dimensional fax encoding and decompresses it. This algorithm
    uses the same huffman tables the huffman compression uses. The fax3 compression
    adds words for bof, eof and such.
}
var
  Decompressor:THuffmanDecompressor;
begin
  Decompressor:=THuffmanDecompressor.create;
  Decompressor.ImageWidth:=imageWidth;
  Decompressor.ImageHeight:=imageHeight;
  Decompressor.AdvanceToByteBoundary:=False;
  Decompressor.HasEOL:=hasEOL;
  Decompressor.Decompress(aCompressedStream,16);
  Decompressor.Free;
end;

procedure DecompressCCITTFax4( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight:integer; StartingRow:Pointer;Interval:Integer);
var
  Decompressor:TCCITT4Decompressor;
begin
  Decompressor:=TCCITT4Decompressor.create;
  Decompressor.ImageWidth:=imageWidth;
  Decompressor.ImageHeight:=imageHeight;
  Decompressor.DecompressToBitmap:=True;
  Decompressor.BitmapRow:=StartingRow;
  Decompressor.ScanLineInterval:=Interval;
  Decompressor.Decompress(aCompressedStream,0);
  Decompressor.Free;
end;

procedure DecompressCCITTFax4Old( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight: Integer; rowsPerBlock: Integer  );
{
	This procedure takes a stream that has been compressed according to the
    CCITT Group 4 2-dimensional fax encoding and decompresses it. This algorithm
    uses the same huffman tables the huffman compression uses. The fax4 compression
    adds EOL markers
}
var
  Decompressor:TCCITT4Decompressor;
begin
  Decompressor:=TCCITT4Decompressor.create;
  Decompressor.ImageWidth:=imageWidth;
  Decompressor.ImageHeight:=imageHeight;
  Decompressor.DecompressToBitmap:=False;
  Decompressor.Decompress(aCompressedStream,0);
  Decompressor.Free;
end;

procedure DecompressModifiedHuffman( aCompressedStream: TBitStream;
    const imageWidth: Integer; const imageHeight: Integer  );
{
This procedure takes a stream that has been compressed according to the
    CCITT Group 3 1-dimensional huffman encoding and decompresses it. The
    procedure reads bits off of the stream and passes them to a huffman code
    table. A match is eventually found and this cooresponds to a particular
    run-length of either black or white pixels. The color is switched once a
    key in the table has been located.
}
var
  Decompressor:THuffmanDecompressor;
begin
  Decompressor:=THuffmanDecompressor.create;
  Decompressor.ImageWidth:=imageWidth;
  Decompressor.ImageHeight:=imageHeight;
  Decompressor.HasEOL:=false;
  Decompressor.AdvanceToByteBoundary:=true;
  Decompressor.Decompress(aCompressedStream,0);
  Decompressor.Free;
end;

procedure DecompressPackBits( aCompressedStream: TMemoryStream;
    bytesPerRow: Integer; ImageHeight: Integer  );
{
    PackBits is a simple runlength encoding first used by Apple
    computer. The parameters are the compressed stream and size of
    the uncompressed stream. The compressed stream will
    contain the decompressed data after the procedure returns.
}
var
    idx: LongWord;
    byteIdx: Integer;
    signedByte: ShortInt;
    aPixVal: byte;
    aBuffer: array[0..127] of byte;
    aDecompressedStream: TBitStream;
    rowIdx: Integer;
begin
    aDecompressedStream := TBitStream.Create;
    try
        aDecompressedStream.Seek( 0, soFromBeginning );
        aCompressedStream.Seek( 0, soFromBeginning );

        //decompress each row
        for rowIdx:=0 to ImageHeight-1 do begin
            byteIdx:=0;
            while byteIdx < bytesPerRow do begin
                aCompressedStream.Read( signedByte, 1 );
                if signedByte >= 0 then begin
                    //read the next (signedByte+1) bytes literally
                    aCompressedStream.Read( aBuffer, signedByte+1 );
                    for idx:=0 to signedByte do begin
                        aDecompressedStream.Write( aBuffer[idx], 1 );
                        inc( byteIdx );
                        if byteIdx mod bytesPerRow = 0 then
                            break;
                    end;
                end
                else begin
                    //write run length of abs( byte )
                    aCompressedStream.Read( aPixVal, 1 );
                    for idx:=0 to -1*signedByte do begin
                        aDecompressedStream.Write( aPixVal, 1 );
                        inc( byteIdx );
                        if byteIdx mod bytesPerRow = 0 then
                            break;
                    end;
                end;
            end;
        end;
        aCompressedStream.Clear;
        aCompressedStream.SetSize( aDecompressedStream.Size );
        aDecompressedStream.Seek( 0, soFromBeginning );
        aCompressedStream.CopyFrom( aDecompressedStream, aDecompressedStream.Size );
    finally
         aDecompressedStream.Free;
    end;
end;

{ TBitStream }

constructor TBitStream.Create;
begin
  Inherited Create;
  FBitPosition := 0;
  FBitSize := 0;
end;

procedure TBitStream.DumpBitStream(aFileName: String);
var
    aTextFile: TextFile;
    idx: Integer;
    aLine: String;
    lineCount: Integer;
begin
    AssignFile( aTextFile, aFileName );
    ReWrite( aTextFile );
    lineCount := 0;
    for idx:=0 to BitSize-1 do begin
        if Bits[idx] = BLACK then
            aLine := aLine + '1'
        else
            aLine := aLine + '0';

        if Length( aLine ) = 8 then
            aLine := aLine + ' ';

        if Length( aLine ) = 17 then begin
            Inc( lineCount );
            WriteLn( aTextFile, IntToStr( LineCount ) + ': ' + #9 + aLine );
            aLine := '';
        end;
    end;
    if Length( aLine ) > 0 then
        WriteLn( aTextFile, aLine );

    CloseFile( aTextFile );
end;

function TBitStream.GetBinaryString: String;
var
    idx: Integer;
begin
    for idx:=0 to BitSize-1 do begin
        if Bits[idx] then
            result := result + '0'
        else
            result := result + '1';
    end;
end;

function TBitStream.GetBits(idx: Integer): Boolean;
begin
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
  result :=(PByteArray(Memory)[idx div 8] and (128 Shr(idx and 7))) <> 0;
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}

end;

function TBitStream.GetBitStr(idx: Integer): String;
begin
    if Bits[idx] then
        result := '1'
    else
        result := '0';
end;

procedure TBitStream.SetBits(idx: Integer; const Value: Boolean);
var
  byteIdx: Integer;
  aByte: Byte;
begin
  byteIdx := idx div 8;

{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
  If byteIdx>=Size then
    aByte:=0
  else
    aByte:=PByteArray(Memory)[ByteIdx];

  If Value then
    aByte:=aByte or (128 Shr (idx and 7))
  else
    aByte:=aByte and (Not (128 Shr (idx and 7)));

  If byteIdx>=Size then
  begin
    Seek( byteIdx, 0 );
    Write( aByte, 1 );
  end
  else
    PByteArray(Memory)[ByteIdx]:=aByte;
  FBitPosition:=idx;
  if idx>=FBitSize then
    FBitSize:=idx+1;
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}
end;

const DoubleBits : Array[1..4] of Byte = ($81,$42,$24,$18);

procedure TBitStream.AdjustFillOrder;
var
  idx:Integer;

  function AdjustByte(aByte:byte):Byte;
  var i : integer;
  begin
    for i:=1 to 4 do
    begin
      if (abyte and DoubleBits[i] <>0) and (abyte and DoubleBits[i] <>DoubleBits[i]) then
        aByte:=aByte xor Doublebits[i];
    end;
    result:=aByte;
  end;

begin
  for idx:=0 to Size-1 do
  begin
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
    PByteArray(Memory)[Idx]:=AdjustByte(PByteArray(Memory)[Idx]);
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}

  end;
end;

procedure TBitStream.WriteBits( Value: Boolean; aCount: Integer = 1 );
var
  aByte: byte;
begin
  if FBitPosition >= 2147483646 then
      ShowMessage( 'Bounds Error...' );
  Seek(FBitPosition div 8,soFromBeginning);
  if aCount>=(8-(FBitPosition and 7)) then begin
    Dec(aCount,WriteBitsToNextByteMarker(Value));
    Seek(FBitPosition div 8,soFromBeginning);
    If aCount>= 8 then
    begin
      If Value then
        Write(arTrueBuffer[0],aCount div 8)
      else
        Write(arFalseBuffer[0],aCount div 8);
      FBitPosition:=Position*8;
      aCount:=aCount and 7;
    end;
  end;
  if aCount>0 then
  begin
    if ((FBitPosition and 7) = 0) and (Position=Size) then
    begin
      // at most 8 more bits are needed at the very end of the stream
      // so it doesn't matter if we overwrite a few bits at the end
      if Value then
        aByte:=$FF
      else
        aByte:=0;
      Write(aByte,1);
      inc(FBitPosition,aCount);
    end
    else begin
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
      // Set number of needed bits as left-most bits
      aByte:=255 shl (8 - aCount);
      // move the bits to the right position in the byte
      aByte:=aByte shr (FBitPosition and 7);
      // The byte for these bits is already in the stream, we don't need Write here
      if Value then
        PByteArray(Memory)[Position]:=PByteArray(Memory)[Position] or aByte
      else
        PByteArray(Memory)[Position]:=PByteArray(Memory)[Position] and not aByte;
      Inc(FBitPosition,aCount);
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}
    end;
  end;
end;

function TBitStream.WriteBitsToNextByteMarker(Value: Boolean ):integer;
var
  bitsLeft: Integer;
  aByte : Byte;
begin
  if FBitPosition and 7 > 0 then
  begin
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
    aByte:=PByteArray(Memory)[FBitPosition div 8];
    bitsLeft := 8-(FBitPosition and 7);
    If Value then
      // mask the existing bits and set the rest
      aByte:=aByte or not (255 shl bitsleft)
    else
      // mask the existing bits and clear the rest
      aByte:=aByte and (255 shl bitsleft);
    PByteArray(Memory)[FBitPosition div 8]:=aByte;
    Inc(FBitPosition,bitsLeft);
    result:=bitsLeft;
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}
  end
  else
    result:=0;
end;

function TBitStream.ReadBit: Boolean;
begin
  Result:=Bits[FBitPosition];
  Inc(FBitPosition);
end;

function TBitStream.ReadBitChar: Char;
begin
  If ReadBit then
    result:='1'
  else
    result:='0';
end;

function TBitStream.Write(const Buffer; Count: Integer): Longint;
var
  TempSize:Integer;
begin
  TempSize:=Size;
  result:=inherited Write(Buffer,Count);
  If TempSize<>Size then
    BitSize:=Size*8;
end;

procedure TBitStream.SetBitSize(const Value: Integer);
begin
  FBitSize := Value;
  Inherited SetSize((FBitSize + 7) div 8);
end;

procedure TBitStream.SetSize(NewSize: Integer);
begin
  inherited;
  FBitSize:=NewSize*8;
end;

procedure TBitStream.clear;
begin
  inherited Clear;
  FBitPosition:=0;
  fBitSize:=0;
end;

procedure TBitStream.AdvanceBits(Count: Integer);
begin
  Inc(FBitPosition,Count);
end;

function TBitStream.PeekBits: word;
{ retrieves 16 bits }
var
  Bytes:Cardinal;
begin
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
  Bytes:=PByteArray(Memory)[FBitPosition div 8] shl 16+
         PByteArray(Memory)[(FBitPosition div 8)+1] shl 8+
         PByteArray(Memory)[(FBitPosition div 8)+2];
  // Move bits to lower bit of variable
  // Let the compiler chop off the rest of the bits by converting to 2-byte variable
  Result:=(Bytes shr (8-(FBitPosition and 7)));
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}
end;

{ TReferenceLine }

function TReferenceLine.GetB1( anIdx: Integer; isIdxWhite: Boolean ): Integer;
var
  CompVal:Word;
  PeekedBits : Word;
  CurIdx:Integer;
  CheckP:Word;
begin
  //set same color found if the idx = 0 and the color is black
  //this is a boundary condition for the algorithm.
  if (anIdx<>0) or (Bits[0]<>BLACK) then
    // look for start of run of same color
    While (anIdx<LineWidth) and (Bits[anIdx]<>isIdxWhite) do
      Inc(anIdx);
  // look where run of same color ends
  BitPosition:=anIdx;
  // Start by comparing 16 bits at a time
  If isIdxWhite then
    CompVal:=$FFFF
  else
    CompVal:=$0000;
  PeekedBits:=PeekBits;
  While (PeekedBits=CompVal) and (anIdx<(LineWidth-15)) do
  begin
    Inc(anIdx,16);
    AdvanceBits(16);
    PeekedBits:=PeekBits;
  end;
  // Maximum of 15 bits to check now, and they're all in PeekedBits
  // Mask increasing number of bits for comparison, starting with highest bit
  curIdx:=15;
  CheckP:=$8000;
  While (anIdx<LineWidth) and ((PeekedBits and CheckP) =(CompVal and CheckP)) do
  begin
    inc(anIdx);
    Dec(curIdx);
{Turn range checking off if it is on and}
{remember the range checking state}
{$IFOPT R+}
  {$DEFINE CKRANGE}
  {$R-}
{$ENDIF}
    CheckP:=$FFFF shl curIdx;
{Turn range checking back on if it was on when we started}
{$IFDEF CKRANGE}
  {$UNDEF CKRANGE}
  {$R+}
{$ENDIF}
  end;
  result := anIdx;
end;

function TReferenceLine.GetB2( anIdx: Integer; isIdxWhite: Boolean ): Integer;
begin
  anIdx := GetB1( anIdx, isIdxWhite );
  While (anIdx<LineWidth) and (Bits[anIdx]<>isIDXWhite) do
    Inc(anIdx);
  result:=anIdx;
end;

{ THuffmanDecompressor }

constructor THuffmanDecompressor.create;
begin
  FReferenceLine:=TReferenceLine.Create;
  FCurrentLine:=TReferenceLine.Create;
end;

procedure THuffmanDecompressor.Decompress( aCompressedStream: TBitStream;
                                           StartingIdx: Integer );
var
  currentRow: Integer;
begin
  FCompressedBitStream := TBitStream.Create;
  FCompressedBitStream.CopyFrom(aCompressedStream,0);
  aCompressedStream.Clear;
  aCompressedStream.SetSize( ImageHeight * ((ImageWidth + 7 )div 8 ) );

  FCurrentLine.LineWidth := ImageWidth;
  try
    FCompressedBitstream.BitPosition:=StartingIdx;
    currentRow := 0;
    while currentRow < ImageHeight do begin
      FCurrentLine.BitPosition:=0;
      DecompressLine;
      aCompressedStream.Copyfrom(FCurrentLine,0); // could copy to a bitmap.scanline immediately here
      Inc( currentRow );
    end;
  finally
    FCompressedBitStream.Free;
  end;

end;

function THuffmanDecompressor.DecompressLine: Boolean;
var
  isWhite: Boolean;
  isTerminateCodeFound: Boolean;
  lastColorIsWhite: Boolean;
  blackEOL: String;
  whiteEOLCount: Integer;
  FCurrentWidth : Integer;
begin
  isWhite := true;
  FCurrentWidth := 0;
  isTerminateCodeFound := false;
  lastColorIsWhite := false;

  while ( FCurrentWidth < FImageWidth ) or not isTerminateCodeFound do begin
    PixelsAreWhite := isWhite;
    If HuffmanCodeLookUp( isWhite ) then begin
      isWhite:=not isWhite;
      if (FCurrentWidth+PixelsDecompressed) = FImageWidth then
        isTerminateCodeFound := True;
    end;
    FCurrentLine.WriteBits(PixelsAreWhite,PixelsDecompressed);
    lastColorIsWhite := PixelsAreWhite;
    Inc( FCurrentWidth, PixelsDecompressed );
  end;
  result := FCurrentWidth = FImageWidth;
  if result then
    FCurrentLine.WriteBitsToNextByteMarker( WHITE );

  if HasEOL and result then begin
    if lastColorIsWhite then begin
      whiteEOLCount := 0;
      while ( FCompressedBitStream.ReadBit = WHITE ) and
            ( FCompressedBitStream.BitPosition < FCompressedBitStream.BitSize ) and
            ( whiteEOLCount < 20 ) do begin
          Inc( whiteEOLCount );
      end;
    end
    else begin
      while ( Length( blackEOL ) < 8 ) and ( Length( blackEOL ) mod 4 = 0 ) do begin
        blackEOL := blackEOL + FCompressedBitStream.ReadBitChar;
        if Length( blackEOL ) > 15 then begin
          ShowMessage( 'Black EOL Error' );
          result := false;
          break;
        end;
      end;
    end;
  end;

  if FAdvanceToByteBoundary and result then
    FCompressedBitStream.BitPosition:=((FCompressedBitStream.BitPosition + 7) div 8) * 8;

  if not result then
      raise ETiffException.Create( 'THuffmanDecompressor.DecompressLine: ' +
          'Current line width: ' + IntToStr( FCurrentWidth ) + ' does not match image ' +
          ' line width: ' + IntToStr( ImageWidth ) );
end;

procedure THuffmanDecompressor.DecompressTwoCodes(IsWhite: Boolean);
var
  TerminateCodeCount: Integer;
begin
  terminateCodeCount := 0;
  while terminateCodeCount < 2 do begin
    If HuffmanCodeLookUp(isWhite)then begin
      FCurrentLine.WriteBits(isWhite,PixelsDecompressed);
      isWhite:=not isWhite;
      Inc( terminateCodeCount );
    end
    else
      FCurrentLine.WriteBits(isWhite,PixelsDecompressed);
  end;
end;

destructor THuffmanDecompressor.Destroy;
begin
  FReferenceLine.Free;
  FCurrentLine.Free;
  inherited;
end;

function THuffmanDecompressor.HuffmanCodeLookUp(isWhite: Boolean):boolean;
var
  code:Word;
  codeIdx: Integer;
  isCode:Boolean;
begin
  IsCode:=false;
  Code:=FCompressedBitStream.PeekBits;
  codeIdx:=-1;
  While Not IsCode and (CodeIdx<104) do
  begin
    Inc(codeIdx);
    If (Code and HuffmanCodes[isWhite,CodeIdx].Mask)= HuffmanCodes[isWhite,CodeIdx].Code then
       isCode:=true;
  end;
  if isCode then
  begin
    FCompressedBitStream.AdvanceBits(HuffmanCodes[isWhite,codeIdx].CodedLen);
    PixelsDecompressed := HuffmanCodes[isWhite,codeIdx].DecodedLen;
    result:= codeIdx <= 63;
  end
  else begin
    raise ETiffException.Create( 'Bit String not found: $'+IntToHex(code,4));
  end;
end;

{ TCCITT4Decompressor }

procedure TCCITT4Decompressor.Decompress(aCompressedStream: TBitStream;
  StartingIdx: Integer);
begin
  FCompressedBitStream:=TBitStream.Create;
  FCompressedBitStream.CopyFrom(aCompressedStream,0);
  If Not DecompressToBitmap then
  begin
    aCompressedStream.Clear;
    aCompressedStream.SetSize( ImageHeight * ( (ImageWidth + 7 )div 8 ) );
  end;

  FReferenceLine.LineWidth := ImageWidth;
  FCurrentLine.LineWidth := ImageWidth;
  FReferenceLine.WriteBits(WHITE, ImageWidth );

  rowCount := 0;
  try
    isWhite:=true;
    FCurrentLine.BitPosition := 0;

    While FCompressedBitStream.BitPosition < FCompressedBitStream.BitSize do
    begin
      case DoModeLookUp of
        mtPass: ProcessPass;
        mtHorz: ProcessHorzCodes;
        mtV0: ProcessVerticalMode( mtV0 );
        mtVL1: ProcessVerticalMode( mtVL1 );
        mtVL2: ProcessVerticalMode( mtVL2 );
        mtVL3: ProcessVerticalMode( mtVL3 );
        mtVR1: ProcessVerticalMode( mtVR1 );
        mtVR2: ProcessVerticalMode( mtVR2 );
        mtVR3: ProcessVerticalMode( mtVR3 );
        mtExt: raise ETiffException.Create('Extension modes not supported for G4 compression.' );
        mtNone:raise ETiffException.Create('Mode lookup not succeeded, invalid format' );
      end;
      if ( FCurrentLine.BitPosition >= imageWidth ) then begin
        FCurrentLine.WriteBitsToNextByteMarker(WHITE);
        If DecompressToBitmap then
        begin
          Move(FCurrentLine.Memory^,BitmapRow^,FCurrentline.Size);
          Inc(Integer(BitmapRow),ScanLineInterval);
        end
        else begin
          aCompressedStream.Copyfrom(FCurrentLine,0); // could copy to a bitmap.scanline immediately here
        end;
        Inc(rowcount);
        if rowcount=ImageHeight then
          break;
        //reset the reference lines
        FReferenceLine.LoadFromStream(FCurrentLine);
        FReferenceLine.Seek( 0, soFromBeginning );
        FCurrentLine.BitPosition:=0;
        isWhite:=true;
      end;
    end;
    Assert(rowCount=imageHeight,'Rows finished not equal to image rows.');
  finally
    FCompressedBitStream.Free;
  end;
end;

function TCCITT4Decompressor.DoModeLookup: TModeType;
var
  code:Word;
  codeIdx: TModeType;
begin
  { Handling mtV0 as an exception did not improve times
  If not FCompressedBitStream[FCompressedBitStream.BitPosition] then
  begin
    FCompressedBitStream.AdvanceBits(1);
    Result:=mtV0;
  end
  else begin}
  Code:=FCompressedBitStream.PeekBits;
  for CodeIdx:=mtPass to mtExt do
  begin
    If (Code and LookupCodes[CodeIdx].Mask)= LookUpCodes[CodeIdx].Code then
    begin
      FCompressedBitStream.AdvanceBits(LookupCodes[codeIdx].CodedLen);
      result:= CodeIdx;
      Exit;
    end;
  end;
  result:=mtNone
end;

procedure TCCITT4Decompressor.ProcessHorzCodes;
begin
  DecompressTwoCodes(isWhite);
end;

procedure TCCITT4Decompressor.ProcessPass;
var
  passIdx: Integer;
  pixCount: Integer;
begin

  passIdx := FReferenceLine.GetB2( FCurrentLine.BitPosition, isWhite );

  pixCount := Abs( passIdx - FCurrentLine.BitPosition );

  FCurrentLine.WriteBits( isWhite, pixCount );
  {ShowDebugMessage( 'Total Pixels Decompressed: ' +
      IntToStr( decompressedBitCount )
      + ' Current Row Total: ' + IntToStr( currentWidth )
      , rowCount );}

end;

procedure TCCITT4Decompressor.ProcessVerticalMode(aMode: TModeType);
var
  b1: Integer;
  newPos: Integer;
  pixCount: Integer;
begin
  newPos := 0;

  b1 := FReferenceLine.GetB1( FCurrentLine.BitPosition, isWhite );

  case aMode of
    mtV0: newPos := b1;
    mtVL1: newPos := b1 - 1;
    mtVL2: newPos := b1 - 2;
    mtVL3: newPos := b1 - 3;
    mtVR1: newPos := b1 + 1;
    mtVR2: newPos := b1 + 2;
    mtVR3: newPos := b1 + 3;
  end;

  pixCount := abs( newPos - FCurrentLine.BitPosition );

  //set v0 = 1 when the current width is greater than zero.
  //pixCount will often be zero at the first element of a line.
  //The first pixCount is always zero when the first image element is
  //black.
  if ( pixCount = 0 ) and ( aMode = mtV0 ) and ( FCurrentLine.BitPosition <> 0 ) then
    pixCount := 1;
  //make sure not to write too many bits to the buffer
  if ( pixCount + FCurrentLine.BitPosition ) > ImageWidth then
    PixCount:=ImageWidth-FCurrentLine.BitPosition;
  FCurrentLine.WriteBits( isWhite, pixCount );
  isWhite := not isWhite;

  {ShowDebugMessage( 'Total Pixels Decompressed: ' +
    IntToStr( decompressedBitCount )
    + ' Current Row Total: ' + IntToStr( currentWidth )
    , rowCount );}
end;

end.
