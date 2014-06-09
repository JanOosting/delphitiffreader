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
unit CompressionConstants;

interface

//CCITT Group 3 Huffman codes
type
  TCodeRec = record
    Code : word;
    Mask : word;
    CodedLen : word;
    DecodedLen : word;
  end;

const EOL_CODE = 91;

const
  HuffmanCodes : array[False..true,0..104] of TCodeRec = (
   ((Code: $0DC0;Mask: $FFC0;CodedLen:10;DecodedLen:    0 ),    // Black 0
    (Code: $4000;Mask: $E000;CodedLen: 3;DecodedLen:    1 ),    // Black 1
    (Code: $C000;Mask: $C000;CodedLen: 2;DecodedLen:    2 ),    // Black 2
    (Code: $8000;Mask: $C000;CodedLen: 2;DecodedLen:    3 ),    // Black 3
    (Code: $6000;Mask: $E000;CodedLen: 3;DecodedLen:    4 ),    // Black 4
    (Code: $3000;Mask: $F000;CodedLen: 4;DecodedLen:    5 ),    // Black 5
    (Code: $2000;Mask: $F000;CodedLen: 4;DecodedLen:    6 ),    // Black 6
    (Code: $1800;Mask: $F800;CodedLen: 5;DecodedLen:    7 ),    // Black 7
    (Code: $1400;Mask: $FC00;CodedLen: 6;DecodedLen:    8 ),    // Black 8
    (Code: $1000;Mask: $FC00;CodedLen: 6;DecodedLen:    9 ),    // Black 9
    (Code: $0800;Mask: $FE00;CodedLen: 7;DecodedLen:   10 ),    // Black 10
    (Code: $0A00;Mask: $FE00;CodedLen: 7;DecodedLen:   11 ),    // Black 11
    (Code: $0E00;Mask: $FE00;CodedLen: 7;DecodedLen:   12 ),    // Black 12
    (Code: $0400;Mask: $FF00;CodedLen: 8;DecodedLen:   13 ),    // Black 13
    (Code: $0700;Mask: $FF00;CodedLen: 8;DecodedLen:   14 ),    // Black 14
    (Code: $0C00;Mask: $FF80;CodedLen: 9;DecodedLen:   15 ),    // Black 15
    (Code: $05C0;Mask: $FFC0;CodedLen:10;DecodedLen:   16 ),    // Black 16
    (Code: $0600;Mask: $FFC0;CodedLen:10;DecodedLen:   17 ),    // Black 17
    (Code: $0200;Mask: $FFC0;CodedLen:10;DecodedLen:   18 ),    // Black 18
    (Code: $0CE0;Mask: $FFE0;CodedLen:11;DecodedLen:   19 ),    // Black 19
    (Code: $0D00;Mask: $FFE0;CodedLen:11;DecodedLen:   20 ),    // Black 20
    (Code: $0D80;Mask: $FFE0;CodedLen:11;DecodedLen:   21 ),    // Black 21
    (Code: $06E0;Mask: $FFE0;CodedLen:11;DecodedLen:   22 ),    // Black 22
    (Code: $0500;Mask: $FFE0;CodedLen:11;DecodedLen:   23 ),    // Black 23
    (Code: $02E0;Mask: $FFE0;CodedLen:11;DecodedLen:   24 ),    // Black 24
    (Code: $0300;Mask: $FFE0;CodedLen:11;DecodedLen:   25 ),    // Black 25
    (Code: $0CA0;Mask: $FFF0;CodedLen:12;DecodedLen:   26 ),    // Black 26
    (Code: $0CB0;Mask: $FFF0;CodedLen:12;DecodedLen:   27 ),    // Black 27
    (Code: $0CC0;Mask: $FFF0;CodedLen:12;DecodedLen:   28 ),    // Black 28
    (Code: $0CD0;Mask: $FFF0;CodedLen:12;DecodedLen:   29 ),    // Black 29
    (Code: $0680;Mask: $FFF0;CodedLen:12;DecodedLen:   30 ),    // Black 30
    (Code: $0690;Mask: $FFF0;CodedLen:12;DecodedLen:   31 ),    // Black 31
    (Code: $06A0;Mask: $FFF0;CodedLen:12;DecodedLen:   32 ),    // Black 32
    (Code: $06B0;Mask: $FFF0;CodedLen:12;DecodedLen:   33 ),    // Black 33
    (Code: $0D20;Mask: $FFF0;CodedLen:12;DecodedLen:   34 ),    // Black 34
    (Code: $0D30;Mask: $FFF0;CodedLen:12;DecodedLen:   35 ),    // Black 35
    (Code: $0D40;Mask: $FFF0;CodedLen:12;DecodedLen:   36 ),    // Black 36
    (Code: $0D50;Mask: $FFF0;CodedLen:12;DecodedLen:   37 ),    // Black 37
    (Code: $0D60;Mask: $FFF0;CodedLen:12;DecodedLen:   38 ),    // Black 38
    (Code: $0D70;Mask: $FFF0;CodedLen:12;DecodedLen:   39 ),    // Black 39
    (Code: $06C0;Mask: $FFF0;CodedLen:12;DecodedLen:   40 ),    // Black 40
    (Code: $06D0;Mask: $FFF0;CodedLen:12;DecodedLen:   41 ),    // Black 41
    (Code: $0DA0;Mask: $FFF0;CodedLen:12;DecodedLen:   42 ),    // Black 42
    (Code: $0DB0;Mask: $FFF0;CodedLen:12;DecodedLen:   43 ),    // Black 43
    (Code: $0540;Mask: $FFF0;CodedLen:12;DecodedLen:   44 ),    // Black 44
    (Code: $0550;Mask: $FFF0;CodedLen:12;DecodedLen:   45 ),    // Black 45
    (Code: $0560;Mask: $FFF0;CodedLen:12;DecodedLen:   46 ),    // Black 46
    (Code: $0570;Mask: $FFF0;CodedLen:12;DecodedLen:   47 ),    // Black 47
    (Code: $0640;Mask: $FFF0;CodedLen:12;DecodedLen:   48 ),    // Black 48
    (Code: $0650;Mask: $FFF0;CodedLen:12;DecodedLen:   49 ),    // Black 49
    (Code: $0520;Mask: $FFF0;CodedLen:12;DecodedLen:   50 ),    // Black 50
    (Code: $0530;Mask: $FFF0;CodedLen:12;DecodedLen:   51 ),    // Black 51
    (Code: $0240;Mask: $FFF0;CodedLen:12;DecodedLen:   52 ),    // Black 52
    (Code: $0370;Mask: $FFF0;CodedLen:12;DecodedLen:   53 ),    // Black 53
    (Code: $0380;Mask: $FFF0;CodedLen:12;DecodedLen:   54 ),    // Black 54
    (Code: $0270;Mask: $FFF0;CodedLen:12;DecodedLen:   55 ),    // Black 55
    (Code: $0280;Mask: $FFF0;CodedLen:12;DecodedLen:   56 ),    // Black 56
    (Code: $0580;Mask: $FFF0;CodedLen:12;DecodedLen:   57 ),    // Black 57
    (Code: $0590;Mask: $FFF0;CodedLen:12;DecodedLen:   58 ),    // Black 58
    (Code: $02B0;Mask: $FFF0;CodedLen:12;DecodedLen:   59 ),    // Black 59
    (Code: $02C0;Mask: $FFF0;CodedLen:12;DecodedLen:   60 ),    // Black 60
    (Code: $05A0;Mask: $FFF0;CodedLen:12;DecodedLen:   61 ),    // Black 61
    (Code: $0660;Mask: $FFF0;CodedLen:12;DecodedLen:   62 ),    // Black 62
    (Code: $0670;Mask: $FFF0;CodedLen:12;DecodedLen:   63 ),    // Black 63
    (Code: $03C0;Mask: $FFC0;CodedLen:10;DecodedLen:   64 ),    // Black 64
    (Code: $0C80;Mask: $FFF0;CodedLen:12;DecodedLen:  128 ),    // Black 65
    (Code: $0C90;Mask: $FFF0;CodedLen:12;DecodedLen:  192 ),    // Black 66
    (Code: $05B0;Mask: $FFF0;CodedLen:12;DecodedLen:  256 ),    // Black 67
    (Code: $0330;Mask: $FFF0;CodedLen:12;DecodedLen:  320 ),    // Black 68
    (Code: $0340;Mask: $FFF0;CodedLen:12;DecodedLen:  384 ),    // Black 69
    (Code: $0350;Mask: $FFF0;CodedLen:12;DecodedLen:  448 ),    // Black 70
    (Code: $0360;Mask: $FFF8;CodedLen:13;DecodedLen:  512 ),    // Black 71
    (Code: $0368;Mask: $FFF8;CodedLen:13;DecodedLen:  576 ),    // Black 72
    (Code: $0250;Mask: $FFF8;CodedLen:13;DecodedLen:  640 ),    // Black 73
    (Code: $0258;Mask: $FFF8;CodedLen:13;DecodedLen:  704 ),    // Black 74
    (Code: $0260;Mask: $FFF8;CodedLen:13;DecodedLen:  768 ),    // Black 75
    (Code: $0268;Mask: $FFF8;CodedLen:13;DecodedLen:  832 ),    // Black 76
    (Code: $0390;Mask: $FFF8;CodedLen:13;DecodedLen:  896 ),    // Black 77
    (Code: $0398;Mask: $FFF8;CodedLen:13;DecodedLen:  960 ),    // Black 78
    (Code: $03A0;Mask: $FFF8;CodedLen:13;DecodedLen: 1024 ),    // Black 79
    (Code: $03A8;Mask: $FFF8;CodedLen:13;DecodedLen: 1088 ),    // Black 80
    (Code: $03B0;Mask: $FFF8;CodedLen:13;DecodedLen: 1152 ),    // Black 81
    (Code: $03B8;Mask: $FFF8;CodedLen:13;DecodedLen: 1216 ),    // Black 82
    (Code: $0290;Mask: $FFF8;CodedLen:13;DecodedLen: 1280 ),    // Black 83
    (Code: $0298;Mask: $FFF8;CodedLen:13;DecodedLen: 1344 ),    // Black 84
    (Code: $02A0;Mask: $FFF8;CodedLen:13;DecodedLen: 1408 ),    // Black 85
    (Code: $02A8;Mask: $FFF8;CodedLen:13;DecodedLen: 1472 ),    // Black 86
    (Code: $02D0;Mask: $FFF8;CodedLen:13;DecodedLen: 1536 ),    // Black 87
    (Code: $02D8;Mask: $FFF8;CodedLen:13;DecodedLen: 1600 ),    // Black 88
    (Code: $0320;Mask: $FFF8;CodedLen:13;DecodedLen: 1664 ),    // Black 89
    (Code: $0328;Mask: $FFF8;CodedLen:13;DecodedLen: 1728 ),    // Black 90
    (Code: $0000;Mask: $FFE0;CodedLen:11;DecodedLen:    0 ),    // Black 91
    (Code: $0100;Mask: $FFE0;CodedLen:11;DecodedLen: 1792 ),    // Black 92
    (Code: $0180;Mask: $FFE0;CodedLen:11;DecodedLen: 1856 ),    // Black 93
    (Code: $01A0;Mask: $FFE0;CodedLen:11;DecodedLen: 1920 ),    // Black 94
    (Code: $0120;Mask: $FFF0;CodedLen:12;DecodedLen: 1984 ),    // Black 95
    (Code: $0130;Mask: $FFF0;CodedLen:12;DecodedLen: 2048 ),    // Black 96
    (Code: $0140;Mask: $FFF0;CodedLen:12;DecodedLen: 2112 ),    // Black 97
    (Code: $0150;Mask: $FFF0;CodedLen:12;DecodedLen: 2176 ),    // Black 98
    (Code: $0160;Mask: $FFF0;CodedLen:12;DecodedLen: 2240 ),    // Black 99
    (Code: $0170;Mask: $FFF0;CodedLen:12;DecodedLen: 2304 ),    // Black 100
    (Code: $01C0;Mask: $FFF0;CodedLen:12;DecodedLen: 2368 ),    // Black 101
    (Code: $01D0;Mask: $FFF0;CodedLen:12;DecodedLen: 2432 ),    // Black 102
    (Code: $01E0;Mask: $FFF0;CodedLen:12;DecodedLen: 2496 ),    // Black 103
    (Code: $01F0;Mask: $FFF0;CodedLen:12;DecodedLen: 2560 )),   // Black 104
   ((Code: $3500;Mask: $FF00;CodedLen: 8;DecodedLen:    0 ),    // White 0
    (Code: $1C00;Mask: $FC00;CodedLen: 6;DecodedLen:    1 ),    // White 1
    (Code: $7000;Mask: $F000;CodedLen: 4;DecodedLen:    2 ),    // White 2
    (Code: $8000;Mask: $F000;CodedLen: 4;DecodedLen:    3 ),    // White 3
    (Code: $B000;Mask: $F000;CodedLen: 4;DecodedLen:    4 ),    // White 4
    (Code: $C000;Mask: $F000;CodedLen: 4;DecodedLen:    5 ),    // White 5
    (Code: $E000;Mask: $F000;CodedLen: 4;DecodedLen:    6 ),    // White 6
    (Code: $F000;Mask: $F000;CodedLen: 4;DecodedLen:    7 ),    // White 7
    (Code: $9800;Mask: $F800;CodedLen: 5;DecodedLen:    8 ),    // White 8
    (Code: $A000;Mask: $F800;CodedLen: 5;DecodedLen:    9 ),    // White 9
    (Code: $3800;Mask: $F800;CodedLen: 5;DecodedLen:   10 ),    // White 10
    (Code: $4000;Mask: $F800;CodedLen: 5;DecodedLen:   11 ),    // White 11
    (Code: $2000;Mask: $FC00;CodedLen: 6;DecodedLen:   12 ),    // White 12
    (Code: $0C00;Mask: $FC00;CodedLen: 6;DecodedLen:   13 ),    // White 13
    (Code: $D000;Mask: $FC00;CodedLen: 6;DecodedLen:   14 ),    // White 14
    (Code: $D400;Mask: $FC00;CodedLen: 6;DecodedLen:   15 ),    // White 15
    (Code: $A800;Mask: $FC00;CodedLen: 6;DecodedLen:   16 ),    // White 16
    (Code: $AC00;Mask: $FC00;CodedLen: 6;DecodedLen:   17 ),    // White 17
    (Code: $4E00;Mask: $FE00;CodedLen: 7;DecodedLen:   18 ),    // White 18
    (Code: $1800;Mask: $FE00;CodedLen: 7;DecodedLen:   19 ),    // White 19
    (Code: $1000;Mask: $FE00;CodedLen: 7;DecodedLen:   20 ),    // White 20
    (Code: $2E00;Mask: $FE00;CodedLen: 7;DecodedLen:   21 ),    // White 21
    (Code: $0600;Mask: $FE00;CodedLen: 7;DecodedLen:   22 ),    // White 22
    (Code: $0800;Mask: $FE00;CodedLen: 7;DecodedLen:   23 ),    // White 23
    (Code: $5000;Mask: $FE00;CodedLen: 7;DecodedLen:   24 ),    // White 24
    (Code: $5600;Mask: $FE00;CodedLen: 7;DecodedLen:   25 ),    // White 25
    (Code: $2600;Mask: $FE00;CodedLen: 7;DecodedLen:   26 ),    // White 26
    (Code: $4800;Mask: $FE00;CodedLen: 7;DecodedLen:   27 ),    // White 27
    (Code: $3000;Mask: $FE00;CodedLen: 7;DecodedLen:   28 ),    // White 28
    (Code: $0200;Mask: $FF00;CodedLen: 8;DecodedLen:   29 ),    // White 29
    (Code: $0300;Mask: $FF00;CodedLen: 8;DecodedLen:   30 ),    // White 30
    (Code: $1A00;Mask: $FF00;CodedLen: 8;DecodedLen:   31 ),    // White 31
    (Code: $1B00;Mask: $FF00;CodedLen: 8;DecodedLen:   32 ),    // White 32
    (Code: $1200;Mask: $FF00;CodedLen: 8;DecodedLen:   33 ),    // White 33
    (Code: $1300;Mask: $FF00;CodedLen: 8;DecodedLen:   34 ),    // White 34
    (Code: $1400;Mask: $FF00;CodedLen: 8;DecodedLen:   35 ),    // White 35
    (Code: $1500;Mask: $FF00;CodedLen: 8;DecodedLen:   36 ),    // White 36
    (Code: $1600;Mask: $FF00;CodedLen: 8;DecodedLen:   37 ),    // White 37
    (Code: $1700;Mask: $FF00;CodedLen: 8;DecodedLen:   38 ),    // White 38
    (Code: $2800;Mask: $FF00;CodedLen: 8;DecodedLen:   39 ),    // White 39
    (Code: $2900;Mask: $FF00;CodedLen: 8;DecodedLen:   40 ),    // White 40
    (Code: $2A00;Mask: $FF00;CodedLen: 8;DecodedLen:   41 ),    // White 41
    (Code: $2B00;Mask: $FF00;CodedLen: 8;DecodedLen:   42 ),    // White 42
    (Code: $2C00;Mask: $FF00;CodedLen: 8;DecodedLen:   43 ),    // White 43
    (Code: $2D00;Mask: $FF00;CodedLen: 8;DecodedLen:   44 ),    // White 44
    (Code: $0400;Mask: $FF00;CodedLen: 8;DecodedLen:   45 ),    // White 45
    (Code: $0500;Mask: $FF00;CodedLen: 8;DecodedLen:   46 ),    // White 46
    (Code: $0A00;Mask: $FF00;CodedLen: 8;DecodedLen:   47 ),    // White 47
    (Code: $0B00;Mask: $FF00;CodedLen: 8;DecodedLen:   48 ),    // White 48
    (Code: $5200;Mask: $FF00;CodedLen: 8;DecodedLen:   49 ),    // White 49
    (Code: $5300;Mask: $FF00;CodedLen: 8;DecodedLen:   50 ),    // White 50
    (Code: $5400;Mask: $FF00;CodedLen: 8;DecodedLen:   51 ),    // White 51
    (Code: $5500;Mask: $FF00;CodedLen: 8;DecodedLen:   52 ),    // White 52
    (Code: $2400;Mask: $FF00;CodedLen: 8;DecodedLen:   53 ),    // White 53
    (Code: $2500;Mask: $FF00;CodedLen: 8;DecodedLen:   54 ),    // White 54
    (Code: $5800;Mask: $FF00;CodedLen: 8;DecodedLen:   55 ),    // White 55
    (Code: $5900;Mask: $FF00;CodedLen: 8;DecodedLen:   56 ),    // White 56
    (Code: $5A00;Mask: $FF00;CodedLen: 8;DecodedLen:   57 ),    // White 57
    (Code: $5B00;Mask: $FF00;CodedLen: 8;DecodedLen:   58 ),    // White 58
    (Code: $4A00;Mask: $FF00;CodedLen: 8;DecodedLen:   59 ),    // White 59
    (Code: $4B00;Mask: $FF00;CodedLen: 8;DecodedLen:   60 ),    // White 60
    (Code: $3200;Mask: $FF00;CodedLen: 8;DecodedLen:   61 ),    // White 61
    (Code: $3300;Mask: $FF00;CodedLen: 8;DecodedLen:   62 ),    // White 62
    (Code: $3400;Mask: $FF00;CodedLen: 8;DecodedLen:   63 ),    // White 63
    (Code: $D800;Mask: $F800;CodedLen: 5;DecodedLen:   64 ),    // White 64
    (Code: $9000;Mask: $F800;CodedLen: 5;DecodedLen:  128 ),    // White 65
    (Code: $5C00;Mask: $FC00;CodedLen: 6;DecodedLen:  192 ),    // White 66
    (Code: $6E00;Mask: $FE00;CodedLen: 7;DecodedLen:  256 ),    // White 67
    (Code: $3600;Mask: $FF00;CodedLen: 8;DecodedLen:  320 ),    // White 68
    (Code: $3700;Mask: $FF00;CodedLen: 8;DecodedLen:  384 ),    // White 69
    (Code: $6400;Mask: $FF00;CodedLen: 8;DecodedLen:  448 ),    // White 70
    (Code: $6500;Mask: $FF00;CodedLen: 8;DecodedLen:  512 ),    // White 71
    (Code: $6800;Mask: $FF00;CodedLen: 8;DecodedLen:  576 ),    // White 72
    (Code: $6700;Mask: $FF00;CodedLen: 8;DecodedLen:  640 ),    // White 73
    (Code: $6600;Mask: $FF80;CodedLen: 9;DecodedLen:  704 ),    // White 74
    (Code: $6680;Mask: $FF80;CodedLen: 9;DecodedLen:  768 ),    // White 75
    (Code: $6900;Mask: $FF80;CodedLen: 9;DecodedLen:  832 ),    // White 76
    (Code: $6980;Mask: $FF80;CodedLen: 9;DecodedLen:  896 ),    // White 77
    (Code: $6A00;Mask: $FF80;CodedLen: 9;DecodedLen:  960 ),    // White 78
    (Code: $6A80;Mask: $FF80;CodedLen: 9;DecodedLen: 1024 ),    // White 79
    (Code: $6B00;Mask: $FF80;CodedLen: 9;DecodedLen: 1088 ),    // White 80
    (Code: $6B80;Mask: $FF80;CodedLen: 9;DecodedLen: 1152 ),    // White 81
    (Code: $6C00;Mask: $FF80;CodedLen: 9;DecodedLen: 1216 ),    // White 82
    (Code: $6C80;Mask: $FF80;CodedLen: 9;DecodedLen: 1280 ),    // White 83
    (Code: $6D00;Mask: $FF80;CodedLen: 9;DecodedLen: 1344 ),    // White 84
    (Code: $6D80;Mask: $FF80;CodedLen: 9;DecodedLen: 1408 ),    // White 85
    (Code: $4C00;Mask: $FF80;CodedLen: 9;DecodedLen: 1472 ),    // White 86
    (Code: $4C80;Mask: $FF80;CodedLen: 9;DecodedLen: 1536 ),    // White 87
    (Code: $4D00;Mask: $FF80;CodedLen: 9;DecodedLen: 1600 ),    // White 88
    (Code: $6000;Mask: $FC00;CodedLen: 6;DecodedLen: 1664 ),    // White 89
    (Code: $4D80;Mask: $FF80;CodedLen: 9;DecodedLen: 1728 ),    // White 90
    (Code: $0010;Mask: $FFF0;CodedLen:12;DecodedLen:    0 ),    // White 91
    (Code: $0100;Mask: $FFE0;CodedLen:11;DecodedLen: 1792 ),    // White 92
    (Code: $0180;Mask: $FFE0;CodedLen:11;DecodedLen: 1856 ),    // White 93
    (Code: $01A0;Mask: $FFE0;CodedLen:11;DecodedLen: 1920 ),    // White 94
    (Code: $0120;Mask: $FFF0;CodedLen:12;DecodedLen: 1984 ),    // White 95
    (Code: $0130;Mask: $FFF0;CodedLen:12;DecodedLen: 2048 ),    // White 96
    (Code: $0140;Mask: $FFF0;CodedLen:12;DecodedLen: 2112 ),    // White 97
    (Code: $0150;Mask: $FFF0;CodedLen:12;DecodedLen: 2176 ),    // White 98
    (Code: $0160;Mask: $FFF0;CodedLen:12;DecodedLen: 2240 ),    // White 99
    (Code: $0170;Mask: $FFF0;CodedLen:12;DecodedLen: 2304 ),    // White 100
    (Code: $01C0;Mask: $FFF0;CodedLen:12;DecodedLen: 2368 ),    // White 101
    (Code: $01D0;Mask: $FFF0;CodedLen:12;DecodedLen: 2432 ),    // White 102
    (Code: $01E0;Mask: $FFF0;CodedLen:12;DecodedLen: 2496 ),    // White 103
    (Code: $01F0;Mask: $FFF0;CodedLen:12;DecodedLen: 2560 )));  // White 104

var
    arTrueBuffer: array[0..319] of Byte;
    arFalseBuffer: array[0..319] of Byte;

//CCITT Group 4 Constants
type
  TModeType = (
    mtPass,
    mtHorz,
    mtV0,
    mtVR1,
    mtVR2,
    mtVR3,
    mtVL1,
    mtVL2,
    mtVL3,
    mtExt,
    mtNone);

const
  LookUpCodes : Array[mtPass..mtExt] of TCodeRec =
   ((Code: $1000;Mask: $F000;CodedLen:4;DecodedLen: 0 ),  //mtPass '0001'
    (Code: $2000;Mask: $E000;CodedLen:3;DecodedLen: 0 ),  //mtHorz '001'
    (Code: $8000;Mask: $8000;CodedLen:1;DecodedLen: 0 ),  //mtV0   '1'
    (Code: $6000;Mask: $E000;CodedLen:3;DecodedLen: 0 ),  //mtVR1  '011'
    (Code: $0C00;Mask: $FC00;CodedLen:6;DecodedLen: 0 ),  //mtVR2  '000011'
    (Code: $0600;Mask: $FE00;CodedLen:7;DecodedLen: 0 ),  //mtVR3  '0000011'
    (Code: $4000;Mask: $E000;CodedLen:3;DecodedLen: 0 ),  //mtVL1  '010'
    (Code: $0800;Mask: $FC00;CodedLen:6;DecodedLen: 0 ),  //mtVL2  '000010'
    (Code: $0400;Mask: $FE00;CodedLen:7;DecodedLen: 0 ),  //mtVL3  '0000010'
    (Code: $0200;Mask: $FE00;CodedLen:7;DecodedLen: 0 )); //mtExt  '0000001'

implementation

initialization
  FillChar(arFalseBuffer[0],320,0);
  FillChar(arTrueBuffer[0],320,$FF);
end.
