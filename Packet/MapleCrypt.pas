(*
    This file is part of the Delphi MapleStory Server

    	Copyright (C) 2009-2010  Hendi

    The code contains portions of:

	    OdinMS
	    KryptoDEV Source
	    Vana

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License version 3
    as published by the Free Software Foundation. You may not use, modify
    or distribute this program under any other version of the
    GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License <http://www.gnu.org/licenses/>
    for more details.
*)

unit MapleCrypt;

interface

uses SysUtils, BitTools, DECCipher, SyncObjs;

const
  IVValues: array[0..255] of Byte = (
    $EC, $3F, $77, $A4, $45, $D0,	$71, $BF, $B7, $98, $20, $FC, $4B, $E9, $B3, $E1,
		$5C, $22, $F7, $0C, $44, $1B, $81, $BD, $63, $8D, $D4, $C3, $F2, $10, $19, $E0,
    $FB, $A1, $6E, $66, $EA, $AE, $D6, $CE, $06, $18, $4E, $EB, $78, $95, $DB, $BA,
    $B6, $42, $7A, $2A, $83, $0B, $54, $67, $6D, $E8, $65, $E7, $2F, $07, $F3, $AA,
    $27, $7B, $85, $B0, $26, $FD,	$8B, $A9, $FA, $BE, $A8, $D7, $CB, $CC,	$92, $DA,
    $F9, $93, $60, $2D, $DD, $D2, $A2, $9B, $39, $5F, $82, $21, $4C, $69, $F8, $31,
    $87, $EE,	$8E, $AD, $8C, $6A, $BC, $B5, $6B, $59, $13, $F1, $04, $00, $F6, $5A,
    $35, $79, $48, $8F, $15, $CD, $97, $57, $12, $3E, $37, $FF, $9D, $4F, $51, $F5,
    $A3, $70, $BB, $14, $75, $C2,	$B8, $72, $C0, $ED, $7D, $68, $C9, $2E, $0D, $62,
    $46, $17, $11, $4D,	$6C, $C4, $7E, $53, $C1, $25, $C7, $9A, $1C, $88, $58, $2C,
		$89, $DC, $02, $64, $40, $01, $5D, $38, $A5, $E2, $AF, $55,	$D5, $EF, $1A, $7C,
    $A7, $5B, $A6, $6F, $86, $9F, $73, $E6, $0A, $DE, $2B, $99, $4A, $47, $9C, $DF,
    $09, $76,	$9E, $30, $0E, $E4, $B2, $94, $A0, $3B, $34, $1D, $28, $0F,	$36, $E3,
    $23, $B4, $03, $D8, $90, $C8, $3C, $FE, $5E, $32, $24, $50, $1F, $3A, $43, $8A,
    $96, $41, $74, $AC, $52, $33, $F0, $D9, $29, $80, $B1, $16, $D3, $AB, $91, $B9,
		$84, $7F, $61, $1E, $CF, $C5, $D1, $56, $3D, $CA, $F4, $05, $C6, $E5, $08, $49);

  PacketKey: array[0..31] of Byte = (
    $13, $00, $00, $00, $08, $00, $00, $00, $06, $00, $00, $00, $B4, $00, $00, $00,
    $1B, $00, $00, $00, $0F, $00, $00, $00, $33, $00, $00, $00, $52, $00, $00, $00);

type
  TMapleDecoder = class
  private
    FIV: TBytes;
    FMapleVersion: Word;
    FCipher: TCipher_Rijndael;
    FCrit: TCriticalSection;

    function GetPacketHeader(Length: Cardinal): TBytes;
  public
    constructor Create(const IV; MapleVersion: Word);
    destructor Destroy; override;

    procedure AESDecrypt(var Data: TBytes);
    procedure AESEncrypt(var Data: TBytes);

    procedure NextIV;

    procedure MapleCustomDecrypt(var Data: TBytes);
    procedure MapleCustomEncrypt(var Data: TBytes);

    procedure DecryptPacket(var Data: TBytes);
    procedure EncryptPacket(var Data: TBytes);

    function CheckPacket(const Packet: TBytes): Boolean;

    property IV: TBytes read FIV;

    class function GetPacketLength(const Packet: TBytes): Cardinal;
  end;

implementation

{ TMapleAESOFB }

constructor TMapleDecoder.Create(const IV; MapleVersion: Word);
begin
  SetLength(FIV, 4);
  Move(IV, FIV[0], 4);

  FMapleVersion := ((MapleVersion shr 8) and $FF) or ((MapleVersion shl 8) and $FF00);

  FCipher := TCipher_Rijndael.Create;
  FCipher.Mode := cmOFBx;

  FCrit := TCriticalSection.Create;
end;

destructor TMapleDecoder.Destroy;
begin
  FIV := nil;

  FreeAndNil(FCipher);
  FCrit.Free;

  inherited;
end;

procedure TMapleDecoder.AESDecrypt(var Data: TBytes);
var
  ExpIV: TBytes;
  Pos, First: Integer;
begin
  ExpIV := MultiplyBytes(FIV, 4, 4);

  FCipher.Init(PacketKey, Length(PacketKey), ExpIV[0], Length(ExpIV));

  Pos := 0;
  First := 1;
  try
    while Length(Data) > Pos do
    begin
      if Length(Data) > Pos + 1460 - First * 4 then
        FCipher.Decode(Data[Pos], Data[Pos], 1460 - First * 4)
      else
        FCipher.Decode(Data[Pos], Data[Pos], Length(Data) - Pos);

      FCipher.Done;   // prepare for the decryption of the next bunch of bytes

      Inc(Pos, 1460 - First * 4);
      if First > 0 then
        First := 0;
    end;
  finally
    ExpIV := nil;
  end;

  FCrit.Enter;
  try
    NextIV;
  finally
    FCrit.Leave;
  end;
end;

procedure TMapleDecoder.AESEncrypt(var Data: TBytes);
var
  Pos, First: Integer;
  ExpIV: TBytes;
begin
  ExpIV := MultiplyBytes(FIV, 4, 4);

  FCipher.Init(PacketKey, Length(PacketKey), ExpIV[0], Length(ExpIV));

  Pos := 0;
  First := 1;
  try
    while Length(Data) > Pos do
    begin
      if (Length(Data) > Pos + 1460 - First * 4) then
        FCipher.Encode(Data[Pos], Data[Pos], 1460 - First * 4)
      else
        FCipher.Encode(Data[Pos], Data[Pos], Length(Data) - Pos);

      FCipher.Done;   // prepare for the encryption of the next bunch of bytes

      Inc(Pos, 1460 - First * 4);
      if First > 0 then
        First := 0;
    end;
  finally
    ExpIV := nil;
  end;

  FCrit.Enter;
  try
    NextIV;
  finally
    FCrit.Leave;
  end;
end;

procedure TMapleDecoder.NextIV;
var
  x: TBytes;
  i: Integer;
  a, b: Byte;
  c, d: Cardinal;
begin
  SetLength(x, 4);
  x[0] := $F2;
  x[1] := $53;
  x[2] := $50;
  x[3] := $C6;

  for i := 0 to 3 do
  begin
    a := x[1];

    b := IVValues[a];
    Dec(b, FIV[i]);
    Inc(x[0], b);
    b := x[2];
    b := b xor IVValues[FIV[i]];
    Dec(a, b);
    x[1] := a;
    a := x[3];
    b := a;
    Dec(a, x[0]);
    b := IVValues[b];
    Inc(b, FIV[i]);
    b := b xor x[2];
    x[2] := b;
    Inc(a, IVValues[FIV[i]]);
    x[3] := a;

    c := x[0] + x[1] * $100 + x[2] * $10000 + x[3] * $1000000;
    d := c;
    c := c shr $1D;
    d := d shl $03;
    c := c or d;
    x[0] := Byte(c mod $100);
    c := c div $100;
    x[1] := Byte(c mod $100);
    c := c div $100;
    x[2] := Byte(c mod $100);
    x[3] := Byte(c div $100);
  end;

  FIV := nil;
  FIV := x;
end;

function TMapleDecoder.CheckPacket(const Packet: TBytes): Boolean;
begin
  Result := (((Packet[0] xor FIV[2]) and $FF) = ((FMapleVersion shr 8) and $FF)) and (((Packet[1] xor FIV[3]) and $FF) = (FMapleVersion and $FF));
end;

class function TMapleDecoder.GetPacketLength(const Packet: TBytes): Cardinal;
begin
  Result := (Packet[0] + Packet[1] * $100) xor (Packet[2] + Packet[3] * $100);
end;

procedure TMapleDecoder.MapleCustomDecrypt(var Data: TBytes);
var
  i, j: Integer;
  a, b, c: Byte;
begin
  for i := 0 to 2 do
  begin
    b := 0;
    for j := Length(Data) downto 1 do
    begin
      c := Data[j - 1];
      c := RollLeft(c, 3);
      c := c xor $13;
      a := c;
      c := c xor b;
      Dec(c, j);
      c := RollRight(c, 4);
      b := a;
      Data[j - 1] := c;
    end;

    b := 0;
    for j := Length(Data) downto 1 do
    begin
      c := Data[Length(Data) - j];
      Dec(c, $48);
      c := c xor $FF;
      c := RollLeft(c, j);
      a := c;
      c := c xor b;
      Dec(c, j);
      c := RollRight(c, 3);
      b := a;
      Data[Length(Data) - j] := c;
    end;
  end;
end;

procedure TMapleDecoder.MapleCustomEncrypt(var Data: TBytes);
var
  i, j, a: Integer;
  c: Byte;
begin
  for i := 0 to 2 do
  begin
    a := 0;

    for j := Length(Data) downto 1 do
    begin
      c := Data[Length(Data) - j];
      c := RollLeft(c, 3);
      Inc(c, j);
      c := c xor a;
      a := c;
      c := RollRight(a, j);
      c := c xor $FF;
      Inc(c, $48);
      Data[Length(Data) - j] := c;
    end;

    a := 0;
    for j := Length(Data) downto 1 do
    begin
      c := Data[j - 1];
      c := RollLeft(c, 4);
      Inc(c, j);
      c := c xor a;
      a := c;
      c := c xor $13;
      c := RollRight(c, 3);
      Data[j - 1] := c;
    end;
  end;
end;

procedure TMapleDecoder.EncryptPacket(var Data: TBytes);
var
  Unenc, Res, Header: TBytes;
begin
  Unenc := Copy(Data, 0, Length(Data));

  SetLength(Res, Length(Data) + 4);
  Header := GetPacketHeader(Length(Data));
  Data := nil;

  MapleCustomEncrypt(Unenc);
  AESEncrypt(Unenc);

  Move(Header[0], Res[0], 4);
  Move(Unenc[0], Res[4], Length(Unenc));
  Data := Res;

  Unenc := nil;
  Header := nil;
end;

procedure TMapleDecoder.DecryptPacket(var Data: TBytes);
begin
  AESDecrypt(Data);
  MapleCustomDecrypt(Data);
end;

function TMapleDecoder.GetPacketHeader(Length: Cardinal): TBytes;
var
  IIV, MLength, XoredIV: Cardinal;
begin
  IIV := FIV[3] and $FF;
  IIV := IIV or ((FIV[2] shl 8) and $FF00);

  IIV := IIV xor FMapleVersion;
  MLength := ((Length shl 8) and $FF00) or (Length shr 8);
  XoredIV := IIV xor MLength;

  SetLength(Result, 4);
  Result[0] := (IIV shr 8) and $FF;
	Result[1] := IIV and $FF;
	Result[2] := (XoredIV shr 8) and $FF;
	Result[3] := XoredIV and $FF;
end;

end.
