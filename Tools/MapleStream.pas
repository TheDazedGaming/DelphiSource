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

unit MapleStream;

interface

uses Classes, SysUtils, Types;

type
  TMapleStream = class(TMemoryStream)
  public
    function ReadByte: Byte;
    function ReadShort: Smallint;
    function ReadInt: Integer;
    function ReadAnsiString(Length: Integer): string;
    function ReadMapleAnsiString: string;
    function ReadPos: TPoint;

    procedure Skip(const Count: Int64);

    function ToString: string; override;
    function ToStringFromCurPos: string;

    procedure WriteByte(b: Byte); overload;
    procedure WriteByte(b: ShortInt); overload;
    procedure WriteByte(const V: Variant); overload;
    procedure WriteBool(b: Boolean);
    procedure WriteShort(s: Smallint);
    procedure WriteInt(i: Integer);
    procedure WriteAnsiString(s: string);
    procedure WriteMapleAnsiString(s: string);
    procedure WriteInt64(i: Int64);
    procedure WritePos(Pos: TPoint);
    procedure WriteHex(s: string);
  end;

implementation

{ TMapleStream }

function TMapleStream.ReadByte: Byte;
begin
  Read(Result, 1);
end;

function TMapleStream.ReadShort: Smallint;
begin
  Read(Result, 2);
end;

function TMapleStream.ReadInt: Integer;
begin
  Read(Result, 4);
end;

function TMapleStream.ReadAnsiString(Length: Integer): string;
var
  a: AnsiString;
begin
  SetLength(a, Length);
  Read(a[1], Length);
  Result := string(a);
end;

function TMapleStream.ReadMapleAnsiString: string;
begin
  Result := ReadAnsiString(ReadShort);
end;

function TMapleStream.ReadPos: TPoint;
begin
  Result.X := ReadShort;
  Result.Y := ReadShort;
end;

procedure TMapleStream.Skip(const Count: Int64);
begin
  Seek(Count, soCurrent);
end;

function TMapleStream.ToString: string;
var
  OldPos: Integer;
begin
  OldPos := Position;
  Position := 0;
  Result := ToStringFromCurPos;
  Position := OldPos;
end;

function TMapleStream.ToStringFromCurPos: string;
var
  OldPos: Integer;
  Data: TBytes;
  b: Byte;
begin
  if Size = 0 then
    Exit('<empty>');

  OldPos := Position;

  Result := '';
  SetLength(Data, Size - Position);
  Read(Data[0], Size - Position);
  for b in Data do
    Result := Result + Format('%.2x ', [b]);
  Data := nil;

  Position := OldPos;
end;

procedure TMapleStream.WriteByte(b: Byte);
begin
  Write(b, 1);
end;

procedure TMapleStream.WriteByte(b: ShortInt);
begin
  Write(b, 1);
end;

procedure TMapleStream.WriteByte(const V: Variant);
begin
  Write(TVarData(V).VByte, 1);
end;

procedure TMapleStream.WriteBool(b: Boolean);
begin
  if not b then
    WriteByte(0)
  else
    WriteByte(1);
end;

procedure TMapleStream.WriteShort(s: Smallint);
begin
  Write(s, 2);
end;

procedure TMapleStream.WriteInt(i: Integer);
begin
	Write(i, 4);
end;

procedure TMapleStream.WriteInt64(i: Int64);
begin
  Write(i, 8);
end;

procedure TMapleStream.WriteAnsiString(s: string);
var
  a: AnsiString;
begin
  a := AnsiString(s);
  Write(a[1], Length(a));
end;

procedure TMapleStream.WriteMapleAnsiString(s: string);
begin
  WriteShort(Length(s));
  WriteAnsiString(s);
end;

procedure TMapleStream.WritePos(Pos: TPoint);
begin
  WriteShort(Pos.X);
  WriteShort(Pos.Y);
end;

procedure TMapleStream.WriteHex(s: string);
var
  i: Integer;
  b: Byte;
begin
  s := StringReplace(s, ' ', '', [rfReplaceAll]);
  if Odd(Length(s)) then  // can't be odd
    raise EArgumentException.Create('Length of hex-string is odd');

  for i := 1 to Length(s) div 2 do
  begin
    b := StrToInt('$' + s[i * 2 - 1] + s[i * 2]);
    Write(b, 1);
  end;
end;

end.
