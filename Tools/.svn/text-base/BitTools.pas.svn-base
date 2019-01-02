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

unit BitTools;

interface

uses Windows, SysUtils;

function MultiplyBytes(b: TBytes; Count, Mul: Integer): TBytes;
function RollLeft(b: ShortInt; Count: Integer): ShortInt;
function RollRight(b: ShortInt; Count: Integer): ShortInt;

implementation

function MultiplyBytes(b: TBytes; Count, Mul: Integer): TBytes;
var
  x: Integer;
begin
  SetLength(Result, Count * Mul);
  for x := 0 to High(Result) do
    Result[x] := b[x mod Count];
end;

function RollLeft(b: ShortInt; Count: Integer): ShortInt;
var
  Tmp: Integer;
begin
  Tmp := Integer(b) and $FF;
  Tmp := Tmp shl (Count mod 8);
  Result := ShortInt((Tmp and $FF) or (Tmp shr 8));
end;

function RollRight(b: ShortInt; Count: Integer): ShortInt;
var
  Tmp: Integer;
begin
  Tmp := Integer(b) and $FF;
  Tmp := Cardinal(Tmp shl 8) shr (Count mod 8);
  Result := ShortInt(Cardinal(Tmp and $FF) or (Cardinal(Tmp) shr 8));
end;

end.
