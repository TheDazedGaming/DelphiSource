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

unit Utils;

interface

uses Windows, SysUtils, WinSock;

type
  TArrayofstring = array of string;
  TArrayofInteger = array of Integer;
  TIPArray = array[0..3] of AnsiChar;

  TMiniCharInfo = record
    ID: Integer;
    Name: string;

    constructor Create(AID: Integer; AName: string);
  end;

function Explode(const Separator, s: string; Limit: Integer = 0): TArrayofstring;
function TransformIP(IP: string): TIPArray;
function TimeToTick(const Time: TDateTime): Int64;
function DistanceSq(const Left, Right: TPoint): Integer;
function RandomHexStr(const Len: Integer): string;
function RightPaddedStr(s: string; PadChar: Char; NewLength: Integer): string;

implementation

function Explode(const Separator, s: string; Limit: Integer = 0): TArrayofstring;
var
  SepLen: Integer;
  F, P: PChar;
  ALen, Index: Integer;
begin
  SetLength(Result, 0);
  if (S = '') or (Limit < 0) then Exit;
  if Separator = '' then
  begin
    SetLength(Result, 1);
    Result[0] := S;
    Exit;
  end;
  SepLen := Length(Separator);
  ALen := Limit;
  SetLength(Result, ALen);

  Index := 0;
  P := PChar(S);
  while P^ <> #0 do
  begin
    F := P;
    P := StrPos(P, PChar(Separator));
    if (P = nil) or ((Limit > 0) and (Index = Limit - 1)) then P := StrEnd(F);
    if Index >= ALen then
    begin
      Inc(ALen, 5);   // 5 at once so it's faster
      SetLength(Result, ALen);
    end;
    SetString(Result[Index], F, P - F);
    Inc(Index);
    if P^ <> #0 then Inc(P, SepLen);
  end;
  if Index < ALen then SetLength(Result, Index);   // Set real length
end;

function ResolveName(HostName: string): string;
var
  TMPResult: AnsiString;
  WSA: TWSAData;
  H: PHostEnt;
  P: PAnsiChar;
begin
  if WSAStartup($101, WSA) = 0 then
  begin
    GetMem(P, 255 + 1);
    StrPCopy(P, AnsiString(HostName));
    H := GetHostByName(P);
    FreeMem(P);
    if H <> nil then
    begin
      P := inet_ntoa(PInAddr(H^.h_addr_list^)^);
      TMPResult := StrPas(P);
    end;
    WSACleanup;

    if TMPResult <> '' then
      Result := string(TMPResult)
    else
      Result := '';
  end;
end;

function TransformIP(IP: string): TIPArray;
var
  Split: TArrayofstring;
  i: Integer;
begin
  // xxx Name is resolved everytime a client connects to a channel, maybe cache it?
  for i := 1 to Length(IP) do
    if CharInSet(IP[i], ['a'..'z']) or CharInSet(IP[i], ['A'..'Z']) then
      IP := ResolveName(IP);

  Split := Explode('.', IP);

  for i := 0 to High(Result) do
    Result[i] := AnsiChar(Chr(StrToInt(Split[i])));

  Split := nil;
end;

function TimeToTick(const Time: TDateTime): Int64;
var
  SystemTime: TSystemTime;
  FileTime: TFileTime;
  uli: ULARGE_INTEGER;
begin
  DateTimeToSystemTime(Time, SystemTime);
  SystemTimeToFileTime(SystemTime, FileTime);
  uli.LowPart := FileTime.dwLowDateTime;
  uli.HighPart := FileTime.dwHighDateTime;
  Result := uli.QuadPart;
end;

function DistanceSq(const Left, Right: TPoint): Integer;
var
  dx, dy: Integer;
begin
  dx := Left.x - Right.x;
  dy := Left.y - Right.y;
  Result := (Dx * Dx) + (Dy * Dy);
end;

function RandomHexStr(const Len: Integer): string;
const
  CharSet: string = 'abcdef0123456789';
var
  i: Integer;
begin
  SetLength(Result, Len);
  for i := 1 to Len do
    Result[i] := CharSet[1 + Random(Length(CharSet))];
end;

function RightPaddedStr(s: string; PadChar: Char; NewLength: Integer): string;
var
  i: Integer;
begin
  Result := s;
  SetLength(Result, NewLength);
  for i := Length(s) + 1 to NewLength do
    Result[i] := PadChar;
end;

{ TMiniCharInfo }

constructor TMiniCharInfo.Create(AID: Integer; AName: string);
begin
  ID := AID;
  Name := AName;
end;

end.
