(*
    This file is part of the Delphi MapleStory Server

    	Copyright (C) 2009-2010 Hendi

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

unit ChecksumCommand;

interface

uses Classes, SysUtils, CommandProcessor, MapleClient, Utils, Generics.Collections;

type
  TItemEntry = record
    ID: Integer;
    Name: string;
    Enabled: Boolean;
    Checksum: Integer;
  end;
  PItemEntry = ^TItemEntry;

  TItemDB = TDictionary<Integer, PItemEntry>;

var
  Items: TItemDB = nil;

implementation

uses PlayerCommands, MapleServerHandler, MaplePacketCreator;

function LoadDatabase(const FN: string): TItemDB;
var
  SL: TStringList;
  S: TArrayofstring;
  i: Integer;
  E: PItemEntry;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(ExtractFilePath(ParamStr(0)) + FN);
    Result := TItemDB.Create;
    for i := 0 to SL.Count - 1 do
    begin
      S := Explode('|', SL[i]);
      New(E);
      E.ID := StrToInt(S[0]);
      E.Name := S[1];
      E.Enabled := Boolean(StrToInt(S[2]));
      E.Checksum := StrToInt(S[3]);
      Result.Add(E.ID, E);

      S := nil;
    end;
  finally
    SL.Free;
  end;
end;

procedure SaveDatabase(const FN: string; DB: TItemDB);
var
  SL: TStringList;
  Order: TList<Integer>;
  i: Integer;
  E: PItemEntry;
begin
  SL := TStringList.Create;
  Order := TList<Integer>.Create;
  try
    for i in DB.Keys do
      Order.Add(i);
    Order.Sort;

    for i in Order do
    begin
      E := DB[i];
      SL.Add(Format('%d|%s|%d|$%x', [E.ID, E.Name, Ord(E.Enabled), E.Checksum]));
      Dispose(E);
    end;

    SL.SaveToFile(ExtractFilePath(ParamStr(0)) + FN);
  finally
    SL.Free;
    Order.Free;
  end;
end;

procedure ItemCRC_L(Cmd: string; C: TMapleClient);
var
  i: Integer;
begin
  Items := LoadDatabase('Items.txt');
  for i in Items.Keys do
    Drop(IntToStr(i), C);
end;

procedure ItemCRC_S(Cmd: string; C: TMapleClient);
begin
  try
    SaveDatabase('Items.txt', Items);
  finally
    Items.Free;
  end;
end;

procedure ItemCRC_CopyConfig(Cmd: string; C: TMapleClient);
var
  i: Integer;
  Src, Dst: TItemDB;
  Params: TArrayofstring;
begin
  Params := Explode(' ', Cmd);
  Src := LoadDatabase(Params[0]);
  Dst := LoadDatabase(Params[1]);

  for i in Src.Keys do
    if Dst.ContainsKey(i) then
      PItemEntry(Dst[i]).Enabled := PItemEntry(Src[i]).Enabled;

  SaveDatabase(Params[1], Dst);

  C.Write(ShowInfoOnScreen('Configuration copied!'));

  Src.Free;
  Dst.Free;
  Params := nil;
end;

procedure ItemCRC_Full(Cmd: string; C: TMapleClient);
var
  E: PItemEntry;
begin
  Items := TDictionary<Integer, PItemEntry>.Create;
  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT itemid, label FROM item_data RIGHT JOIN strings ON objectid = itemid WHERE itemid < 5000000 AND itemid div 10000 <> 238 AND NOT flags LIKE ''%cash_item%'' AND object_type = ''item'';';
    Open;

    while not EOF do
    begin
      New(E);
      E.ID := FieldByName('itemid').AsInteger;
      E.Name := StringReplace(FieldByName('label').AsString, #13#10, '', [rfReplaceAll]);
      E.Enabled := False;
      E.Checksum := 0;
      Items.Add(E.ID, E);

      Drop(IntToStr(E.ID), C);

      Next;
    end;

    Free;
  end;
end;

initialization
  CmdHandlers.Add('itemcrc_l', ItemCRC_L);
  CmdHandlers.Add('itemcrc_s', ItemCRC_S);
  CmdHandlers.Add('itemcrc_full', ItemCRC_Full);
  CmdHandlers.Add('itemcrc_copyconfig', ItemCRC_CopyConfig);

end.
