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

unit PlayerCommands;

interface

uses SysUtils, CommandProcessor, MapleClient, Utils, ItemDataProvider, GameLogic,
     MapleCharacter, DropHandler, MapleItem;

procedure Drop(Cmd: string; C: TMapleClient);

implementation

uses Main, PlayerInventory;

{ /level }

procedure Level(Cmd: string; C: TMapleClient);
var
  Val: Integer;
begin
  if TryStrToInt(Cmd, Val) and (Val < 256) then
  begin
    C.Player.Level := StrToInt(Cmd);
    C.Player.UpdateSingleStat(msLevel, C.Player.Level);
  end
  else
    C.Player.LevelUp;
end;

{ /item }

procedure Item(Cmd: string; C: TMapleClient);
var
  Params: TArrayofstring;
  I, Q: Integer;
begin
  Params := Explode(' ', Cmd);
  if Length(Params) > 1 then
    Q := StrToInt(Params[1])
  else
    Q := 1;

  I := StrToInt(Params[0]);
  if not ItemDataProv.IsItem(I) then
    Exit;

  C.Player.GainItemByID(I, Q, C.Player.Name);
  Params := nil;
end;

{ /drop }

procedure Drop(Cmd: string; C: TMapleClient);
var
  Params: TArrayofstring;
  I, Q: Integer;
  D: TDrop;
  Item: TItem;
begin
  Params := Explode(' ', Cmd);
  if Length(Params) > 1 then
    Q := StrToInt(Params[1])
  else
    Q := 1;

  I := StrToInt(Params[0]);
  if not ItemDataProv.IsItem(I) then
    Exit;

  if Params[0][1] = '1' then
    Item := ItemDataProv.LoadEquip(I)
  else
    Item := TItem.Create(I, 0, Q);

  D := TDrop.Create(C.Player.Map, Item, C.Player.Map.CalcDropPos(C.Player.Position), C.Player.ID, True);
  D.DoDrop(C.Player.Position);
  Params := nil;
end;

{ /mesos }

procedure Mesos(Cmd: string; C: TMapleClient);
var
  bi: Int64;
begin
  bi := StrToInt64(Cmd);
  if bi < 0 then
    bi := 0
  else
    if bi > MAXINT then
      bi := MAXINT;

  C.Player.Mesos := Integer(bi);
  C.Player.UpdateSingleStat(msMeso, C.Player.Mesos);
end;

{ /job }

procedure Job(Cmd: string; C: TMapleClient);
var
  Job: Integer;
begin
  // First try to parse the ID - it's a string and needs to be converted to an Integer
  if not TryStrToInt(Cmd, Job) then
  begin
    Log('[Commands] Parsing the job ID failed!');
    Exit;
  end;

  C.Player.ChangeJob(TMapleJob(Job)); // it's an int, need to typecast it to TMapleJob
end;

{ /sp }

procedure SetSP(Cmd: string; C: TMapleClient);
begin
  C.Player.RemainingSP := StrToInt(Cmd);
  C.Player.UpdateSingleStat(msRemainingSP, C.Player.RemainingSP);
end;

{ /ap }

procedure SetAP(Cmd: string; C: TMapleClient);
begin
  C.Player.RemainingAP := StrToInt(Cmd);
  C.Player.UpdateSingleStat(msRemainingAP, C.Player.RemainingAP);
end;

initialization
  CmdHandlers.Add('level', Level);
  CmdHandlers.Add('item', Item);
  CmdHandlers.Add('drop', Drop);
  CmdHandlers.Add('mesos', Mesos);
  CmdHandlers.Add('job', Job);
  CmdHandlers.Add('sp', SetSP);
  CmdHandlers.Add('ap', SetAP);

end.
