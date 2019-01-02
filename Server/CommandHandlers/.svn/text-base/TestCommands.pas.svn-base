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

unit TestCommands;

interface

uses SysUtils, CommandProcessor, MapleClient, Utils, MapleStream, MapleServerHandler,
MapleNPC, MaplePacketCreator;

implementation

uses Main;

procedure ReloadOpcodes(Cmd: string; C: TMapleClient);
begin
  MSH.LoadOpcodes;
end;

procedure act(Cmd: string; C: TMapleClient);
var
  P: TMapleStream;
begin
  P := TMapleStream.Create;
  with P do
  begin
    WriteShort(OpHandler.SendOps['NPCAnimation']);
    Log('%d', [TMapleNPC(C.Player.Map.MapObject[650001]).ID]);
    WriteInt(650001);
    WriteByte($FF);
    WriteByte($00);
    WriteInt64(0);
    WriteShort(0);
  end;

  C.Write(P);
end;

procedure info(Cmd: string; C: TMapleClient);
var
  Result: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowItemGainInChat']);

    WriteByte(StrToInt(Cmd));
    WriteMapleAnsiString('UI/tutorial.img/25');
    WriteInt(1);
  end;
  C.Write(Result);
end;

procedure unstuck(Cmd: string; C: TMapleClient);
begin
  C.Write(MaplePacketCreator.EnableActions(True));
end;

// For finding energy charge buff mask
procedure cbuff(Cmd: string; C: TMapleClient);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['UseSkill']);

    WriteInt64(StrToInt64(Cmd));
    WriteInt64(0);

    WriteShort(0);
    WriteShort(5000);
    WriteShort(0);
    WriteInt(0);
    WriteInt(0);
    WriteByte(0);
    WriteShort(0);
    WriteShort(0);
    WriteByte(0);
  end;
  C.Write(Packet);
end;

procedure SendPacket(Cmd: string; C: TMapleClient);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  Packet.WriteHex(Cmd);
  C.Write(Packet);
end;

initialization
  CmdHandlers.Add('reloadopcodes', ReloadOpcodes);
  CmdHandlers.Add('anim', act);
  CmdHandlers.Add('info', info);
  CmdHandlers.Add('unstuck', unstuck);
  CmdHandlers.Add('cbuff', cbuff);
  CmdHandlers.Add('packet', SendPacket);

end.
