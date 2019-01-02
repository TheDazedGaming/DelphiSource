(*
    This file is part of the Delphi MapleStory Server

    	Copyright (C) 2009-2011  Hendi

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

unit PlayerInteractionHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator,
     MapleCharacter, Trade;

type
  TPlayerInteraction = (piCreate, piInvite = 2, piDecline, piView, piChat = 6,
    piCancel = 10, piAddItem = 15, piAddMesos, piConfirm);

  TPlayerInteractionHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main;

{ TPlayerInteractionHandler }

class procedure TPlayerInteractionHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Mode: TPlayerInteraction;
  CType: Byte;
  Partner: TMapleCharacter;
  T: TTrade;
  Inv, Pos, Count, Slot: SmallInt;
begin
Log(Packet.ToString);
  Mode := TPlayerInteraction(Packet.ReadByte);
  T := TTrade(C.Player.Trade);

  case Mode of
    piCreate:
    begin
      CType := Packet.ReadByte;

      if CType = Byte(itTrade) then
      begin
        if C.Player.IsOccupied then
          Exit;

        C.Player.Trade := TTrade.Create(C.Player);
      end;
    end;

    piInvite:
    begin
      Partner := TMapleCharacter(C.Player.Map.MapObject[Packet.ReadInt]);
      if Partner = nil then
        Exit;

      if Assigned(T) and T.Opened then
        T.Invite(Partner);
    end;

    piDecline:
    begin
      if Assigned(T) and not T.Opened then
        T.Decline;
    end;

    piView:
    begin
      if Assigned(T) and not T.Opened then
        T.Accept;
    end;

    piChat:
    begin
      Packet.Skip(4);

      if Assigned(T) and T.Opened then
        T.Chat(Packet.ReadMapleAnsiString);
    end;

    piCancel:
    begin
      if Assigned(T) and T.Opened then
        T.Cancel;
    end;

    piAddItem:
    begin
      if Assigned(T) and T.Opened then
      begin
        Inv := Packet.ReadByte;
        Pos := Packet.ReadShort;
        Count := Packet.ReadShort;
        Slot := Packet.ReadByte;
        T.AddItem(Inv, Pos, Count, Slot);
      end;
    end;

    piAddMesos:
    begin
      if Assigned(T) and T.Opened then
        T.AddMesos(Packet.ReadInt);
    end;

    piConfirm:
    begin
      if Assigned(T) and T.Opened then
        T.Confirm;
    end;
  end;
end;

initialization
  HandlerClasses.Add('PlayerInteraction', TPlayerInteractionHandler);

end.
