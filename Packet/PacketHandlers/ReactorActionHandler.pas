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

unit ReactorActionHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator, GameLogic,
     Generics.Collections, MapleMapObject, MapleReactor;

type
  THitReactorHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

{ THitReactorHandler }

class procedure THitReactorHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  OID, CharPos: Integer;
  Stance: SmallInt;
  MO: TMapleMapObject;
begin
  OID := Packet.ReadInt;
  CharPos := Packet.ReadInt;  // relative position
  Stance := Packet.ReadShort;

  MO := C.Player.Map.MapObject[OID];
  if (MO <> nil) and (MO is TMapleReactor) then
    TMapleReactor(MO).Hit(CharPos, Stance, C);
end;

initialization
  HandlerClasses.Add('HitReactor', THitReactorHandler);

finalization

end.
