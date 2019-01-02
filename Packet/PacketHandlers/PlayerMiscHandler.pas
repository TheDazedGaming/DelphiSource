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

unit PlayerMiscHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream;

type
  TChangeMode = (cmKey, cmAutoHPPotion, cmAutoMPPotion);

  TKeymapChangeHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TChangeQuickSlotHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleCharacter;

{ TKeymapChangeHandler }

class procedure TKeymapChangeHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Mode: TChangeMode;
  ChangeCount, i, Key, Action: Integer;
  KType: Byte;
begin
  Mode := TChangeMode(Packet.ReadInt);
  ChangeCount := Packet.ReadInt;

  if ChangeCount = 0 then
    Exit;

  for i := 0 to ChangeCount - 1 do
    case Mode of
      cmKey:
      begin
        Key := Packet.ReadInt;
        KType := Packet.ReadByte;
        Action := Packet.ReadInt;
        C.Player.ChangeKeyBinding(Key, KType, Action);
      end;
      cmAutoHPPotion: ;  // xxx
      cmAutoMPPotion: ;

      else Log('Unknown Keymap-Change mode: %d', [Integer(Mode)]);
    end;
end;

{ TChangeQuickSlotHandler }

class procedure TChangeQuickSlotHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  i: Integer;
  QS: TQuickSlot;
begin
  QS := C.Player.QuickSlot;
  for i := 0 to 7 do
    QS[i] := Packet.ReadInt;
  C.Player.QuickSlot := QS;
end;

initialization
  HandlerClasses.Add('ChangeKeymap', TKeymapChangeHandler);
  HandlerClasses.Add('ChangeQuickSlot', TChangeQuickSlotHandler);

finalization

end.
