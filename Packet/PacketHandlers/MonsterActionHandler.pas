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

unit MonsterActionHandler;

interface

uses MapleStream, SysUtils, PacketProcessor, MapleMapObject, MapleMonster,
     Generics.Collections, MovementParser, Types, MaplePacketCreator, Windows, GameLogic,
     MapleClient;

type
  TMoveLifeHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TMobDamagedHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleCharacter;

{ TMoveLifeHandler }

class procedure TMoveLifeHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  ObjectID, MoveID: Integer;
  MMO: TMapleMapObject;
  Mons: TMapleMonster;
  Res: TList<TLifeMovement>;
  Action, SkillID, SkillLv: Byte;
  Delay: Int16;
  NextAttackPossible: Boolean;
  StartPos: TPoint;
  LM: TLifeMovement;
const
  {$IF DEFINED(CHAOS)}
  BYTES_LEFT = 33;
  {$ELSEIF DEFINED(AFTERSHOCK)}
  BYTES_LEFT = 25;
  {$ELSEIF DEFINED(VERSION88_UP)}
  BYTES_LEFT = 17;
  {$ELSE}
  BYTES_LEFT = 9;
  {$IFEND}
begin
  ObjectID := Packet.ReadInt;
  MoveID := Packet.ReadShort;

  MMO := C.Player.Map.MapObject[ObjectID];

  // Doesn't exist anymore / map was changed to a town and still received a monster packet
  if (MMO = nil) or (not (MMO is TMapleMonster)) then
    Exit;

  Mons := TMapleMonster(MMO);
  NextAttackPossible := (Packet.ReadByte and $F) <> 0;
  Action := Packet.ReadByte;
  SkillID := Packet.ReadByte;
  SkillLv := Packet.ReadByte;
  Delay := Packet.ReadShort;

  // xxx Skill handling

  // 2 ints (counts), byte IsCheatMobMoveRand, int GetHackedCode
  Packet.Skip({$IFNDEF VERSION88_UP} 5 {$ELSE} 13 {$ENDIF});
  Packet.Skip({$IFNDEF VERSION88_UP} 8 {$ELSE} 12 {$ENDIF});   // more crap I have no idea of   CC DD FF 00 CC DD FF 00

  StartPos.X := Packet.ReadShort;
  StartPos.Y := Packet.ReadShort;

  {$IFDEF VERSION88_UP}
  Packet.Skip(4);
  {$ENDIF}

  Res := TLifeMovement.ParseMovement(Packet);
  try
    // xxx Aggro & Controlling

    C.Write(MoveMonsterResponse(ObjectID, MoveID, Mons.MP, NextAttackPossible));

    if Res <> nil then
    begin
      if Packet.Size - Packet.Position <> BYTES_LEFT then
      begin
        Log('[WARNING] Movement parsing error (available not correct: %d)', [Packet.Size - Packet.Position]);
        Exit;
      end;

      C.Player.Map.BroadcastMessage(C.Player,
         MaplePacketCreator.MoveMonster(NextAttackPossible, Action, SkillID, SkillLv, Delay, ObjectID, StartPos, Res), False);
      TLifeMovement.UpdatePosition(Res, Mons, -1);
      C.Player.Map.MoveMonster(Mons, Mons.Position);
      // xxx Hack detection
    end;
  finally
    if Assigned(Res) then
    begin
      for LM in Res do
        LM.Free;

      Res.Free;
    end;
  end;
end;

{ TMobDamagedHandler }

class procedure TMobDamagedHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin
  if C.Player.Job < mjLegend then
    Exit;

  // Increase combo. It's resetted in TCancelBuffHandler
  C.Player.ComboCounter := C.Player.ComboCounter + 1;
  C.Write(ShowAranComboCounter(C.Player.ComboCounter));
end;

initialization
  HandlerClasses.Add('MoveLife', TMoveLifeHandler);
  HandlerClasses.Add('MobDamaged', TMobDamagedHandler);

finalization

end.
