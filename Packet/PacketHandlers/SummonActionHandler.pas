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

unit SummonActionHandler;

interface

uses SysUtils, Generics.Collections, PacketProcessor, MapleClient, MapleStream,
     MaplePacketCreator, GameLogic, NPCConversation, MovementParser, MapleSummon,
     AttackParser;

type
  TMoveSummonHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TSummonAttackHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TDamageSummonHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TSummonTalkHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main;

{ TMoveSummonHandler }

class procedure TMoveSummonHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  OID: Integer;
  Res: TList<TLifeMovement>;
  LM: TLifeMovement;
  S: TMapleSummon;
begin
  OID := Packet.ReadInt;
  S := nil;
  for S in C.Player.Summons do
    if S.ObjectID = OID then
      Break;

  if (OID = 0) or (S = nil) then
    Exit;

  Packet.Skip({$IFDEF VERSION83} 4 {$ELSE} 8 {$ENDIF});  // Start Position & something else
  Res := TLifeMovement.ParseMovement(Packet);
  try
    C.Player.Map.BroadcastMessage(C.Player, S.GetMovePacket(Res));
    TLifeMovement.UpdatePosition(Res, S, 0);
  finally
    if Assigned(Res) then
    begin
      for LM in Res do
        LM.Free;

      Res.Free;
    end;
  end;
end;

{ TSummonAttackHandler }

class procedure TSummonAttackHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  OID: Integer;
  Attack: TAttackInfo;
  S: TMapleSummon;
begin
  OID := Packet.ReadInt;
  S := nil;
  for S in C.Player.Summons do
    if S.ObjectID = OID then
      Break;

  if (OID = 0) or (S = nil) then
    Exit;

  Packet.Skip({$IFDEF VERSION83} 4 {$ELSE} 20 {$ENDIF});

  Attack := TAttackParser.ParseAttack(Packet, atSummon, C.Player);
  try
   // C.Player.Map.BroadcastMessage(C.Player, S.GetAttackPacket(Attack));
    TAttackParser.ApplyAttack(Attack, C.Player);
  finally
    Attack.Free;
  end;
end;

{ TDamageSummonHandler }

class procedure TDamageSummonHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  OID, Unk, Damage, Monster: Integer;
  S: TMapleSummon;
begin
  OID := Packet.ReadInt;
  S := nil;
  for S in C.Player.Summons do
    if S.ObjectID = OID then
      Break;

  if (OID = 0) or (S = nil) then
    Exit;

  Unk := Packet.ReadByte;
  Damage := Packet.ReadInt;
  Monster := Packet.ReadInt;

  C.Player.Map.BroadcastMessage(C.Player, S.GetDamagePacket(Unk, Damage, Monster));

  S.AddHP(-Damage);
  if S.HP <= 0 then
    S.Cancel;
end;

{ TSummonTalkHandler }

class procedure TSummonTalkHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin
  if Assigned(C.Player.CurConversation) then
    Exit;

  // Get first two digits of map: 13 = Ereve; 14 = Rien
  case Trunc(C.Player.MapID / 10000000) of
    13: C.Player.CurConversation := TNPCConversation.Create(1101008, C, ExtractFilePath(ParamStr(0)) + 'Scripts\NPC\helperCygnus.ds');
    14: C.Player.CurConversation := TNPCConversation.Create(1202000, C, ExtractFilePath(ParamStr(0)) + 'Scripts\NPC\awake.ds');
    else Log('Unknown summon-talk map: %d', [C.Player.MapID]);
  end;

  if C.Player.CurConversation <> nil then
    C.Player.CurConversation.Start;
end;

initialization
  HandlerClasses.Add('MoveSummon', TMoveSummonHandler);
  HandlerClasses.Add('SummonAttack', TSummonAttackHandler);
  HandlerClasses.Add('DamageSummon', TDamageSummonHandler);
  HandlerClasses.Add('SummonTalk', TSummonTalkHandler);

end.
