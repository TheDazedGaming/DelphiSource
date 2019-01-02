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

unit PlayerActionHandler;

interface

uses Classes, SysUtils, MapleClient, Generics.Collections, PacketProcessor, MapleStream,
     MovementParser, AttackParser, MapleMapObject, GameLogic, SkillDataProvider,
     WinSock, Utils;

type
  TMovePlayerHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TCloseRangeDamageHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TRangedAttackHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TMagicAttackHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TEnergyChargeAttackHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TFaceExpressionHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TChangeMapHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TChangeChannelHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TChangeMapSpecialHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TUseChairItemHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TChairActionHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TUseInnerPortalHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleCharacter, MaplePacketCreator, MaplePortal, MapleMap,
     MapleItem, Settings, ChannelServer, PlayerInventory, Skills;

{ TMovePlayerHandler }

class procedure TMovePlayerHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Res: TList<TLifeMovement>;
  Char: TMapleCharacter;
  i: Integer;
begin
  Packet.Skip({$IFDEF CHAOS} 13 {$ELSE} {$IFNDEF VERSION88_UP} 9 {$ELSE} 37 {$ENDIF}{$ENDIF});

  Res := TLifeMovement.ParseMovement(Packet);

  if Assigned(Res) then
  begin
    Char := C.Player;
    Char.Map.BroadcastMessage(Char, MovePlayer(Char.ID, Res));

    TLifeMovement.UpdatePosition(Res, Char, 0);
    Char.Map.MovePlayer(Char, Char.Position);

    for i := 0 to Res.Count - 1 do
      Res[i].Free;
    Res.Free;
  end;
end;

{ TCloseRangeDamageHandler }

class procedure TCloseRangeDamageHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Info: TAttackInfo;
begin
  Info := TAttackParser.ParseAttack(Packet, atMelee, C.Player);
  try
    C.Player.Map.BroadcastMessage(C.Player, CloseRangeAttack(Info));

    TAttackParser.ApplyAttack(Info, C.Player);

    if C.Player.ActiveBuffs.GetEnergyCharge > 0 then
      C.Player.ActiveBuffs.IncreaseEnergyChargeLevel(Info.DamagedTargets);

    case Info.Skill of
      Crusader.PanicSword,
      Crusader.PanicAxe,
      Crusader.ComaSword,
      Crusader.ComaAxe,
      DawnWarrior.Panic,
      DawnWarrior.Coma:
        C.Player.ActiveBuffs.SetCombo(0, True);

      else if Info.TotalDamage > 0 then
        C.Player.ActiveBuffs.AddCombo;
    end;
  finally
    Info.Free;
  end;
end;

{ TRangedAttackHandler }

class procedure TRangedAttackHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
const
  MagicalMitten = 1472063;
var
  Player: TMapleCharacter;
  Info: TAttackInfo;
  Weapon, Item: TItem;
  WType, IType: TItemType;
  IsClaw, IsBow, IsCBow, IsGun, IsMitten: Boolean;
  i, Projectile, UseCount: Integer;
  Skill: TSkill;
begin
  Player := C.Player;
  Info := TAttackParser.ParseAttack(Packet, atRanged, Player);
  try
    Weapon := Player.Inventory[miEquipped][esWeapon];
    WType := GetItemType(Weapon.ID);

    Projectile := 0;
    UseCount := 1;
    Item := nil;

    if not Player.ActiveBuffs.HasSoulArrow then
    begin
      if Info.Skill > 0 then
      begin
        Skill := SkillDataProv.GetPlayerSkill(Info.Skill);
        UseCount := PSkillLevelInfo(Skill.Effects[Player.GetSkillLevel(Skill)])^.BulletCon;

        // Skills that only use 1 have 0 in the DB
        if UseCount = 0 then
          UseCount := 1;
      end;

      if Player.ActiveBuffs.HasShadowPartner then
        UseCount := UseCount * 2;

      // order will be wrong when using "for Item in Player.Inventory[miUse]" here
      for i := 0 to Player.Inventory[miUse].SlotLimit - 1 do
      begin
        Item := Player.Inventory[miUse].Items[i];
        if Item = nil then
          Continue;

        IType := GetItemType(Item.ID);
        IsClaw := (WType = itClaw) and (IType = itStar) and (Weapon.ID <> MagicalMitten);
        IsBow := (WType = itBow) and (IsArrowForBow(Item.ID));
        IsCBow := (WType = itCrossbow) and (IsArrowForCBow(Item.ID));
        IsGun := (WType = itGun) and (IType = itBullet);
        IsMitten := (Weapon.ID = MagicalMitten) and (IType = itArrow);

        if (IsClaw or IsBow or IsCBow or IsGun or IsMitten) and (Item.Quantity >= UseCount) then
        begin
          Projectile := Item.ID;
          Break;
        end;
      end;

      if Projectile = 0 then
      begin
        Log('Ranged attack failed - no projectile O.o');
        Exit;
      end;

      Player.RemoveItemFromSlot(miUse, Item.Position, UseCount, False, True);
    end;

    Player.Map.BroadcastMessage(Player, RangedAttack(Info, Projectile));

    TAttackParser.ApplyAttack(Info, Player);
  finally
    Info.Free;
  end;
end;

{ TMagicAttackHandler }

class procedure TMagicAttackHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Player: TMapleCharacter;
  Info: TAttackInfo;
  (*Skill: TSkill;
  Effect: PSkillLevelInfo;*)
begin
  Player := C.Player;
  Info := TAttackParser.ParseAttack(Packet, atMagic, Player);
  try
    Player.Map.BroadcastMessage(Player, MagicAttack(Info));

   (* Skill := SkillDataProv.GetPlayerSkill(Info.Skill);
    if Skill = nil then
    begin
      Log('[WARNING] Skill = nil? %d', [Info.Skill]);
      Exit;
    end;
    Effect := Skill.Effects[Player.GetSkillLevel(Skill)];   *)

    // xxx Cooldown

    TAttackParser.ApplyAttack(Info, Player);
  finally
    Info.Free;
  end;
end;

{ TEnergyChargeAttackHandler }

class procedure TEnergyChargeAttackHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Info: TAttackInfo;
begin
  Info := TAttackParser.ParseAttack(Packet, atEnergyCharge, C.Player);
  try
    C.Player.Map.BroadcastMessage(C.Player, EnergyChargeAttack(Info));
    TAttackParser.ApplyAttack(Info, C.Player);
  finally
    Info.Free;
  end;
end;

{ TFaceExpressionHandler }

class procedure TFaceExpressionHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Face: Integer;
  Char: TMapleCharacter;
begin
  Face := Packet.ReadInt;
  if Face > 7 then
  begin
    // xxx Check if player has the item, else [HACK]
  end;

  Char := C.Player;
  Char.Map.BroadcastMessage(Char, FacialExpression(Char.ID, Face));
end;

{ TChangeMapHandler }

class procedure TChangeMapHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  PortalName: string;
  Portal: TMaplePortal;
  TargetID: Integer;
  Player: TMapleCharacter;
  ToMap: TMapleMap;
  DoWarp: Boolean;
begin
  Player := C.Player;

  Packet.ReadByte;  // Type
  TargetID := Packet.ReadInt;
  PortalName := Packet.ReadMapleAnsiString;
  Portal := C.Player.Map.GetPortal(PortalName);

  if (TargetID <> -1) and (Player.HP = 0) then
  begin
    Player.HP := 50;
    Player.Stance := 0;

    DoWarp := True;
    if Player.EventInstance <> nil then
      DoWarp := Player.EventInstance.RevivePlayer(Player.EventChar);

    if not DoWarp then
      Exit;

    if Player.Map.ForcedReturn <> NO_MAP then
      ToMap := C.Channel.MapProvider.LoadMap(Player.Map.ForcedReturn)
    else
      ToMap := C.Channel.MapProvider.LoadMap(Player.Map.ReturnMap);

    if ToMap <> nil then
      Player.ChangeMap(ToMap)
    else
      Log('[FATAL] Map = nil');
  end
  else
  if (TargetID > -1){ and (Player.IsGM)} then   // xxx doesnt work with Aran Tutorial otherwise
  begin
    ToMap := C.Channel.MapProvider.LoadMap(TargetID);
    Player.ChangeMap(ToMap);
  end
  else
  if (TargetID > -1) and (not Player.IsGM) then
    Log('[Hacking] Player %s attempted Mapjumping without being a GM', [Player.Name])
  else
    if Portal = nil then
      C.Write(PortalBlocked)
    else
      Portal.Enter(C);
end;

{ TChangeMapSpecialHandler }

class procedure TChangeMapSpecialHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  sPortal: string;
  Portal: TMaplePortal;
begin
  Packet.Skip(1);
  sPortal := Packet.ReadMapleAnsiString;

  Portal := C.Player.Map.GetPortal(sPortal);
  if Portal <> nil then
    Portal.Enter(C)
  else
  begin
    Log('[MapChangedSpecial] Portal not found on map! ' + sPortal);
    C.Write(PortalBlocked);
  end;
end;

{ TUseChairItemHandler }

class procedure TUseChairItemHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  ItemID: Integer;
  Player: TMapleCharacter;
begin
  ItemID := Packet.ReadInt;
  Player := C.Player;

  if not Player.Inventory[miSetup].ContainsID(ItemID) then
    Exit;  // hacking

  Player.Chair := ItemID;
  Player.Map.BroadcastMessage(Player, ShowChair(Player.ID, ItemID), False);

  C.Write(EnableActions(True));
end;

{ TChairActionHandler }

class procedure TChairActionHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  ID: SmallInt;
  Player: TMapleCharacter;
begin
  ID := Packet.ReadShort;
  Player := C.Player;

  Player.Chair := ID;
  C.Write(ChairAction(ID));

  if ID = -1 then   // Cancel
    Player.Map.BroadcastMessage(Player, ShowChair(Player.ID, 0), False);
end;

{ TUseInnerPortalHandler }

class procedure TUseInnerPortalHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Portal: string;
  ToX, ToY: Integer;
  Player: TMapleCharacter;
begin
  Packet.Skip(1);
  Portal := Packet.ReadMapleAnsiString;
  ToX := Packet.ReadShort;
  ToY := Packet.ReadShort;

  if C.Player.Map.GetPortal(Portal) = nil then
  begin
    C.Disconnect;
    Exit;
  end;

  Player := C.Player;
  Player.Map.MovePlayer(Player, Point(ToX, ToY));
  Player.Map.BroadcastMessage(Player, ShowSpecialEffect(Player.ID, sePortal), False);
end;

{ TChangeChannelHandler }

class procedure TChangeChannelHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Channel: Byte;
  IP: TIPArray;
  Addy: In_Addr;
begin
  Channel := Packet.ReadByte;
  C.UpdateLoginState(lsServerTransition);

  IP := TransformIP(frmSettings.edtCSIP.Text);
  Addy.S_un_b.s_b1 := IP[0];
  Addy.S_un_b.s_b2 := IP[1];
  Addy.S_un_b.s_b3 := IP[2];
  Addy.S_un_b.s_b4 := IP[3];

  C.Write(ChangeChannel(Addy, TChannelServer(C.World.Channels[Channel]).Port));

  C.BeforeReconnect;   // Enter the critical sessions so it doesn't fuck up packets
  C.Disconnect;
end;

initialization
  HandlerClasses.Add('MovePlayer', TMovePlayerHandler);
  HandlerClasses.Add('CloseRangeAttack', TCloseRangeDamageHandler);
  HandlerClasses.Add('RangedAttack', TRangedAttackHandler);
  HandlerClasses.Add('MagicAttack', TMagicAttackHandler);
  HandlerClasses.Add('EnergyChargeAttack', TEnergyChargeAttackHandler);
  HandlerClasses.Add('FaceExpression', TFaceExpressionHandler);
  HandlerClasses.Add('ChangeMap', TChangeMapHandler);
  HandlerClasses.Add('ChangeChannel', TChangeChannelHandler);
  HandlerClasses.Add('MapChanged', TPacketHandler);   // empty
  HandlerClasses.Add('MapChangedSpecial', TChangeMapSpecialHandler);
  HandlerClasses.Add('UseChairItem', TUseChairItemHandler);
  HandlerClasses.Add('ChairAction', TChairActionHandler);
  HandlerClasses.Add('UseInnerPortal', TUseInnerPortalHandler);

finalization

end.
