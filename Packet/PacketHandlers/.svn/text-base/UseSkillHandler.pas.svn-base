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

unit UseSkillHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator, GameLogic,
     Generics.Collections, MTRand, Math, Skills, MapleSummon;

type
  TUseSkillHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TCancelBuffHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

procedure AddItemBuff(Player: TObject; ID, Time: Integer);

implementation

uses Main, Buffs, MapleCharacter, SkillDataProvider, MapleMapObject, MapleMonster,
     AttackParser, MapleParty;

// Redirector for PlayerInventory, can't use a class helper in a class helper
procedure AddItemBuff(Player: TObject; ID, Time: Integer);
begin
  TMapleCharacter(Player).AddBuff(ID, Time);
end;

{ TUseSkillHandler }

function GetAffectedMembers(Party: TMapleParty; Mask: Byte): TList<Integer>;
var
  i: Integer;
begin
  Result := TList<Integer>.Create;
  for i := 5 downto 0 do
  begin
    if (Party.Members.Count > i) and (Mask and 1 > 0) then
      Result.Add(TMaplePartyCharacter(Party.Members[i]).ID);

    Mask := Mask shr 1;
  end;
end;

class procedure TUseSkillHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  SkillID, MobID, i, Heal, Exp, Old: Integer;
  AddedInfo: SmallInt;
  SkillLevel, Count: Byte;
  Player, Member: TMapleCharacter;
  Skill: TSkill;
  SInfo: PSkillLevelInfo;
  Mob: TMapleMapObject;
  Success: Boolean;
  S: TMapleSummon;
  Members: TList<Integer>;
begin
  Packet.Skip(4);  // Some tickcount
  SkillID := Packet.ReadInt;
  AddedInfo := 0;
  SkillLevel := Packet.ReadByte;
  Player := C.Player;
  Skill := SkillDataProv.GetPlayerSkill(SkillID);

  i := Player.GetSkillLevel(Skill);
  if (SkillLevel = 0) or (i <> SkillLevel) then
  begin
    Log('[Hacking] SkillLevel wrong: %d, but should be %d!', [SkillLevel, i]);
    Exit;
  end;
  SInfo := Skill.Effects[i];

  ApplySkillCosts(Player, SkillID, SkillLevel);
  Player.Map.BroadcastMessage(Player, ShowSkill(Player, SkillID, SkillLevel, 1));

  case SkillID of
    ILWizard.Slow,
    FPWizard.Slow,
    BlazeWizard.Slow,
    Page.Threaten,
    Priest.Doom:
    begin
      if (SkillID = ILWizard.Slow) or (SkillID = FPWizard.Slow) or (SkillID = BlazeWizard.Slow) or (SkillID = Page.Threaten) then
        Packet.Skip(4);
      Count := Packet.ReadByte;
      for i := 0 to Count - 1 do
      begin
        MobID := Packet.ReadInt;
        Mob := Player.Map.MapObject[MobID];
        if (Mob <> nil) and (Mob is TMapleMonster) then
          TAttackParser.HandleMobStatus(Player, TMapleMonster(Mob), SkillID);
      end;
    end;

    Cleric.Heal:
    begin
      Heal := Min(100, SInfo.HPRate) * Player.CurrentMaxHP div 100;

      if Player.Party <> nil then
      begin
        Exp := 0;
        Members := GetAffectedMembers(Player.Party, Packet.ReadByte);
        try
          for i in Members do
          begin
            if Player.Map.MapObject[i] = nil then
              Continue;

            Member := TMapleCharacter(Player.Map.MapObject[i]);
            Old := Member.HP;
            if Old > 0 then
            begin
              Member.AddHP(Heal div Members.Count);
              if Member <> Player then
              begin
                TMapleClient(Member.Client).Write(ShowOwnBuffEffect(Member, SkillID, SkillLevel, 2));
                Member.Map.BroadcastMessage(Member, ShowSkill(Member, SkillID, SkillLevel, 2));
                Inc(Exp, 20 * (Member.HP - Old) div (8 * Member.Level + 190));
              end;
            end;
          end;
        finally
          Members.Free;
        end;

        if Exp > 0 then
          Player.GainExp(Exp, True, False, False);
      end
      else
        Player.AddHP(Heal);
    end;

    Fighter.Rage,
    DawnWarrior.Rage,
    Spearman.IronWill,
    Spearman.HyperBody,
    FPWizard.Meditation,
    ILWizard.Meditation,
    BlazeWizard.Meditation,
    Cleric.Bless,
    Assassin.Haste,
    Bandit.Haste,
    NightWalker.Haste:
    begin
      if Player.Party <> nil then
      begin
        Members := GetAffectedMembers(Player.Party, Packet.ReadByte);
        try
          for i in Members do
          begin
            if Player.Map.MapObject[i] = nil then
              Continue;

            Member := TMapleCharacter(Player.Map.MapObject[i]);
            TMapleClient(Member.Client).Write(ShowOwnBuffEffect(Member, SkillID, SkillLevel, 2));
            Member.Map.BroadcastMessage(Member, ShowSkill(Member, SkillID, SkillLevel, 2));
            Member.AddBuff(SkillID, SkillLevel, 0);
          end;
        finally
          Members.Free;
        end;
      end
    end;

    Citizen.Capture:
    begin
      MobID := Packet.ReadInt;
      Mob := Player.Map.MapObject[MobID];
      if (Mob <> nil) and (Mob is TMapleMonster) then
        with TMapleMonster(Mob) do
        begin
          Success := (HP < Stats.HP div 2) and (Rand.RandInt(100) <= SInfo.Prop);
          C.Write(ShowMagnet(MobID, Success));
          C.Write(ShowOwnBuffEffect(Player, SkillID, SkillLevel, 1, Ord(not Success)));
          if (Success) and (ID >= 9304000) and (ID < 9305000) then
          begin
            Player.Jaguar := (ID - 9303999) * 10;
            Player.Map.KillMonster(Mob, Player, False);
            C.Write(UpdateJaguar(Player.Jaguar));
          end;
        end;
      Exit;
    end;

    else if Packet.ReadByte = $80 then
      AddedInfo := Packet.ReadShort;
  end;

  if Player.AddBuff(SkillID, SkillLevel, AddedInfo) then
    Exit;

  if TMapleSummon.IsSummon(SkillID) then
  begin
    S := TMapleSummon.Create(Player, SkillID, SkillLevel, SInfo.Time, Player.Position);
    S.AddHP(SInfo.X);
    Player.AddSummon(S);
    Player.Map.AddMapObject(S);
    Player.Map.BroadcastMessage(S.GetSpawnPacket(True));
  end;
end;

{ TCancelBuffHandler }

class procedure TCancelBuffHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Skill: Integer;
begin
  Skill := Packet.ReadInt;

  // Aran Combo Reset - Thanks to Haiku for informing me about this
  if Skill = Aran.ComboAbility then
  begin
    C.Player.ComboCounter := 0;
    Exit;
  end;

  // Also change TPlayerActiveBuff.StopSkill when changing this!!
  if C.Player.ActiveBuffs.GetActiveSkillLevel(Skill) = 0 then
  begin
    Log('ActiveSkillLevel = 0 !! @%d', [Skill]);
    Exit;
  end;

  C.Player.ActiveBuffs.RemoveBuff(Skill, False);

  // EndBuff() is declared in the class helper "TBuffs"
  C.Player.EndBuff(Skill);
end;

initialization
  HandlerClasses.Add('UseSkill', TUseSkillHandler);
  HandlerClasses.Add('CancelBuff', TCancelBuffHandler);

finalization

end.
