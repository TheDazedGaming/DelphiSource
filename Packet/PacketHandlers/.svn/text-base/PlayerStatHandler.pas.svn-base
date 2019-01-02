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

unit PlayerStatHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator, GameLogic,
     Generics.Collections, MTRand, Math, Skills;

type
  TDistributeAPHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TDistributeAPMultiHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TDistributeSPHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  THealOvertimeHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TTakeDamageHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TCharInfoRequestHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TGiveFameHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleCharacter, MapleMonster, SkillDataProvider;

function UpdateStat(Player: TMapleCharacter; Stat: TMapleStat;
  List: TPlayerStatUpdate; Amount: Word = 1): Boolean;
var
  Max: Integer;
  MaxIncrease: TSkill;
begin
  case Stat of
    msStr:
    begin
      if Player.Str >= 999 then
        Exit(False);

      Player.Str := Player.Str + Amount;
      List.Add(TPair<TMapleStat, Integer>.Create(Stat, Player.Str));
    end;

    msDex:
    begin
      if Player.Dex >= 999 then
        Exit(False);

      Player.Dex := Player.Dex + Amount;
      List.Add(TPair<TMapleStat, Integer>.Create(Stat, Player.Dex));
    end;

    msInt:
    begin
      if Player.Int >= 999 then
        Exit(False);

      Player.Int := Player.Int + Amount;
      List.Add(TPair<TMapleStat, Integer>.Create(Stat, Player.Int));
    end;

    msLuk:
    begin
      if Player.Luk >= 999 then
        Exit(False);

      Player.Luk := Player.Luk + Amount;
      List.Add(TPair<TMapleStat, Integer>.Create(Stat, Player.Luk));
    end;

    msMaxHP:
    begin
      Max := Player.MaxHP;
      if (Player.HpApUsed = 10000) or (Max = 30000) then
        Exit(False);

      case GetJobClass(Player.Job, True) of
        mjBeginner: Inc(Max, Byte(bhBeginnerAP));
        mjWarrior:
        begin
          Inc(Max, Byte(bhWarriorAP));

          if Player.HasHPIncrease then
          begin
            MaxIncrease := Player.GetHPIncrease;
            Inc(Max, PSkillLevelInfo(MaxIncrease.Effects[Player.GetSkillLevel(MaxIncrease)])^.Y);
          end;
        end;
        mjMagician: Inc(Max, Byte(bhMagicianAP));
        mjBowman:   Inc(Max, Byte(bhBowmanAP));
        mjThief:    Inc(Max, Byte(bhThiefAP));
        mjPirate:   Inc(Max, Byte(bhPirateAP));  // xxx increase (brawler)
        mjGM:       Inc(Max, Byte(bhGMAP));
        mjAran:     Inc(Max, Byte(bhAranAP));

        else
          Log('[WARNING] Could not determine job class: %d', [Word(Player.Job)]);
      end;

      Inc(Max, Rand.RandInt(Byte(bhVariation)));

      Max := Min(30000, Max);

      Player.HpApUsed := Player.HpApUsed + 1;
      Player.MaxHP := Max;
      List.Add(TPair<TMapleStat, Integer>.Create(Stat, Max));
    end;

    msMaxMP:
    begin
      Max := Player.MaxMP;
      if (Player.MpApUsed = 10000) or (Max = 30000) then
        Exit(False);

      case GetJobClass(Player.Job, True) of
        mjBeginner: Inc(Max, Byte(bmBeginnerAP));
        mjWarrior:  Inc(Max, Byte(bmWarriorAP));
        mjMagician:
        begin
          Inc(Max, Byte(bmMagicianAP));

          if Player.HasMPIncrease then
          begin
            MaxIncrease := Player.GetMPIncrease;
            Inc(Max, 2 * PSkillLevelInfo(MaxIncrease.Effects[Player.GetSkillLevel(MaxIncrease)])^.Y);
          end;
        end;
        mjBowman:   Inc(Max, Byte(bmBowmanAP));
        mjThief:    Inc(Max, Byte(bmThiefAP));
        mjPirate:   Inc(Max, Byte(bmPirateAP));
        mjGM:       Inc(Max, Byte(bmGMAP));
        mjAran:     Inc(Max, Byte(bmAranAP));

        else
          Log('[WARNING] Could not determine job class: %d', [Word(Player.Job)]);
      end;

      Inc(Max, Rand.RandInt(Byte(bmVariation)));

      Max := Min(30000, Max);

      Player.MpApUsed := Player.MpApUsed + 1;
      Player.MaxMP := Max;
      List.Add(TPair<TMapleStat, Integer>.Create(Stat, Max));
    end;

    else   // unknown
    begin
      TMapleClient(Player.Client).Write(StatUpdateOK);
      Exit(False);
    end;
  end;

  Result := True;
end;

{ TDistributeAPHandler }

class procedure TDistributeAPHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Update: Integer;
  Player: TMapleCharacter;
  StatUpdate: TPlayerStatUpdate;
begin
  C.Write(StatUpdateOK);
  Packet.Skip(4);

  Update := Packet.ReadInt;

  StatUpdate := TPlayerStatUpdate.Create;
  try
    Player := C.Player;
    if Player.RemainingAP > 0 then
    begin
      if not UpdateStat(Player, TMapleStat(Update), StatUpdate) then
        Exit;

      Player.RemainingAP := Player.RemainingAP - 1;
      StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msRemainingAP, Player.RemainingAP));
      C.Write(UpdatePlayerStats(StatUpdate, True));
    end
    else
      Log('[Hacking] Player %s is distributing AP to %d without having any!', [Player.Name, Update]);
  finally
    StatUpdate.Free;
  end;
end;

{ TDistributeAPMultiHandler }

class procedure TDistributeAPMultiHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Update, Amount: Integer;
  Player: TMapleCharacter;
  StatUpdate: TPlayerStatUpdate;
begin
  C.Write(StatUpdateOK);
  Packet.Skip(8);

  StatUpdate := TPlayerStatUpdate.Create;
  try
    Player := C.Player;
    if Player.RemainingAP > 0 then
    begin
      while Packet.Position < Packet.Size do
      begin
        Update := Packet.ReadInt;
        Amount := Packet.ReadInt;

        if (Player.RemainingAP < Amount) or
           (not UpdateStat(Player, TMapleStat(Update), StatUpdate, Amount)) then
          Exit;

        Player.RemainingAP := Player.RemainingAP - Amount;
      end;

      StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msRemainingAP, Player.RemainingAP));
      C.Write(UpdatePlayerStats(StatUpdate, True));
    end
    else
      Log('[Hacking] Player %s is distributing AP without having any!', [Player.Name]);
  finally
    StatUpdate.Free;
  end;
end;

{ TDistributeSPHandler }

class procedure TDistributeSPHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  SkillID, RemainingSP: Integer;
  IsBeginnerSkill: Boolean;
  Player: TMapleCharacter;
  Snails, Recovery, NimbleFeet: Integer;
  SnailsLevel, RecoveryLevel, NimbleFeetLevel, MaxLevel, CurLevel: Byte;
  Skill: TSkill;
begin
  Packet.Skip(4);
  SkillID := Packet.ReadInt;

  IsBeginnerSkill := False;
  Player := C.Player;
  if not IsExtendedSPJob(Player.Job) then
    RemainingSP := Player.RemainingSP
  else
    RemainingSP := Player.ExtendedSP[AdvancementOf(TMapleJob(SkillID div 10000))];

  Snails := Skills.GetBeginnerSkill(Player.Job, Beginner.ThreeSnails);
  Recovery := Skills.GetBeginnerSkill(Player.Job, Beginner.Recovery);
  NimbleFeet := Skills.GetBeginnerSkill(Player.Job, Beginner.NimbleFeet);

  if (SkillID = Snails) or (SkillID = Recovery) or (SkillID = NimbleFeet) then
  begin
    SnailsLevel := Player.GetSkillLevel(SkillDataProv.GetPlayerSkill(Snails));
    RecoveryLevel := Player.GetSkillLevel(SkillDataProv.GetPlayerSkill(Recovery));
    NimbleFeetLevel := Player.GetSkillLevel(SkillDataProv.GetPlayerSkill(NimbleFeet));

    if not IsResistance(Player.Job) then
      RemainingSP := Min(Player.Level - 1, 6) - SnailsLevel - RecoveryLevel - NimbleFeetLevel
    else
      RemainingSP := Min(Player.Level - 1, 9) - SnailsLevel - RecoveryLevel - NimbleFeetLevel;

    IsBeginnerSkill := True;
  end;

  Skill := SkillDataProv.GetPlayerSkill(SkillID);
  MaxLevel := Skill.MaxLevel;
  CurLevel := Player.GetSkillLevel(Skill);

  if (RemainingSP > 0) and (CurLevel + 1 <= MaxLevel) and Skill.CanBeLearnedBy(Player.Job) then
  begin
    if not IsBeginnerSkill then
      if not IsExtendedSPJob(Player.Job) then
        Player.RemainingSP := Player.RemainingSP - 1
      else
        Player.ExtendedSP[AdvancementOf(TMapleJob(SkillID div 10000))] := Player.ExtendedSP[AdvancementOf(TMapleJob(SkillID div 10000))] - 1;

    Player.UpdateSingleStat(msRemainingSP, Player.RemainingSP);
    Player.ChangeSkillLevel(Skill, CurLevel + 1);
  end
  else if not Skill.CanBeLearnedBy(Player.Job) then
    Log('[Hacking] Trying to distribute SP on skill %d, which doesn''t match job %d of %s', [SkillID, Word(Player.Job), Player.Name])
  else if not ((RemainingSP > 0) and (CurLevel + 1 <= MaxLevel)) then
    Log('[Hacking] %s is distributing SP to %d without having any', [Player.Name, SkillID]);
end;

{ THealOvertimeHandler }

class procedure THealOvertimeHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  HealHP, HealMP: Word;
begin
  Packet.Skip(8);
  HealHP := Packet.ReadShort;
  if HealHP <> 0 then
  begin
    // xxx Hacking when > 140

    C.Player.AddHP(HealHP);
  end;

  HealMP := Packet.ReadShort;
  if HealMP <> 0 then
  begin
    if HealMP > 300 then
      Log('[Hacking] Player %s is regenerating too many MP: %d (MaxMP: %d)',
                            [C.Player.Name, HealMP,
                             C.Player.MaxMP]);

    C.Player.AddMP(HealMP);
  end;
end;

{ TTakeDamageHandler }

class procedure TTakeDamageHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Player: TMapleCharacter;
  DamageFrom, Direction, Stance: ShortInt;
  Damage, MonsterIDFrom, OID, NoDamageSkill, SkillDmg: Integer;
  Attacker: TMapleMonster;
  Skill: TSkill;
  Reduc: SmallInt;
  MPDamage, HPDamage: Word;
  AppliedDamage: Boolean;
begin
  Player := C.Player;

  Packet.Skip(4);
  DamageFrom := Packet.ReadByte;
  Packet.Skip(1);
  Damage := Packet.ReadInt;

  if DamageFrom <> -2 then
  begin
    MonsterIDFrom := Packet.ReadInt;
    OID := Packet.ReadInt;
    Attacker := TMapleMonster(Player.Map.MapObject[OID]);
    Direction := Packet.ReadByte;
    Stance := Packet.ReadByte;
    // xxx check if player has PowerStance
  end
  else
  begin
    Log('[Damage] From = -2');

    MonsterIDFrom := 0;
    OID := 0;
    Attacker := nil;
    Direction := 0;
    Stance := 0;
  end;

  if (DamageFrom <> -1) and (DamageFrom <> -2) and (Attacker <> nil) then
    Log('[UNSUPPORTED] Special mob attack!');

  if Damage = -1 then
  begin
    Log('[Damage] = -1');
    NoDamageSkill := 4020002 + (Trunc(Integer(Player.Job) / 10 - 40) * 100000)
  end
  else
    NoDamageSkill := 0;

  if (Damage < -1) or (Damage > 60000) then
  begin
    Log('[Hacking] Taking abnormal damage from %d: %d', [MonsterIDFrom, Damage]);
    Exit;
  end;

  AppliedDamage := False;
  if Damage > 0 then  // xxx and (not Player.IsHidden)
  begin
    // xxx handle all kinds of skills
    Skill := Player.GetPowerGuard;
    if (Attacker <> nil) and (not Attacker.Stats^.Boss) and (DamageFrom = -1) and (Skill <> nil) and (Player.ActiveBuffs.IsActive(Skill.ID)) then
    begin
      SkillDmg := Ceil(Damage * (Player.ActiveBuffs.GetActiveSkillInfo(Skill.ID)^.X / 100));
      SkillDmg := Min(SkillDmg, Attacker.Stats^.HP div 2);
      Player.Map.DamageMonster(Player, Attacker, SkillDmg);
      Dec(Damage, SkillDmg);
      Player.Map.BroadcastMessage(Player, DamageMonster(OID, SkillDmg));
    end;

    if Player.HasMagicGuard then
    begin
      Skill := Player.GetMagicGuard;
      if Player.ActiveBuffs.IsActive(Skill.ID) then
      begin
        Reduc := Player.ActiveBuffs.GetActiveSkillInfo(Skill.ID)^.X;
        MPDamage := (Damage * Reduc) div 100;
        HPDamage := Damage - MPDamage;

        if MPDamage < Player.MP then
        begin
          Player.AddMP(-MPDamage);
          Player.AddHP(-HPDamage);
        end
        else
        begin
          Player.AddHP(-(HPDamage + (MPDamage - Player.MP)));
          Player.AddMP(-Player.MP);  // doesn't tell the client when it's just set to 0
        end;

        AppliedDamage := True;
      end;
    end;

    if not AppliedDamage then
      Player.AddHP(-Damage);
  end;

  // xxx if not Player.IsHidden then
  Player.Map.BroadcastMessage(Player,
      DamagePlayer(DamageFrom, Direction, Stance, MonsterIDFrom, Player.ID, Damage, NoDamageSkill));
end;

{ TCharInfoRequestHandler }

class procedure TCharInfoRequestHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  ID: Integer;
  IsSelf: Boolean;
begin
  Packet.Skip(4);
  ID := Packet.ReadInt;
  IsSelf := Packet.ReadByte <> 0;

  C.Write(
    CharInfoResponse(C.Channel.Player[ID], IsSelf));
end;

{ TGiveFameHandler }

class procedure TGiveFameHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Who: Integer;
  Mode, Change: ShortInt;
  Target: TMapleCharacter;
  Status: TFameStatus;
begin
  Who := Packet.ReadInt;
  Mode := Packet.ReadByte;

  if Mode = 0 then
    Change := -1
  else
    Change := 1;

  Target := TMapleCharacter(C.Player.Map.MapObject[Who]);

  if Target = C.Player then
  begin
    Log('[Hacking] Player %s tried to fame himself!', [Target.Name]);
    Exit;
  end
  else
  if C.Player.Level < 15 then
  begin
    Log('[Hacking] Player %s tried to fame, but is under Lv. 15!', [C.Player.Name]);
    Exit;
  end;

  Status := C.Player.CanGiveFame(Target);
  case Status of
    fsOK:
    begin
      Target.ModifyFame(Change);
      C.Player.HasGivenFame(Target);
      C.Write(GiveFameResponse(Mode, Target.Name, Target.Fame));
      Target.UpdateSingleStat(msFame, Target.Fame);
      TMapleClient(Target.Client).Write(ReceiveFame(Mode, C.Player.Name));
    end;

    else
      C.Write(GiveFameErrorResponse(Byte(Status) + 3));
  end;
end;

initialization
  HandlerClasses.Add('DistributeAP', TDistributeAPHandler);
  HandlerClasses.Add('DistributeAPMulti', TDistributeAPMultiHandler);
  HandlerClasses.Add('DistributeSP', TDistributeSPHandler);
  HandlerClasses.Add('HealOvertime', THealOvertimeHandler);
  HandlerClasses.Add('TakeDamage', TTakeDamageHandler);
  HandlerClasses.Add('CharInfoRequest', TCharInfoRequestHandler);
  HandlerClasses.Add('GiveFame', TGiveFameHandler);

finalization

end.
