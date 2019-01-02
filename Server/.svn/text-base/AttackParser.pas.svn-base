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

unit AttackParser;

interface

uses Classes, SysUtils, MapleStream, Generics.Collections, MapleCharacter,
     MapleMap, MapleMonster, SkillDataProvider, GameLogic, MTRand;

type
  TDamagePair = TPair<Integer, TList<Integer>>;

  TAttackType = (atMelee, atRanged, atMagic, atEnergyCharge, atSummon);

  TAttackInfo = class
  public
    AttType: TAttackType;
    NumAttackedAndDamage, TargetCount, HitCount, DamagedTargets: Byte;
    Skill, Charge,FDirection, TotalDamage: Integer;
    SkillLevel, Display, Stance, WeaponClass, WeaponSpeed, UnkV80: Byte;
    AllDamage: TList<TDamagePair>;
    Player: TMapleCharacter;

    constructor Create(APlayer: TMapleCharacter; AType: TAttackType);
    destructor Destroy; override;
  end;

  TAttackParser = class
  public
    class function ParseAttack(Data: TMapleStream; AType: TAttackType; Player: TMapleCharacter): TAttackInfo;
    class procedure ApplyAttack(Att: TAttackInfo; Player: TMapleCharacter);
    class procedure HandleMobStatus(Player: TMapleCharacter; Mob: TMapleMonster; SkillID: Integer);
  end;

implementation

uses Main, Skills, MapleClient, MaplePacketCreator;

{ TAttackInfo }

constructor TAttackInfo.Create(APlayer: TMapleCharacter; AType: TAttackType);
begin
  DamagedTargets := 0;
  SkillLevel := 0;
  TotalDamage := 0;
  AllDamage := TList<TPair<Integer, TList<Integer>>>.Create;
  Player := APlayer;
  AttType := AType;
end;

destructor TAttackInfo.Destroy;
var
  P: TDamagePair;
begin
  for P in AllDamage do
    TList<Integer>(P.Value).Free;

  AllDamage.Free;

  inherited;
end;

{ TAttackParser }

class function TAttackParser.ParseAttack(Data: TMapleStream;
  AType: TAttackType; Player: TMapleCharacter): TAttackInfo;
var
  i, oID, j, Damage: Integer;
  AllDamageNumbers: TList<Integer>;
begin
  Result := TAttackInfo.Create(Player, AType);

  if AType <> atSummon then
  begin
    Data.Skip(1);
    {$IF DEFINED(VERSION88_UP) AND NOT DEFINED(CHAOS)}
    // When hitting a reactor it sends a byte more
    Data.Skip(1);
    i := 0;
    while Data.ReadByte = $FF do
      Inc(i);
    if i < 7 then
      Log('AttackParser failed on ' + Data.ToString);
    Data.Seek(Data.Position - 1, soBeginning);
    {$IFEND}

    Result.NumAttackedAndDamage := Data.ReadByte;
    Result.TargetCount := Result.NumAttackedAndDamage div $10;
    Result.HitCount := Result.NumAttackedAndDamage mod $10;
    {$IFDEF VERSION88_UP}Data.Skip(8);{$ENDIF}
    Result.Skill := Data.ReadInt;

    if Result.Skill > 0 then
      Result.SkillLevel := Player.GetSkillLevel(SkillDataProv.GetPlayerSkill(Result.Skill));

    //Log('[DamageParsing] TargetCount: ' + IntToStr(Result.TargetCount));
    //Log('[DamageParsing] HitCount: ' + IntToStr(Result.HitCount));

    {$IFDEF BIGBANG}
    Data.Skip(1);
    {$ENDIF}

    {$IFNDEF CHAOS}
    Data.Skip(4);
    Data.Skip(4);
    {$IFDEF VERSION88_UP}
    Data.Skip(8);

    if AType = atMagic then
      Data.Skip(24);
    {$ENDIF}
    {$ENDIF}

    {$IFDEF BIGBANG}
    if AType = atRanged then
      Data.Skip(1);
    {$ENDIF}

    case Result.Skill of
      Brawler.CorkscrewBlow,
      Gunslinger.Grenade,
      ThunderBreaker.CorkscrewBlow:
        Result.Charge := Data.ReadInt;
      else Result.Charge := 0;
    end;

    Result.Display := Data.ReadByte;
    Result.UnkV80 := Data.ReadByte;
    //Log('[DamageParsing] UnkV80: %d', [Result.UnkV80]);
    Result.Stance := Data.ReadByte;

    if Result.Skill > 0 then
      ApplySkillCosts(Player, Result.Skill, Player.GetSkillLevel(SkillDataProv.GetPlayerSkill(Result.Skill)));

    {$IFDEF BIGBANG}
    Data.Skip(4);
    {$ENDIF}

    Result.WeaponClass := Data.ReadByte;
    Result.WeaponSpeed := Data.ReadByte;
  end
  else
  begin
    Result.Stance := Data.ReadByte;
    {$IFNDEF VERSION83}
    Data.Skip(8);
    {$ENDIF}
    Result.TargetCount := Data.ReadByte;
    Result.HitCount := 1;
    {$IFNDEF VERSION90}
    Data.Skip(4);
    {$ENDIF}
  end;

  Data.Skip(4);  // Tick count
  {$IFDEF VERSION88_UP}Data.Skip(4);{$ENDIF}

  if AType = atRanged then
  begin
    Data.Skip(2);   // Star slot
    Data.Skip(2);   // Cash star slot
    Data.Skip(1);
  end;

  for i := 0 to Result.TargetCount - 1 do
  begin
    oID := Data.ReadInt;
    Data.Skip(4);  // 06 XX XX 01
    Data.Skip(8);  // Position of mob & damage

    if Result.Skill <> 5221004 then    // Meso explosion
      Data.Skip(2);

    if AType = atSummon then
      Data.Skip(4);

    AllDamageNumbers := TList<Integer>.Create;
    for j := 0 to Result.HitCount - 1 do
    begin
      Damage := Data.ReadInt;
      AllDamageNumbers.Add(Damage);
    end;

    if AType <> atSummon then
      Data.Skip(4);   // maybe mob CRC

    Result.AllDamage.Add(TPair<Integer, TList<Integer>>.Create(oID, AllDamageNumbers));
  end;
end;

class procedure TAttackParser.ApplyAttack(Att: TAttackInfo; Player: TMapleCharacter);
var
  Map: TMapleMap;
  Mob: TMapleMonster;
  Dmg: TDamagePair;
  TotDmgToOneMob, EachDmg, Eater: Integer;
  Dead, Eaten: Boolean;
  MPEater: PSkillLevelInfo;
begin
  Eaten := False;
  Eater := Player.ActiveBuffs.GetMPEater;
  if Eater > 0 then
    MPEater := Player.GetSkillInfo(Eater)
  else
    MPEater := nil;

  Map := Player.Map;

  for Dmg in Att.AllDamage do
  begin
    Mob := TMapleMonster(Map.MapObject[Dmg.Key]);

    if (Mob = nil) or ((Att.Skill = Cleric.Heal) and (not Mob.Stats.Undead)) then
      Continue;

    TotDmgToOneMob := 0;
    for EachDmg in Dmg.Value do
    begin
      Inc(TotDmgToOneMob, EachDmg);

      if Assigned(MPEater) and (not Eaten) and (Att.AttType = atMagic) then
      begin
        Eaten := Mob.MPEat(Player, MPEater);
        if Eaten then
        begin
          TMapleClient(Player.Client).Write(ShowOwnBuffEffect(Player, Eater, MPEater.Level, 1));
          Map.BroadcastMessage(Player, ShowSkill(Player, Eater, MPEater.Level, 1));
        end;
      end;
    end;

    if TotDmgToOneMob > 0 then
      Inc(Att.DamagedTargets);

    // xxx thousands of skills

    Inc(Att.TotalDamage, TotDmgToOneMob);

    Dead := Map.DamageMonster(Player, Mob, TotDmgToOneMob);

    if (not Dead) and (Att.Skill > 0) and (TotDmgToOneMob > 0) then
      HandleMobStatus(Player, Mob, Att.Skill);
  end;
end;

class procedure TAttackParser.HandleMobStatus(Player: TMapleCharacter;
  Mob: TMapleMonster; SkillID: Integer);
var
  Level: Byte;
  Skill: TSkill;
  SInfo: PSkillLevelInfo;
  Statuses: TList<TMobStatus>;
  Success: Boolean;
begin
  Skill := SkillDataProv.GetPlayerSkill(SkillID);
  Level := Player.GetSkillLevel(Skill);
  SInfo := Skill.Effects[Level];
  Statuses := TList<TMobStatus>.Create;
  try
    Success := (SInfo <> nil) and (Rand.RandInt(99) < SInfo.Prop);

    if Mob.Stats.CanFreeze then
    begin
      case SkillID of
        ILWizard.ColdBeam:
          Statuses.Add(TMobStatus.Create(MobStatus.Freeze, 1, SkillID, SInfo.Time * 3));  // not sure about the time
      end;
    end;

    if not Mob.Stats.Boss then
    begin
      case SkillID of
        Brawler.BackspinBlow,
        Brawler.DoubleUppercut:
          Statuses.Add(TMobStatus.Create(MobStatus.Stun, 1, SkillID, SInfo.Time));

        Hunter.ArrowBomb,
        Crusader.ComaSword,
        Crusader.ComaAxe,
        DawnWarrior.Coma,
        Crusader.Shout,
        WhiteKnight.ChargedBlow,
        ChiefBandit.Assaulter,
        Marauder.EnergyBlast,
        Gunslinger.BlankShot:
          if Success then
            Statuses.Add(TMobStatus.Create(MobStatus.Stun, 1, SkillID, SInfo.Time));

        Ranger.SilverHawk,
        Sniper.GoldenEagle,
        WildHunter.SilverHawk:
          if Success then
            Statuses.Add(TMobStatus.Create(MobStatus.Stun, 1, SkillID, SInfo.X));

        FPMage.Seal,
        ILMage.Seal,
        BlazeWizard.Seal:
          if Success then
            Statuses.Add(TMobStatus.Create(MobStatus.Seal, 1, SkillID, SInfo.Time));

        Priest.Doom:
          if Success then
            Statuses.Add(TMobStatus.Create(MobStatus.Doom, 1, SkillID, SInfo.Time));

        {$IFDEF BIGBANG}
        Crusader.PanicSword,
        DawnWarrior.Panic:
          if Success then
            Statuses.Add(TMobStatus.Create(MobStatus.Darkness, SInfo.X, SkillID, SInfo.Time));
        {$ENDIF}
      end;
    end;

    case SkillID of
      FPWizard.Slow,
      ILWizard.Slow,
      BlazeWizard.Slow:
        Statuses.Add(TMobStatus.Create(MobStatus.Speed, SInfo.X, SkillID, SInfo.Time));

      Page.Threaten:
        if Success then
        begin
          Statuses.Add(TMobStatus.Create(MobStatus.WAtk, SInfo.X, SkillID, SInfo.Time));
          Statuses.Add(TMobStatus.Create(MobStatus.WDef, SInfo.Y, SkillID, SInfo.Time));
          //Statuses.Add(TMobStatus.Create(MobStatus.Acc, SInfo.Y, SkillID, SInfo.SubTime));
        end;

      Thief.Disorder,
      NightWalker.Disorder:
      begin
        Statuses.Add(TMobStatus.Create(MobStatus.WAtk, SInfo.X, SkillID, SInfo.Time));
        Statuses.Add(TMobStatus.Create(MobStatus.WDef, SInfo.Y, SkillID, SInfo.Time));
      end;
    end;

    if Statuses.Count > 0 then
      Mob.AddStatus(Statuses);
  finally
    Statuses.Free;
  end;
end;

end.
