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

unit Buffs;

interface

uses Generics.Collections, MapleCharacter, BuffDataProvider, SkillDataProvider,
     PlayerBuffs, BuffStat;

type
  { Delphi might have disadvantages concerning some things (e.g. namespaces..),
    but this is really cool }
  TBuffs = class helper for TMapleCharacter
  private
    function GetValue(Value: TSkillValue; Skill: PSkillLevelInfo; Level: Byte): Smallint;
    function ParseBuffs(SkillID: Integer; Level: Byte): TList<TBuff>;
    function ParseMountInfo(SkillID: Integer): Integer;

    procedure DoAct(SkillID: Integer; Level: Byte);
  public
    function AddBuff(SkillID: Integer; Level: Byte; AddedInfo: SmallInt): Boolean; overload;
    procedure AddBuff(ItemID, Time: Integer); overload;
    procedure EndBuff(SkillID: Integer);

    function ParseBuffInfo(SkillID: Integer; Level: Byte): PActiveBuff;
    function ParseBuffMapInfo(SkillID: Integer; Level: Byte; Entry: Boolean): PActiveMapBuff;
  end;

implementation

uses MaplePacketCreator, MapleClient, Skills;

{ TBuffs }

function TBuffs.AddBuff(SkillID: Integer; Level: Byte; AddedInfo: SmallInt): Boolean;
var
  Skill: PSkillLevelInfo;
  Time, Mount: Integer;
  Buffs: TList<TBuff>;
  PlayerSkill: PActiveBuff;
  MapSkill, EnterSkill: PActiveMapBuff;
begin
  if not BuffDataProv.IsBuff(SkillID) then
    Exit(False);

  Mount := ParseMountInfo(SkillID);
  Skill := SkillDataProv.GetPlayerSkill(SkillID).Effects[Level];
  Time := Skill^.Time;

  case SkillID of
    WildHunter.JaguarRider:
      if Mount = 0 then  // Hacking
        Exit(True)
      else
        Time := 0;  // Infinite

    Crusader.ComboAttack,
    DawnWarrior.ComboAttack:
      ActiveBuffs.SetCombo(0, False);

    WhiteKnight.FireChargeSword,
    WhiteKnight.FireChargeBW,
    WhiteKnight.IceChargeSword,
    WhiteKnight.IceChargeBW,
    WhiteKnight.LightningChargeSword,
    WhiteKnight.LightningChargeBW,
    ThunderBreaker.LightningCharge:
      ActiveBuffs.Charge := SkillID;
  end;

  Buffs := ParseBuffs(SkillID, Level);
  PlayerSkill := ParseBuffInfo(SkillID, Level);
  MapSkill := ParseBuffMapInfo(SkillID, Level, False);
  EnterSkill := ParseBuffMapInfo(SkillID, Level, True);

  if Mount > 0 then
    MaplePacketCreator.UseMount(TMapleClient(Client), SkillID, PlayerSkill, MapSkill, Mount)
  else
    case SkillID of
      Pirate.Dash,
      ThunderBreaker.Dash:
        UsePirateBuff(TMapleClient(Client), SkillID, Time, PlayerSkill, MapSkill);

      Marauder.EnergyCharge,
      ThunderBreaker.EnergyCharge:
      begin
        if ActiveBuffs.EnergyCharge < 10000 then
          Time := 0;
        UsePirateBuff(TMapleClient(Client), 0, Time, PlayerSkill, MapSkill);
      end;

      else MaplePacketCreator.UseBuff(
         TMapleClient(Client), SkillID, Time, PlayerSkill, MapSkill, AddedInfo);
    end;

  // This class can access MapleCharacter's public properties as if they belonged to this class.
  if (SkillID <> ActiveBuffs.GetEnergyCharge) or (ActiveBuffs.EnergyCharge = 10000) then
  begin
    ActiveBuffs.AddBuffInfo(SkillID, Buffs);
    ActiveBuffs.AddMapEntryBuffInfo(EnterSkill);
    ActiveBuffs.SetActiveSkillLevel(SkillID, Level);
    ActiveBuffs.RemoveBuff(SkillID, False);
    ActiveBuffs.AddBuff(SkillID, Time);
    DoAct(SkillID, Level);
  end;

  Buffs.Free;
  FreeActiveBuff(PlayerSkill);
  FreeActiveMapBuff(MapSkill);
  FreeActiveMapBuff(EnterSkill);

  RecalcLocalStats;

  Result := True;
end;

procedure TBuffs.EndBuff(SkillID: Integer);
var
  Level: Byte;
  Buffs: TList<TBuff>;
  MESkill: PActiveMapBuff;
  ToEnd: TBuffStat;
begin
  case SkillID of
    Crusader.ComboAttack,
    DawnWarrior.ComboAttack:
      ActiveBuffs.SetCombo(0, False);

    Marauder.EnergyCharge,
    ThunderBreaker.EnergyCharge:
      ActiveBuffs.EnergyCharge := 0;

    WhiteKnight.FireChargeSword,
    WhiteKnight.FireChargeBW,
    WhiteKnight.IceChargeSword,
    WhiteKnight.IceChargeBW,
    WhiteKnight.LightningChargeSword,
    WhiteKnight.LightningChargeBW,
    ThunderBreaker.LightningCharge:
      ActiveBuffs.Charge := 0;
  end;

  Level := ActiveBuffs.GetActiveSkillLevel(SkillID);
  Buffs := ParseBuffs(SkillID, Level);
  MESkill := ParseBuffMapInfo(SkillID, Level, True);
  ToEnd := ActiveBuffs.RemoveBuffInfo(SkillID, Buffs);

  MaplePacketCreator.EndBuff(TMapleClient(Client), ToEnd);

  Buffs.Free;

  ActiveBuffs.DeleteMapEntryBuffInfo(MESkill);
  ActiveBuffs.SetActiveSkillLevel(SkillID, 0);

  FreeActiveMapBuff(MESkill);

  RecalcLocalStats;
  EnforceMaxHPMP;
end;

procedure TBuffs.AddBuff(ItemID, Time: Integer);
var
  Buffs: TList<TBuff>;
  PlayerSkill: PActiveBuff;
  MapSkill, EnterSkill: PActiveMapBuff;
begin
  ItemID := -ItemID;   // Make the Item ID negative for the packet and to discern from skill buffs

  Buffs := ParseBuffs(ItemID, 0);
  PlayerSkill := ParseBuffInfo(ItemID, 0);
  MapSkill := ParseBuffMapInfo(ItemID, 0, False);
  EnterSkill := ParseBuffMapInfo(ItemID, 0, True);

  MaplePacketCreator.UseBuff(TMapleClient(Client), ItemID, Time, PlayerSkill, MapSkill, 0);

  ActiveBuffs.RemoveBuff(ItemID, False);
  ActiveBuffs.AddBuffInfo(ItemID, Buffs);
  ActiveBuffs.AddBuff(ItemID, Time);
  ActiveBuffs.AddMapEntryBuffInfo(EnterSkill);
  ActiveBuffs.SetActiveSkillLevel(ItemID, 1);

  Buffs.Free;
  FreeActiveBuff(PlayerSkill);
  FreeActiveMapBuff(MapSkill);
  FreeActiveMapBuff(EnterSkill);
end;

procedure TBuffs.DoAct(SkillID: Integer; Level: Byte);
var
  SkillsInfo: PSkillInfo;
  Value: Smallint;
begin
  SkillsInfo := BuffDataProv.GetSkillInfo(SkillID);

  if SkillsInfo^.BAct then
  begin
    Value := GetValue(SkillsInfo^.Act.Value, SkillDataProv.GetPlayerSkill(SkillID).Effects[Level], Level);
    ActiveBuffs.AddAct(SkillID, SkillsInfo^.Act.Act, Value, SkillsInfo^.Act.Time);
  end;
end;

function TBuffs.GetValue(Value: TSkillValue; Skill: PSkillLevelInfo; Level: Byte): Smallint;
begin
  case Value of
    svX: Result := Skill^.X;
    svY: Result := Skill^.Y;
    svSpeed: Result := Skill^.Speed;
    svJump: Result := Skill^.Jump;
    svWAtk: Result := Skill^.WAtk;
    svWDef: Result := Skill^.WDef;
    svMAtk: Result := Skill^.MAtk;
    svMDef: Result := Skill^.MDef;
    svAcc: Result := Skill^.Acc;
    svAvo: Result := Skill^.Avo;
    svProp: Result := Skill^.Prop;
    svMorph: Result := Skill^.Morph;
    svLv: Result := Level;

    else Result := 0;
  end;
end;

function TBuffs.ParseBuffInfo(SkillID: Integer; Level: Byte): PActiveBuff;
var
  Info: PSkillInfo;
  Cur: TBuffInfo;
  Val: TSkillValue;
  Value: SmallInt;
  Eff: PSkillLevelInfo;
begin
  New(Result);
  Result^ := TActiveBuff.Create(0);
  Info := BuffDataProv.GetSkillInfo(SkillID);
  if SkillID > 0 then
    Eff := SkillDataProv.GetPlayerSkill(SkillID).Effects[Level]
  else
    Eff := nil;

  for Cur in Info^.Player do
  begin
    Result^.HasMapBuff := Result^.HasMapBuff or Cur.HasMapVal;
    if Cur.OnlyMap then
      Continue;

    Result.Stat := Result.Stat or Cur.Buff.Stat;

    Val := Cur.Buff.Value;
    if Val = svNone then
      Value := Cur.ItemVal
    else
      case SkillID of
        Hermit.ShadowPartner,
        ChiefBandit.ShadowPartner,
        NightWalker.ShadowPartner:
          Value := Eff.X * 256 + Eff.Y;

        Crusader.ComboAttack,
        DawnWarrior.ComboAttack:
          Value := ActiveBuffs.Combo + 1;

        Marauder.EnergyCharge,
        ThunderBreaker.EnergyCharge:
          Value := ActiveBuffs.EnergyCharge;

        else
          Value := GetValue(Val, Eff, Level);
      end;

    Result^.Vals.Add(Value);
  end;
end;

function TBuffs.ParseBuffMapInfo(SkillID: Integer; Level: Byte; Entry: Boolean): PActiveMapBuff;
var
  Cur: TBuffInfo;
  Map: TBuffMapInfo;
  Info: PSkillInfo;
  Maps: Integer;
  Val: TSkillValue;
  Value: Smallint;
begin
  New(Result);
  Result^ := TActiveMapBuff.Create(0);
  Maps := 0;
  Info := BuffDataProv.GetSkillInfo(SkillID);

  for Cur in Info^.Player do
  begin
    if ((not Entry) and (not Cur.HasMapVal)) or ((Entry) and (not Cur.HasMapEntry)) then
      Continue;

    Map := Info^.Map[Maps];
    Inc(Maps);
    Val := Map.Buff.Value;

    Result.Stats.Add(Map.Buff.Stat);
    Result.Stat := Result.Stat or Map.Buff.Stat;
    Result.UseVals.Add(Map.UseVal);
    if Map.UseVal then
    begin
      if Val = svNone then
        Value := Cur.ItemVal
      else if (SkillID = Crusader.ComboAttack) or (SkillID = DawnWarrior.ComboAttack) then
        Value := ActiveBuffs.Combo + 1
      else
        Value := GetValue(Val, SkillDataProv.GetPlayerSkill(SkillID).Effects[Level], Level);

      Result^.Values.Add(Value);
    end;
  end;
end;

function TBuffs.ParseBuffs(SkillID: Integer; Level: Byte): TList<TBuff>;
var
  Info: PSkillInfo;
  i: Integer;
begin
  Result := TList<TBuff>.Create;
  Info := BuffDataProv.GetSkillInfo(SkillID);

  for i := 0 to Info^.Player.Count - 1 do
    Result.Add(TBuffInfo(Info^.Player[i]).Buff);
end;

function TBuffs.ParseMountInfo(SkillID: Integer): Integer;
begin
  Result := 0;
  case SkillID of
    WildHunter.JaguarRider:
      case Jaguar of
        10: Result := 1932015;
        20: Result := 1932030;
        30: Result := 1932031;
        40: Result := 1932032;
        50: Result := 1932033;
        60: Result := 1932036;
      end;
  end;
end;

end.
